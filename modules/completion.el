;;; modules/completion.el -*- lexical-binding: t; -*-

(after! ivy
  ;; Avoid `rg` hanging on Windows.
  (setq consult-async-input-debounce 0.3
        counsel-async-command-delay 0.2
        ivy-more-chars-alist '((counsel-rg . 2)
                               (counsel-search . 2)
                               (t . 3)))

  ;; Use the current candidate list for woccur after `ivy-restrict-to-matches`.
  (defun dk/ivy--occur-from-cands (cands)
    "Insert grep-like CANDS into an ivy-occur-grep buffer."
    (require 'counsel)
    (require 'swiper)
    (ivy-occur-grep-mode)
    (setq default-directory (or (ivy-state-directory ivy-last) default-directory))
    (swiper--occur-insert-lines
     (mapcar #'counsel--normalize-grep-match cands)))

  (defun dk/ivy--woccur-use-cands-a (orig-fn &rest args)
    "Use current candidates for woccur when collection is no longer dynamic."
    (let* ((caller (ivy-state-caller ivy-last))
           (grep-callers '(counsel-rg counsel-ag counsel-pt counsel-ack
                            counsel-grep counsel-git-grep))
           (use-cands (and (window-minibuffer-p)
                           (not (ivy-state-dynamic-collection ivy-last))
                           (memq caller grep-callers)
                           ivy--old-cands)))
      (if (not use-cands)
          (apply orig-fn args)
        (require 'wgrep)
        (let ((buffer (generate-new-buffer
                       (format "*ivy-occur%s \"%s\"*"
                               (if caller (concat " " (prin1-to-string caller)) "")
                               ivy-text))))
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (erase-buffer)
              (dk/ivy--occur-from-cands ivy--old-cands))
            (setf (ivy-state-text ivy-last) ivy-text)
            (setq ivy-occur-last ivy-last)
            (setq-local ivy--directory ivy--directory))
          (ivy-exit-with-action
           `(lambda (_)
              (pop-to-buffer ,buffer)
              (ivy-wgrep-change-to-wgrep-mode)))))))

  (advice-add #'+ivy/woccur :around #'dk/ivy--woccur-use-cands-a)

  ;; Ivy already updates history itself. Avoid saving the selected candidate
  ;; into minibuffer history for search-like callers.
  (defvar dk/ivy-no-minibuffer-history-callers
    '(counsel-projectile-find-file counsel-projectile-find-file-dwim
      counsel-file-jump counsel-fzf counsel-locate counsel-search
      ivy-xref-show-xrefs dumb-jump-ivy-jump-to-selected
      swiper swiper-isearch swiper-all swiper-multi swiper-query-replace)
    "Ivy callers that should not push the selected candidate into history.")

  (defun dk/ivy--read-no-minibuffer-history-a (orig-fn prompt collection &rest args)
    "Disable `read-from-minibuffer' history insertion for search-like callers."
    (let* ((caller (plist-get args :caller))
           (grep-like
            (and (boundp 'ivy-highlight-grep-commands)
                 (memq caller ivy-highlight-grep-commands)))
           (extra (memq caller dk/ivy-no-minibuffer-history-callers)))
      (if (or grep-like extra)
          (let ((history-add-new-input nil))
            (apply orig-fn prompt collection args))
        (apply orig-fn prompt collection args))))

  (advice-add #'ivy-read :around #'dk/ivy--read-no-minibuffer-history-a)

  (defun dk/ivy--update-history-a (orig-fn hist)
    "Only record non-empty input for search-like callers."
    (let* ((caller (ivy-state-caller ivy-last))
           (grep-like
            (and (boundp 'ivy-highlight-grep-commands)
                 (memq caller ivy-highlight-grep-commands)))
           (extra (memq caller dk/ivy-no-minibuffer-history-callers)))
      (if (or grep-like extra)
          (unless (eq hist t)
            (when (and (stringp ivy-text) (> (length ivy-text) 0))
              (let ((history-delete-duplicates t))
                (add-to-history
                 hist (propertize ivy-text 'ivy-index ivy--index)))))
        (funcall orig-fn hist))))

  (advice-add #'ivy--update-history :around #'dk/ivy--update-history-a))

(after! ivy-rich
  (plist-put! ivy-rich-display-transformers-list
              'ivy-switch-buffer
              '(:columns
                ((ivy-switch-buffer-transformer (:width 0.45))
                 (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                 (ivy-rich-switch-buffer-path
                  (:width (lambda (x)
                            (ivy-rich-switch-buffer-shorten-path
                             x (ivy-rich-minibuffer-width 0.3))))))
                :delimiter "  ")))

(after! corfu
  (setq tab-always-indent t))
