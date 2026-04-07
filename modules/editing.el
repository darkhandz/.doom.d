;;; modules/editing.el -*- lexical-binding: t; -*-

;; Comment style, positive: block, negative: line.
(add-hook 'c-mode-hook (lambda () (c-toggle-comment-style -1)))
(add-hook 'c-ts-mode-hook (lambda () (c-ts-mode-toggle-comment-style -1)))

(map! :gi "C-'" #'comment-line
      :n "C-'" #'evilnc-comment-or-uncomment-lines
      :v "C-'" #'evilnc-comment-operator)

;; C tree-sitter defaults. Project-local settings can still override these.
(setq c-ts-mode-indent-offset 4)

(add-hook 'c-ts-mode-hook
          (lambda ()
            (setq-local tab-width 4)
            (setq-local indent-tabs-mode nil)))

;; Disable Doom's non-default indentation highlight. It is noisy in this setup.
(advice-add #'doom-highlight-non-default-indentation-h :override #'ignore)

;; Treat `_` as part of a word in the languages used most often.
(add-hook 'c-ts-mode-hook (lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'c-mode-common-hook (lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'python-mode-hook (lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'js2-mode-hook (lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'ld-script-mode-hook (lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'emacs-lisp-mode-hook (lambda () (modify-syntax-entry ?_ "w")))

(defun dk-remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(add-hook 'text-mode-hook #'dk-remove-dos-eol)
(add-hook 'c-mode-common-hook #'dk-remove-dos-eol)
(add-hook 'c-ts-mode-hook #'dk-remove-dos-eol)

(defconst dk-undo-fu-session-max-file-size (* 512 1024)
  "Maximum file size, in bytes, for persistent undo session recovery.")

(defun dk-undo-fu-session-ignore-large-files-p (filepath)
  "Return non-nil when FILEPATH is too large for `undo-fu-session'."
  (when-let* ((attrs (file-attributes filepath 'integer))
              (size (file-attribute-size attrs)))
    (> size dk-undo-fu-session-max-file-size)))

(after! undo-fu-session
  (add-to-list 'undo-fu-session-incompatible-files
               #'dk-undo-fu-session-ignore-large-files-p))

(map! :map prog-mode-map
      :gni "TAB" #'indent-for-tab-command
      :gni "<tab>" #'indent-for-tab-command)

(use-package! symbol-overlay
  :config
  (setq symbol-overlay-inhibit-map t)
  (define-key evil-normal-state-map (kbd "#") #'symbol-overlay-put)
  (map!
   (:leader
    (:prefix ("d" . "symbol")
     :desc "count" "c" #'symbol-overlay-count
     :desc "remove all" "r" #'symbol-overlay-remove-all
     :desc "backward" "h" #'symbol-overlay-switch-backward
     :desc "forward" "l" #'symbol-overlay-switch-forward
     :desc "next" "n" #'symbol-overlay-jump-next
     :desc "previous" "p" #'symbol-overlay-jump-prev
     :desc "rename" "R" #'symbol-overlay-query-replace))))

(after! evil
  (define-key evil-motion-state-map "j" #'evil-next-visual-line)
  (define-key evil-motion-state-map "k" #'evil-previous-visual-line)
  (define-key evil-insert-state-map (kbd "C-h") #'evil-delete-backward-char-and-join)
  (define-key evil-insert-state-map (kbd "C-d") #'evil-delete-char)
  (evil-define-key '(normal visual insert) 'global (kbd "C-s") #'save-buffer)

  (defun dk-delete-selection-and-paste ()
    (interactive)
    (delete-region (region-beginning) (region-end))
    (yank))

  (evil-define-operator dk-evil-change-line-no-yank (beg end type register yank-handler)
    "Change to end of line without yanking."
    :motion evil-end-of-line
    (interactive "<R><x><y>")
    (evil-change beg end type ?_ yank-handler #'evil-delete-line))

  (evil-define-operator dk-evil-change-no-yank (beg end type register yank-handler)
    "Change without yanking."
    (evil-change beg end type ?_ yank-handler))

  (evil-define-operator dk-evil-change-whole-line-no-yank (beg end type register yank-handler)
    :motion evil-line
    (interactive "<R><x>")
    (evil-change beg end type ?_ yank-handler #'evil-delete-whole-line))

  (define-key evil-visual-state-map (kbd "p") #'dk-delete-selection-and-paste)
  (define-key evil-normal-state-map (kbd "C") #'dk-evil-change-line-no-yank)
  (define-key evil-normal-state-map (kbd "c") #'dk-evil-change-no-yank)
  (define-key evil-visual-state-map (kbd "c") #'dk-evil-change-no-yank)
  (define-key evil-visual-state-map (kbd "S") #'dk-evil-change-whole-line-no-yank)
  (define-key evil-normal-state-map (kbd "\\") #'+default/search-buffer)
  (define-key evil-visual-state-map (kbd "\\") #'+default/search-buffer)
  (modify-syntax-entry ?_ "w"))

(after! evil-escape
  (setq evil-escape-key-sequence "jk"))
