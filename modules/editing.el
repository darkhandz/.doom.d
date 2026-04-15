;;; modules/editing.el -*- lexical-binding: t; -*-

;; Comment style, positive: block, negative: line.
(add-hook 'c-mode-hook (lambda () (c-toggle-comment-style -1)))
(add-hook 'c-ts-mode-hook (lambda () (c-ts-mode-toggle-comment-style -1)))

(map! :gi "C-'" #'comment-line
      :n "C-'" #'evilnc-comment-or-uncomment-lines
      :v "C-'" #'evilnc-comment-operator)

;; C tree-sitter defaults. Project-local settings can still override these.
(setq c-ts-mode-indent-offset 4)

(defvar c-ts-mode-emacs-sources-support)
(defvar so-long-action)
(defvar so-long-variable-overrides)

;; This Emacs/tree-sitter combination rejects predicate-based C queries during
;; `treesit-query-capture'. Keep `c-ts-mode', but replace the affected
;; font-lock features with predicate-free equivalents.
(setq-default c-ts-mode-emacs-sources-support nil)

(with-eval-after-load 'c-ts-mode
  (defun dk-c-ts-safe-comment-settings (mode)
    "Return predicate-free comment font-lock rules for MODE."
    (treesit-font-lock-rules
     :language mode
     :feature 'comment
     '((comment) @font-lock-comment-face)))

  (defun dk-c-ts-safe-definition-settings (mode)
    "Return predicate-free definition font-lock rules for MODE."
    (treesit-font-lock-rules
     :language mode
     :feature 'definition
     `(,@(when (eq mode 'cpp)
           '((destructor_name (identifier) @font-lock-function-name-face)))
       (declaration
        declarator: (_) @c-ts-mode--fontify-declarator)
       (field_declaration
        declarator: (_) @c-ts-mode--fontify-declarator)
       (function_definition
        declarator: (_) @c-ts-mode--fontify-declarator)
       (parameter_declaration
        declarator: (_) @c-ts-mode--fontify-declarator)
       (enumerator
        name: (identifier) @font-lock-property-name-face))))

  (defun dk-c-ts-sanitize-font-lock-settings-a (orig-fn mode)
    "Replace `c-ts-mode' font-lock rules that crash in current runtime."
    (let ((settings (funcall orig-fn mode)))
      (cl-loop for setting in settings
               for feature = (nth 2 setting)
               append
               (pcase feature
                 ('comment (dk-c-ts-safe-comment-settings mode))
                 ('definition (dk-c-ts-safe-definition-settings mode))
                 ;; Only needed for editing Emacs sources, and it uses #match.
                 ('emacs-devel nil)
                 (_ (list setting))))))

  (advice-add #'c-ts-mode--font-lock-settings
              :around #'dk-c-ts-sanitize-font-lock-settings-a))

;; Emacs 30.2 rejects the builtin font-lock query shipped in `lua-ts-mode'
;; on this runtime. Replace just that feature with a Lisp-side matcher.
(with-eval-after-load 'lua-ts-mode
  (defun dk-lua-ts-fontify-builtin (node override start end &rest _)
    "Fontify NODE as a Lua builtin when it matches `lua-ts--builtins'."
    (let ((node-start (treesit-node-start node))
          (node-end (treesit-node-end node)))
      (when (and (>= node-start start)
                 (<= node-end end)
                 (member (treesit-node-text node t) lua-ts--builtins))
        (treesit-fontify-with-override
         node-start node-end font-lock-builtin-face override))))

  (defun dk-lua-ts-safe-builtin-settings ()
    "Return predicate-free builtin font-lock rules for `lua-ts-mode'."
    (treesit-font-lock-rules
     :language 'lua
     :feature 'builtin
     '((identifier) @dk-lua-ts-fontify-builtin)))

  (setq lua-ts--font-lock-settings
        (cl-loop for setting in lua-ts--font-lock-settings
                 for feature = (nth 2 setting)
                 append (if (eq feature 'builtin)
                            (dk-lua-ts-safe-builtin-settings)
                          (list setting)))))

(defun dk-c-so-long-keep-major-mode ()
  "Keep C-family major modes when `so-long' handles long lines."
  (setq-local so-long-action 'so-long-minor-mode)
  ;; Keep long-line protection, but avoid making source buffers read-only.
  (when (boundp 'so-long-variable-overrides)
    (setq-local so-long-variable-overrides
                (assq-delete-all 'buffer-read-only so-long-variable-overrides))))

(add-hook 'c-mode-common-hook #'dk-c-so-long-keep-major-mode)
(add-hook 'c-ts-mode-hook #'dk-c-so-long-keep-major-mode)
(add-hook 'c++-ts-mode-hook #'dk-c-so-long-keep-major-mode)

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
