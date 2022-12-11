;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "darkhandz"
      user-mail-address "darkhandz0@gmail.com")

;; (setq doom-font (font-spec :family "Fira Code Retina" :size 14))
(setq doom-font (font-spec :family "Maple Mono SC NF" :size 17 :weight 'bold))
(setq doom-unicode-font (font-spec :family "Maple Mono SC NF"))

(setq confirm-kill-emacs t)
;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one-light)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)
;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


(add-hook 'c-mode-hook (lambda () (c-toggle-comment-style -1)))


;; ----------------------------- input method----------------------------------
(setq default-input-method "rime"
      rime-show-candidate 'posframe)
(setq rime-user-data-dir "~/.doom.d/rime")
(setq rime-posframe-properties
    (list :font "WenQuanYi Micro Hei Mono-14"
       :background-color "#333333"
       :foreground-color "#dcdccc"
       :internal-border-width 10))
;; ------------------------------ vim sneak -----------------------------------
;; avy, 2 char motion
(map! :leader
      :desc "evil-avy-goto-char-2"
      "j j" #'evil-avy-goto-char-2)
;; make avy works across all visible windows
(setq avy-all-windows t)


;; ------------------------ whitespace-mode has bugs --------------------------
;; disbale whitespace-mode
(advice-add #'doom-highlight-non-default-indentation-h :override #'ignore)

;; ------------------------------ whitespace4r --------------------------------
(progn
  (setq show-trailing-whitespace nil)
  (setq whitespace4r-style '(tabs hspaces zwspaces trailing))
  (setq whitespace4r-display-mappings `((space-mark      . [?·])
                                        (hard-space-mark . [?¤])
                                        (zero-width-space-mark . [?┆])
                                        (tab-mark        . [?- ?⟶]))))
(add-hook 'prog-mode-hook (lambda () (whitespace4r-mode 1)))

;; ------------------------ make _ as part of word ----------------------------
;; For c
(add-hook 'c-mode-common-hook
          (lambda () (modify-syntax-entry ?_ "w")))
;; For python
(add-hook 'python-mode-hook
          (lambda () (modify-syntax-entry ?_ "w")))
;; For Javascript
(add-hook 'js2-mode-hook
          (lambda () (modify-syntax-entry ?_ "w")))

;; ----------------------------------------------------------------------------

;; lsp
(after! lsp-mode
  ;; enable breadcrumb and disable spell check
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-headerline-breadcrumb-enable-diagnostics nil)
  ;; disable lsp code lense (SPC c l T l)
  (setq lsp-lens-enable nil)
  (setq lsp-ui-doc-show-with-cursor nil))

;; ccls
(after! ccls
  (setq ccls-initialization-options '(:index (:comments 2) :completion (:detailedLabel t)))
  ;; optional as ccls is the default in Doom, if you want to use clangd, let the priority smaller than clangd
  (set-lsp-priority! 'ccls 2))

;; ------------------------------------------ winnum --------------------------------------
;; winnum
(use-package! winum
  :init
  (setq-default winum-scope 'frame-local)
  :config)
(winum-mode)

;; Make Treemacs accessible as Window #0
(after! (treemacs winum)
    (setq winum-ignored-buffers-regexp
          (delete (regexp-quote (format "%sFramebuffer-" treemacs--buffer-name-prefix))
                  winum-ignored-buffers-regexp)))

;; SPC n to switch to winum-numbered window n
(map!
 (:leader
    :desc "Switch to window 0" :n "0" #'treemacs-select-window
    :desc "Switch to window 1" :n "1" #'winum-select-window-1
    :desc "Switch to window 2" :n "2" #'winum-select-window-2
    :desc "Switch to window 3" :n "3" #'winum-select-window-3
    :desc "Switch to window 4" :n "4" #'winum-select-window-4
    :desc "Switch to window 5" :n "5" #'winum-select-window-5
    :desc "Switch to window 6" :n "6" #'winum-select-window-6
    :desc "Switch to window 7" :n "7" #'winum-select-window-7
    :desc "Switch to window 8" :n "8" #'winum-select-window-8
    :desc "Switch to window 9" :n "9" #'winum-select-window-9))

;; --------------------------------------------------------------------------------
;; buffer switch
(map!
 (:leader
  :desc "Next buffer" "l" #'next-buffer
  :desc "Previous buffer" "k" #'previous-buffer
  ))


;; consult
;; (use-package! consult
;; :config
;; (consult-customize
;;    consult-ripgrep consult-git-grep consult-grep
;;    consult-bookmark consult-recent-file
;;    +default/search-project +default/search-other-project
;;    +default/search-project-for-symbol-at-point
;;    +default/search-cwd +default/search-other-cwd
;;    +default/search-notes-for-symbol-at-point
;;    +default/search-emacsd
;;    consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
;;    :preview-key '(:debounce 0.5 any)))


;; set the best font height for different screen resolution: 2K - 100, 4K - 180
(map! :leader
      :desc "set font size to adapt 4K"
      "j 4" #'(lambda () (interactive) (set-face-attribute 'default nil :height 180)))
(map! :leader
      :desc "set font size to adapt 2K"
      "j 2" #'(lambda () (interactive) (set-face-attribute 'default nil :height 100)))

;; Hide ^M
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(add-hook 'text-mode-hook 'remove-dos-eol)
(add-hook 'c-mode-common-hook 'remove-dos-eol)


;; -----------------------------------------------------------------------------------------
;; remap p/c/s without yank
(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  ;; avy is more efficient than evil-snipe-s/S
  (evil-define-key '(normal visual) evil-snipe-local-mode-map
    (kbd "s") 'evil-avy-goto-char-in-line
    (kbd "S") 'evil-avy-goto-char)
  (define-key evil-visual-state-map (kbd "S") 'evil-avy-goto-char-in-line)
  (define-key evil-normal-state-map (kbd "S") 'evil-avy-goto-char)
  ;; C-h/C-d to delete char on insert state
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (define-key evil-insert-state-map (kbd "C-d") 'evil-delete-char)
  (defun delete-selection-and-paste ()
    (interactive)
    (delete-region (region-beginning) (region-end))
    (yank))
  (evil-define-operator evil-change-line-no-yank (beg end type register yank-handler)
    "Change to end of line without yanking."
    :motion evil-end-of-line
    (interactive "<R><x><y>")
    (evil-change beg end type ?_ yank-handler #'evil-delete-line))
  (evil-define-operator evil-change-no-yank (beg end type register yank-handler)
    "Change without yanking."
    (evil-change beg end type ?_ yank-handler))
  (evil-define-operator evil-change-whole-line-no-yank (beg end type register yank-handler)
    :motion evil-line
    (interactive "<R><x>")
    (evil-change beg end type ?_ yank-handler #'evil-delete-whole-line))

  (define-key evil-visual-state-map (kbd "p") 'delete-selection-and-paste)
  (define-key evil-normal-state-map (kbd "C") 'evil-change-line-no-yank)
  (define-key evil-normal-state-map (kbd "c") 'evil-change-no-yank)
  (define-key evil-visual-state-map (kbd "c") 'evil-change-no-yank)
  (define-key evil-visual-state-map (kbd "S") 'evil-change-whole-line-no-yank)
  (modify-syntax-entry ?_ "w"))

;; -----------------------------------------------------------------------------------------
(use-package citre
  :defer t
  :init
  ;; This is needed in `:init' block for lazy load to work.
  (require 'citre-config)
  ;; Bind your frequently used commands.  Alternatively, you can define them
  ;; in `citre-mode-map' so you can only use them when `citre-mode' is enabled.
  (global-set-key (kbd "C-c j") 'citre-jump)
  (global-set-key (kbd "C-c h") 'citre-jump-back)
  (global-set-key (kbd "<f1>") 'citre-peek)
  (global-set-key (kbd "C-c c") 'citre-peek)
  (global-set-key (kbd "C-c a") 'citre-ace-peek)
  (global-set-key (kbd "C-c u") 'citre-update-this-tags-file)
  :config
  (setq citre-edit-cmd-buf-default-cmd "ctags\n-o\n%TAGSFILE%\n;; programming languages to be scanned, or \"all\" for all supported languages\n--languages=all\n--kinds-all=*\n--fields=*\n--extras=*\n--exclude=**/.ccls-cache/*\n-R\n;; add dirs/files to scan here, one line per dir/file\n")
  (setq
   ;; Set these if readtags/ctags is not in your PATH.
   ;; citre-readtags-program "/path/to/readtags"
   ;; citre-ctags-program "/path/to/ctags"
   ;; Set these if gtags/global is not in your PATH (and you want to use the
   ;; global backend)
   ;; citre-gtags-program "/path/to/gtags"
   ;; citre-global-program "/path/to/global"
   ;; Set this if you use project management plugin like projectile.  It's
   ;; used for things like displaying paths relatively, see its docstring.
   citre-project-root-function #'projectile-project-root
   ;; Set this if you want to always use one location to create a tags file.
   ;; citre-default-create-tags-file-location 'global-cache
   ;; See the "Create tags file" section above to know these options
   citre-use-project-root-when-creating-tags t
   citre-prompt-language-for-ctags-command t
   ;; By default, when you open any file, and a tags file can be found for it,
   ;; `citre-mode' is automatically enabled.  If you only want this to work for
   ;; certain modes (like `prog-mode'), set it like this.
   citre-auto-enable-citre-mode-modes '(prog-mode)))






;; -----------------------------------------------------------------------------------------
