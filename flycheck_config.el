;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

(setq doom-font (font-spec :family "Fira Code Retina" :size 14))
(setq confirm-kill-emacs nil)
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
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
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

;; avy, any char
(map! :leader
      :desc "evil-avy-goto-char-timer"
      "j j" #'evil-avy-goto-char-timer)

(add-hook 'c-mode-hook (lambda () (c-toggle-comment-style -1)))


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


;; ccls
(after! ccls
  (setq ccls-initialization-options '(:index (:comments 2) :completion (:detailedLabel t)))
  (set-lsp-priority! 'ccls 2)) ; optional as ccls is the default in Doom

;; temp fix, lookup/references can't jump to selected item
(add-hook! lsp-mode
  (defalias '+lookup/references 'lsp-find-references))

;; ------------------------------------------ winnum --------------------------------------
;; winnum
(use-package! winum
  :init
  (setq-default
   winum-scope 'frame-local)
  :config
  )
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

;; -----------------------------------------------------------------------------------------
