;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "darkhandz"
      user-mail-address "darkhandz0@gmail.com")

;; (setq doom-font (font-spec :family "Fira Code Retina" :size 14))
(setq doom-font (font-spec :family "Maple Mono NF CN" :size 18 :weight 'semi-bold))
;; (setq doom-variable-pitch-font (font-spec :family "Noto Serif CJK SC" :size 16))
(setq doom-symbol-font (font-spec :family "Maple Mono NF CN"))

;; 为中文设置额外字体
(if (eq system-type 'gnu/linux)
(when (display-graphic-p)
  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font t charset (font-spec :family "Maple Mono NF CN")))))

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
(setq display-line-numbers-type t)
(remove-hook! '(text-mode-hook) #'display-line-numbers-mode)
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


;; (remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)
;; (add-hook! '+doom-dashboard-mode-hook (hl-line-mode -1))
;; (setq-hook! '+doom-dashboard-mode-hook evil-normal-state-cursor (list nil))
(setq fancy-splash-image (expand-file-name "assets/doom-emacs-gray.svg" doom-user-dir))


;; ----------------------------- frame title --------------------------------
(setq frame-title-format
  '(""
    (:eval
        (replace-regexp-in-string "/home/[^/]+" "🏠 ~" (or buffer-file-name "")))
        ;; (or buffer-file-name ""))
      ;; (if (s-contains-p org-roam-directory (or buffer-file-name ""))
      ;;     (replace-regexp-in-string
      ;;      ".*/[0-9]*-?" "☰ "
      ;;      (subst-char-in-string ?_ ?  buffer-file-name))
      ;;   "%b"))
    (:eval
      (let ((project-name (or projectile-project-name (buffer-name) "❓")))
      (unless (string= "-" project-name)
      (format (if (buffer-modified-p)  " ⋮ ◯ %s ◯" " ⋮ ● %s ●") project-name))))))

;; for windows, when minimize window
(setq icon-title-format frame-title-format)


;; ----------------------------- emacs >= 28.2 will use italic --------------------------------
;; (set-face-attribute 'line-number nil :slant 'normal)
;; (set-face-attribute 'line-number-current-line nil :slant 'normal)
(use-package! display-line-numbers
  :custom-face
  (line-number ((t (:slant normal))))
  (line-number-current-line ((t (:slant normal)))))

;; ----------------------------- mouse scroll --------------------------------
;; scroll one line at a time (less “jumpy” than defaults)
;; (unless (modulep! :ui smooth-scroll)
  (setq mouse-wheel-scroll-amount '(3 ((shift) . 8))) ;; 3 lines / 8 columns at a time,
  (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
  (setq mouse-wheel-follow-mouse t) ;; scroll window under mouse
  (setq mouse-wheel-tilt-scroll t) ;; Enable horizontal scrolling with the second mouse wheel or the touchpad
;;   ;; (setq scroll-step 1) ;; keyboard scroll 5 lines at a time
;;   )

;; ----------------------------- mouse button --------------------------------
;; Map extra mouse buttons to jump between buffers
(map! :after better-jumper
      :nv [mouse-8] #'better-jumper-jump-backward
      :nv [mouse-9] #'better-jumper-jump-forward)

;; -------------------------------- comment -----------------------------------
;; comment mode, positive: block, negative: line
(add-hook 'c-mode-hook (lambda () (c-toggle-comment-style -1)))


;; ----------------------------- input method----------------------------------
(setq default-input-method "rime"
      rime-show-candidate 'posframe)
(setq rime-user-data-dir "~/.config/fcitx/rime")
(setq rime-posframe-properties
    (list :font "Maple Mono NF CN"
       :background-color "#333333"
       :foreground-color "#dcdccc"
       :internal-border-width 10))

;; -------------------------------- ivy --------------------------------------
;; avy, 2 char motion
(map! :leader
      :desc "evil-avy-goto-char-2"
      "j j" #'evil-avy-goto-char-2)

(after! ivy
  ; avoid rg hang on windows
  (setq consult-async-input-debounce 0.3)
  (setq counsel-async-command-delay 0.2)
  ; preview buffer when selected
  ;; (setq +ivy-buffer-preview t)
  ; to avoid one char triggered counsel-rg
  (setq ivy-more-chars-alist '((counsel-rg . 2)
                               (counsel-search . 2)
                               (t . 3))))

;; -------------------------------- corfu ------------------------------------
; don't show current select item as visual text on input area
(after! corfu
  (setq corfu-preview-current nil))

;; ----------------------------- projectile ----------------------------------
; don't add projects automatically
(setq projectile-track-known-projects-automatically nil)

;; use projectile-project-name as workspace name if available
(defun dk-workspace-name-fun (project-root)
  "Return the name of the project.
If `projectile-project-name` is non-nil, return its value.
Otherwise, use `projectile-default-project-name`."
  (or
    ;; load `.dir-locals.el` from project-root
    (with-temp-buffer
      (setq default-directory project-root)
      (hack-dir-local-variables-non-file-buffer)
      (when (boundp 'projectile-project-name)
        projectile-project-name))
    (projectile-default-project-name project-root)))
(setq projectile-project-name-function 'dk-workspace-name-fun)

;; -------------------------------- avy --------------------------------------
;; make avy works across all visible windows
(setq avy-all-windows t)
(setq avy-keys-alist
  `((avy-goto-char . (?a ?s ?d ?f ?g ?q ?w ?e ?r ?u ?i ?o ?p ?c ?v ?n ?m ?h ?j ?k ?l))
    (avy-goto-char-in-line . (?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?u ?i ?o ?p))))

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
;; (add-hook 'prog-mode-hook (lambda () (whitespace4r-mode 1)))

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
  (setq lsp-headerline-breadcrumb-enable t
    lsp-headerline-breadcrumb-enable-diagnostics nil
    ;; hide unreachable ifdefs
    ;; lsp-semantic-tokens-enable t
    lsp-ui-sideline-show-hover t
    ;; disable lsp code lense (SPC c l T l)
    lsp-lens-enable nil)
  (setq lsp-file-watch-threshold 5000))

(defun decrease-color-value (color valueR valueG valueB)
  "Decrease each RGB component of COLOR by VALUE."
  (let* ((r (string-to-number (substring color 1 3) 16))
         (g (string-to-number (substring color 3 5) 16))
         (b (string-to-number (substring color 5 7) 16))
         (new-r (max 0 (- r valueR)))
         (new-g (max 0 (- g valueG)))
         (new-b (max 0 (- b valueB))))
    (format "#%02x%02x%02x" new-r new-g new-b)))

;; lsp-mode breadcrum background
(after! lsp-mode
  (if lsp-headerline-breadcrumb-enable
    (if (equal (car custom-enabled-themes) 'doom-one-light)
      (progn
        (custom-set-faces!
        `(header-line :background ,(decrease-color-value (doom-color 'bg) 10 8 18)))))))

;; resolve Unknown key: :docs-link
(after! lsp-mode
  ;; https://github.com/emacs-lsp/lsp-mode/issues/3577#issuecomment-1709232622
  (delete 'lsp-terraform lsp-client-packages))

;; ccls
(after! ccls
  (setq ccls-initialization-options
        '(:index (:comments 2)
          :completion (:detailedLabel t)))
  ;; optional as ccls is the default in Doom, if you want to use clangd, let the priority smaller than clangd
  (set-lsp-priority! 'ccls 2))

;; clangd
;; (after! lsp-clangd
;;   (setq lsp-clients-clangd-args
;;         '("--background-index"
;;           "--clang-tidy"
;;           "--completion-style=detailed"
;;           "--header-insertion=never"
;;           "--header-insertion-decorators=0"))
;;   (set-lsp-priority! 'clangd 6))

(after! lsp-pyright
  (setq lsp-pyright-langserver-command "basedpyright-langserver"))

;; ------------------------------------------ winnum --------------------------------------
;; winnum
(use-package! winum
  :init
  (setq-default winum-scope 'frame-local)
  (setq winum-auto-setup-mode-line nil)
  (winum-mode)
  :config)

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

;; ----------------------------------------------------------------------------
;; magit show detail datetime instead of relative time
(after! magit
  (setq magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18)))


;; --------------------------------------------------------------------------------
;; SPC l / SPC k for buffer navigation
(map!
 (:leader
  :desc "Next buffer" "l" #'next-buffer
  :desc "Previous buffer" "k" #'previous-buffer))

;; SPC [ / ] for version control hunk navigation
(map!
 (:leader
  :desc "vc next hunk" "]" #'+vc-gutter/next-hunk
  :desc "vc next hunk" "\\" #'+vc-gutter/next-hunk ; especially for my redox keyboard
  :desc "vc prev hunk" "[" #'+vc-gutter/previous-hunk))

;; SPC v for imenu
(map!
  (:leader
   :desc "imenu" "e" #'counsel-imenu))

;; --------------------------------------------------------------------------------
;; set the best font height for different screen resolution: 2K - 100, 4K - 180
(map! :leader
      :desc "set font size to adapt 4K"
      "j 4" #'(lambda () (interactive) (set-face-attribute 'default nil :height 180)))
(map! :leader
      :desc "set font size medium"
      "j 2" #'(lambda () (interactive) (set-face-attribute 'default nil :height 110)))
(map! :leader
      :desc "set font size smaller"
      "j 1" #'(lambda () (interactive) (set-face-attribute 'default nil :height 95)))

;; Hide ^M
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(add-hook 'text-mode-hook 'remove-dos-eol)
(add-hook 'c-mode-common-hook 'remove-dos-eol)



;; -----------------------------------------------------------------------------------------
;; avy is more efficient than evil-snipe-s/S
(after! (evil avy)
  (evil-define-key '(normal visual) evil-snipe-local-mode-map
    (kbd "S") 'evil-avy-goto-char-in-line
    (kbd "s") 'evil-avy-goto-char)
  (define-key evil-visual-state-map (kbd "S") 'evil-avy-goto-char-in-line)
  (define-key evil-normal-state-map (kbd "s") 'evil-avy-goto-char))


;; ----------------------------------- symbol overlay --------------------------------------
(use-package! symbol-overlay
  :config
  (setq symbol-overlay-inhibit-map t) ; remove default keymap to avoid conflict with evil
  (define-key evil-normal-state-map (kbd "#") 'symbol-overlay-put) ; more useful
  (map!
   (:leader
    (:prefix ("d" . "symbol")
    :desc "count"           "c"   #'symbol-overlay-count
    :desc "remove all"      "r"   #'symbol-overlay-remove-all
    :desc "backward"        "h"   #'symbol-overlay-switch-backward
    :desc "forward"         "l"   #'symbol-overlay-switch-forward
    :desc "next"            "n"   #'symbol-overlay-jump-next
    :desc "previous"        "p"   #'symbol-overlay-jump-prev
    :desc "rename"          "R"   #'symbol-overlay-query-replace))))


;; remap p/c/s without yank
(after! evil
  ;; C-h/C-d to delete char on insert state
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (define-key evil-insert-state-map (kbd "C-d") 'evil-delete-char)
  (evil-define-key '(normal visual insert) 'global (kbd "C-s") 'save-buffer)
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
  (define-key evil-normal-state-map (kbd "\\") '+default/search-buffer)
  (define-key evil-visual-state-map (kbd "\\") '+default/search-buffer)

  (modify-syntax-entry ?_ "w"))

;; ------ jk to escape -------
(after! evil-escape
  (setq evil-escape-key-sequence "jk"))

;; -----------------------------------------------------------------------------------------
;; If set to nil or t it will fully disable or fully enable highlighting in every tree sitter enabled language respectively.
(setq +tree-sitter-hl-enabled-modes t)

;; ------------------------------------- awesome-tray --------------------------------------
(defun dark/awesome-tray-module-line-char-count-info ()
  (let* ((total-lines (count-lines (point-min) (point-max)))
        (start (region-beginning)) (end (region-end))
        (selected-lines (max 1 (count-lines start end)))
        (chars (1+ (- end start))))
    (if (region-active-p)
        (format "%dC/%dL" chars selected-lines)
      ;; (format "%dL" total-lines) ;; if you want to see total lines when no selection
      )))

(defface dark/awesome-tray-module-line-char-count-face
  '((((background light)) :inherit awesome-tray-blue-bright-face)
    (t :inherit awesome-tray-blue-bright-face))
  "line and char count face."
  :group 'awesome-tray)


(use-package! awesome-tray
  :init
  (awesome-tray-mode 1)
  :config
  (add-hook 'after-change-major-mode-hook #'hide-mode-line-mode)
  ;; (global-hide-mode-line-mode)
  (add-to-list 'awesome-tray-module-alist
    '("line-char" . (dark/awesome-tray-module-line-char-count-info
                     dark/awesome-tray-module-line-char-count-face)))
  (setq awesome-tray-active-modules '("anzu" "line-char" "location" "belong" "file-path" "git" "mode-name" "date"))
  (setq awesome-tray-date-format "%m-%d %H:%M")
  (setq awesome-tray-git-format "[%s]")
  (setq awesome-tray-belong-update-duration 1)
  (setq awesome-tray-location-info-bottom " ↓")
  (setq awesome-tray-location-info-top " ↑")
  )

;; -----------------------------------------------------------------------------------------
;; refer: https://emacs-china.org/t/topic/25992/9
;; Pulse current line
(use-package! pulse
  :ensure nil
  :custom-face
  (pulse-highlight-start-face ((t (:inherit region :background unspecified))))
  (pulse-highlight-face ((t (:inherit region :background unspecified :extend t))))
  :hook (((dumb-jump-after-jump imenu-after-jump) . my-recenter-and-pulse)
         ((bookmark-after-jump magit-diff-visit-file next-error) . my-recenter-and-pulse-line))
  :init
  (with-no-warnings
    (defun my-pulse-momentary-line (&rest _)
      "Pulse the current line."
      (pulse-momentary-highlight-one-line (point)))

    (defun my-pulse-momentary (&rest _)
      "Pulse the region or the current line."
      (if (fboundp 'xref-pulse-momentarily)
          (xref-pulse-momentarily)
        (my-pulse-momentary-line)))

    (defun my-recenter-and-pulse(&rest _)
      "Recenter and pulse the region or the current line."
      (recenter)
      (my-pulse-momentary))

    (defun my-recenter-and-pulse-line (&rest _)
      "Recenter and pulse the current line."
      (recenter)
      (my-pulse-momentary-line))

    (dolist (cmd '(recenter-top-bottom
                   other-window switch-to-buffer
                   aw-select toggle-window-split
                   windmove-do-window-select
                   pager-page-down pager-page-up
                   treemacs-select-window
                   symbol-overlay-basic-jump))
      (advice-add cmd :after #'my-pulse-momentary-line))

    (dolist (cmd '(pop-to-mark-command
                   pop-global-mark
                   better-jumper-jump-forward
                   better-jumper-jump-backward
                   goto-last-change))
      (advice-add cmd :after #'my-recenter-and-pulse))))

;; ------------------------------ vterm for make compile -------------------------------
(defun get-term-buffer-name()
  (let* (
         (use-eshell (modulep! :term eshell))
         (use-vterm (modulep! :term vterm))
         (buffer-name
           (format (if use-eshell "*doom:eshell-popup:%s*" "*doom:vterm-popup:%s*")
                  (if (bound-and-true-p persp-mode)
                    (safe-persp-name (get-current-persp))
                    "main")))
         (buffer (get-buffer buffer-name))
         (window (get-buffer-window buffer-name)))
    (unless (and (buffer-live-p buffer) (window-live-p window))
      (if use-eshell
          (+eshell/toggle nil)
        (if use-vterm
          (+vterm/toggle nil)
          (user-error "Neither eshell nor vterm is enabled"))))
    buffer-name))

(defun detect-os-by-term-prompt ()
  "在指定的终端buffer中检查最后一行非空字符，并根据最后一个非空白字符是'$'还是'>'返回'linux'或'win'，否则返回'unknown'。"
  (let ((buffer-name (get-term-buffer-name)))
    (with-current-buffer buffer-name
      (save-excursion
        (goto-char (point-max)) ;; 移动到buffer的末尾
        (when (re-search-backward "[^ \t\n]" nil t)
          (let ((char-at-point (char-after)))
            (cond
             ((eq char-at-point ?$) 'linux)
             ((eq char-at-point ?>) 'win)
             (t 'unknown))))))))


(defun dark/term-run-cmd (arg) "Open term window and run cmd"
  (interactive "sCommand: ")

  (let ((buffer-name (get-term-buffer-name))
        (use-eshell (modulep! :term eshell))
        (use-vterm (modulep! :term vterm)))
    (with-current-buffer buffer-name
      (let ((inhibit-read-only t))
        (cond
          (use-vterm
            (vterm-send-string arg)
            (vterm-send-return))
          (use-eshell
            (+eshell-run-command arg buffer))
          (t
            (user-error "Neither eshell nor vterm is enabled")))
      ))))


(defun dark/vterm-toggle() "Hide vterm window"
    (interactive)
    (+vterm/toggle nil))


;; load .dark.el from project root
(defun dk-load-cfg-file ()
  (let* ((project-root (or (doom-project-root) default-directory))
         ;; (setenv "PROOT" project-root)
         (dk-cfg-path (expand-file-name ".dark.el" project-root)))
    (if (file-exists-p dk-cfg-path)
      (let ((inhibit-message t))
        (load-file dk-cfg-path)))))

(defun dk-load-cfg-after-workspace-created (&rest _args)
  (run-with-timer 2 nil #'dk-load-cfg-file))

;; load once after new workspace created
(advice-add '+workspace-new :after #'dk-load-cfg-after-workspace-created)


;; Define a helper function to load config and run command
(defun dark/run-cmd-with-cfg (cmd-var)
  "Load the configuration file and execute the specified command.
CMD-VAR is a symbol representing the variable holding the command string."
  (interactive)
  (dk-load-cfg-file)
  (if dk-run-at-remote-win
    (let ((os (detect-os-by-term-prompt)))
      (cond
       ((eq os 'linux)
        (dark/term-run-cmd dk-login-remote))
       ((eq os 'win)
        ;; 在这里添加针对Windows的具体命令
        (dark/term-run-cmd (symbol-value cmd-var)))
       (t
        ;; 在这里添加通用或未知情况下的命令, \x03是Ctrl-C
        (dark/term-run-cmd "\x03"))))
    (dark/term-run-cmd (symbol-value cmd-var))))


;; Define key-to-command mappings (use symbols for command variables)
(defconst dark/key-cmd-alist
  '(("<f1>" . dk-f1-cmd)
    ("<f2>" . dk-f2-cmd)
    ("<f3>" . dk-f3-cmd)
    ("<f4>" . dk-f4-cmd)
    ("<f5>" . dk-f5-cmd)
    ("<f7>" . dk-f7-cmd)
    ("<f8>" . dk-f8-cmd)
    ("<f9>" . dk-f9-cmd)
    ("<f10>" . dk-f10-cmd)
    ("<f12>" . dk-f12-cmd))
  "Mapping of F1-F12 keys to their respective command variables.")

;; Define a function to set keybindings for a given keymap
(defun dark/set-keybindings (keymap)
  "Set keybindings for the specified KEYMAP."
  (dolist (key-cmd dark/key-cmd-alist)
    (let ((key (car key-cmd))
          (cmd-var (cdr key-cmd)))  ;; cmd-var is a symbol
      (define-key keymap (kbd key)
        (lambda () (interactive)
          (dark/run-cmd-with-cfg cmd-var)))))  ;; Pass the symbol, not the value
  (define-key keymap (kbd "<f6>") #'dark/vterm-toggle))

;; Set global keybindings
(dark/set-keybindings (current-global-map))

;; Set vterm-mode keybindings
(after! vterm
  (dark/set-keybindings vterm-mode-map))



;; disable <M-num> in vterm (M-num is use for switch workspace)
(after! vterm
  (map! :map vterm-mode-map :ni "M-1" #'+workspace/switch-to-0)
  (map! :map vterm-mode-map :ni "M-2" #'+workspace/switch-to-1)
  (map! :map vterm-mode-map :ni "M-3" #'+workspace/switch-to-2)
  (map! :map vterm-mode-map :ni "M-4" #'+workspace/switch-to-3)
  (map! :map vterm-mode-map :ni "M-5" #'+workspace/switch-to-4)
  (map! :map vterm-mode-map :ni "M-6" #'+workspace/switch-to-5)
  (map! :map vterm-mode-map :ni "M-7" #'+workspace/switch-to-6)
  (map! :map vterm-mode-map :ni "M-8" #'+workspace/switch-to-7)
  (map! :map vterm-mode-map :ni "M-9" #'+workspace/switch-to-8))


;; -----------------------------------------------------------------------------------------
;; invalidate filename cache after .ignore changed
(defun update-project-files-by-ignore ()
  (when (and t (buffer-file-name)
    (equal ".ignore"
    (file-name-nondirectory (buffer-file-name))))
    (projectile-invalidate-cache nil)))

(add-hook 'after-save-hook 'update-project-files-by-ignore)



;; -----------------------------------------------------------------------------------------
(add-hook 'emacs-lisp-mode-hook
  (defun enable-autoreload-for-dir-locals ()
  (when (and (buffer-file-name)
        (equal dir-locals-file
          (file-name-nondirectory (buffer-file-name))))
  (add-hook 'after-save-hook 'project-reload-dir-locals nil t))))

(defun project-reload-dir-locals (proj)
  "Read values from the current project's .dir-locals file and
apply them in all project file buffers as if opening those files
for the first time.

Signals an error if there is no current project."
  (interactive (list (project-current)))
  (unless proj
    (user-error "There doesn't seem to be a project here"))
  ;; Load the variables; they are stored buffer-locally, so...
  (hack-dir-local-variables)
  ;; Hold onto them...
  (let ((locals dir-local-variables-alist))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (equal proj (project-current))
                   buffer-file-name)
          ;; transfer the loaded values to this buffer...
          (setq-local dir-local-variables-alist locals)
          ;; and apply them.
          (hack-local-variables-apply))))))


;; -----------------------------------------------------------------------------------------
(defun syncthing-trigger-on-save ()
  "Send a curl request after a file is saved, if `syncthing-folder-id` is set."
  (let ((folder-id (bound-and-true-p syncthing-folder-id))
        ; The REST API key can be generated in the GUI (http://localhost:8384)
        ; Linux: export SYNCTHING_API_KEY="xxxx"
        ; Windows: System - Advance System Setting - Environment variable - User variable - New - Variable: SYNCTHING_API_KEY Value: xxxx
        ; and don't forget to run `doom sync` to update env after env set
        (api-key (getenv "SYNCTHING_API_KEY"))
        (server (getenv "SYNCTHING_SERVER")))
    (when (and folder-id api-key)
      (start-process
       "curl-process" nil
       "curl" "-X" "POST" "-H" (concat "X-API-Key: " api-key)
       (concat server "/rest/db/scan?folder=" folder-id)))
    (let ((inhibit-message t))
      (message (format "syncthing: %s %s %s" server folder-id api-key)))
    ))

(add-hook 'after-save-hook 'syncthing-trigger-on-save)


(defun dk-syncthing-after-magit (&rest _args)
  (syncthing-trigger-on-save))

(after! magit
  (advice-add 'magit-file-checkout :after #'dk-syncthing-after-magit)
  (advice-add 'magit-file-delete :after #'dk-syncthing-after-magit)
  (advice-add 'magit-file-rename :after #'dk-syncthing-after-magit)
  (advice-add 'magit-branch-reset :after #'dk-syncthing-after-magit)
  (advice-add 'magit-checkout :after #'dk-syncthing-after-magit)
  (advice-add 'magit-merge-plain :after #'dk-syncthing-after-magit)
  (advice-add 'magit-reset-internal :after #'dk-syncthing-after-magit)
  (advice-add 'magit-stash--apply :after #'dk-syncthing-after-magit)
  (advice-add 'git-rebase-merge :after #'dk-syncthing-after-magit)
  (advice-add 'magit-discard-apply :after #'dk-syncthing-after-magit)
  (advice-add 'magit-discard-files :after #'dk-syncthing-after-magit)
  (advice-add 'magit-reverse-apply :after #'dk-syncthing-after-magit)
  (advice-add 'magit-reverse-files :after #'dk-syncthing-after-magit)
  (advice-add 'magit-apply-patch :after #'dk-syncthing-after-magit)
)

;; -----------------------------------------------------------------------------------------
;; when we in WSL2 and want to open the file in windows real path
(defun dk-open-file-in-explorer ()
  "Open the folder of the current file in Explorer. If running on Linux, check if dk-proj-wsl-root and dk-proj-win-root are defined.
   If defined, perform replacement; if not, use wslpath to convert the WSL path to a Windows path. For non-Linux systems, directly adjust the slashes."
  (interactive)
  (let ((file (or buffer-file-name default-directory)))
    (when file
      (setq file (expand-file-name file))  ;; 确保路径是绝对路径
      ;; 判断当前操作系统是否为 Linux (通常用于 WSL 环境下)
      (if (eq system-type 'gnu/linux)
          (if (and (boundp 'dk-proj-wsl-root) (boundp 'dk-proj-win-root))
              ;; 如果 dk-proj-wsl-root 和 dk-proj-win-root 已定义，进行路径替换
              (setq file (replace-regexp-in-string
                          (regexp-quote dk-proj-wsl-root) dk-proj-win-root file))
            ;; 否则使用 wslpath 将路径转换为 Windows 路径
            (setq file (shell-command-to-string (concat "wslpath -w " (shell-quote-argument file))))))
      ;; 将路径中的 / 转换为 Windows 风格的 \ 分隔符（适用于 Windows 或其他平台）
      (setq file (replace-regexp-in-string "/" "\\\\" file))
      ;; 打开资源管理器，选择文件
      (start-process "explorer" nil "explorer.exe" (concat "/select," file)))))

;; when we in Linux, files are all in local
(defun dk-open-file-manager-and-select ()
  "Open the default file manager and select the file.
  If no file is associated with the current buffer,
  prompt the user to select a file."
  (interactive)
  (let ((file (or (buffer-file-name)
                  (read-file-name "Select file: "))))
    (cond
     ;; Windows
     ((eq system-type 'windows-nt)
      (call-process-shell-command (format "explorer /select,%s" (replace-regexp-in-string "/" "\\\\" file)) nil 0))
     ;; macOS
     ((eq system-type 'darwin)
      (call-process-shell-command (format "open -R %s" (shell-quote-argument file)) nil 0))
     ;; Linux (检测桌面环境)
     ((eq system-type 'gnu/linux)
      (let ((fm (cond
                 ((executable-find "dolphin") "dolphin --select")
                 ((executable-find "nautilus") "nautilus --no-desktop --browser")
                 ((executable-find "thunar") "thunar")
                 (t nil))))
        (if fm
            (call-process-shell-command (format "%s %s" fm (shell-quote-argument file)) nil 0)
          (message "No known file manager found!")))))
    (message "Opened file manager for: %s" file)))

(defun dk-open-in-file-manager ()
  (interactive)
  (cond
   ((eq system-type 'windows-nt) (dk-open-file-in-explorer))
   ((eq system-type 'darwin) (dk-open-file-manager-and-select))
   ((eq system-type 'gnu/linux) (dk-open-file-manager-and-select))
   (t (message "Unsupported OS"))))

(map! :leader
      :desc "Browse in file manager"
      "o o" #'dk-open-in-file-manager)
