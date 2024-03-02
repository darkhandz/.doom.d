;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "darkhandz"
      user-mail-address "darkhandz0@gmail.com")

;; (setq doom-font (font-spec :family "Fira Code Retina" :size 14))
(setq doom-font (font-spec :family "Maple Mono SC NF" :size 18 :weight 'bold))
(setq doom-unicode-font (font-spec :family "Maple Mono SC NF"))

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
      (buffer-file-name))
      ;; (if (s-contains-p org-roam-directory (or buffer-file-name ""))
      ;;     (replace-regexp-in-string
      ;;      ".*/[0-9]*-?" "☰ "
      ;;      (subst-char-in-string ?_ ?  buffer-file-name))
      ;;   "%b"))
    (:eval
      (let ((project-name (projectile-project-name)))
      (unless (string= "-" project-name)
      (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") project-name))))))




;; ----------------------------- emacs >= 28.2 will use italic --------------------------------
;; (set-face-attribute 'line-number nil :slant 'normal)
;; (set-face-attribute 'line-number-current-line nil :slant 'normal)
(use-package! display-line-numbers
  :custom-face
  (line-number ((t (:slant normal))))
  (line-number-current-line ((t (:slant normal)))))

;; ----------------------------- mouse scroll --------------------------------
;; scroll one line at a time (less “jumpy” than defaults)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 8))) ;; 3 lines / 8 columns at a time,
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse t) ;; scroll window under mouse
(setq mouse-wheel-tilt-scroll t) ;; Enable horizontal scrolling with the second mouse wheel or the touchpad
;; (setq scroll-step 1) ;; keyboard scroll 5 lines at a time

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
    (list :font "Maple Mono SC NF"
       :background-color "#333333"
       :foreground-color "#dcdccc"
       :internal-border-width 10))

;; -------------------------------- ivy --------------------------------------
;; avy, 2 char motion
(map! :leader
      :desc "evil-avy-goto-char-2"
      "j j" #'evil-avy-goto-char-2)

;; make avy works across all visible windows
;; (after! ivy-avy
;;   (setq avy-all-windows t))

(after! ivy
  ; preview buffer when selected
  ;; (setq +ivy-buffer-preview t)
  ; to avoid one char triggered counsel-rg
  (setq ivy-more-chars-alist '((counsel-rg . 2)
                               (counsel-search . 2)
                               (t . 3))))

;; -------------------------------- avy --------------------------------------
(setq avy-all-windows t)
(setq avy-keys-alist
      `((avy-goto-char . ,(number-sequence ?a ?z))
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
    lsp-lens-enable nil
    lsp-ui-doc-show-with-cursor nil))

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
;; SPC l / SPC k for buffer switch
(map!
 (:leader
  :desc "Next buffer" "l" #'next-buffer
  :desc "Previous buffer" "k" #'previous-buffer))

;; --------------------------------------------------------------------------------
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
;; avy is more efficient than evil-snipe-s/S
(after! (evil avy)
  (evil-define-key '(normal visual) evil-snipe-local-mode-map
    (kbd "S") 'evil-avy-goto-char-in-line
    (kbd "s") 'evil-avy-goto-char)
  (define-key evil-visual-state-map (kbd "S") 'evil-avy-goto-char-in-line)
  (define-key evil-normal-state-map (kbd "s") 'evil-avy-goto-char))

;; remap p/c/s without yank
(after! evil
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
  (define-key evil-normal-state-map (kbd "\\") '+default/search-buffer)
  (define-key evil-visual-state-map (kbd "\\") '+default/search-buffer)

  (modify-syntax-entry ?_ "w"))


(defun jester/cycle-line-beginning-end ()
  "Go to line text beginning, line end, line very beginning, in turn."
  (interactive)
  (cl-block 'my-return
    (when (and (looking-at "[^\s]") (looking-back "^\s*")) (evil-end-of-line) (cl-return-from 'my-return)) ; at beg of line text
    (when (looking-at (if evil-move-beyond-eol "$" ".$")) (evil-beginning-of-line) (cl-return-from 'my-return)) ; at end of line
    (when (bolp) (evil-first-non-blank) (cl-return-from 'my-return)) ; at very beg of line
    (evil-first-non-blank)))

;; use '0' to jump to first-char, first-column, last-char
;; (after! evil
;;   (evil-define-key '(normal visual) 'global (kbd "0") #'jester/cycle-line-beginning-end)
;;   (define-key evil-visual-state-map (kbd "0") #'jester/cycle-line-beginning-end)
;;   (define-key evil-normal-state-map (kbd "0") #'jester/cycle-line-beginning-end))

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
                   goto-last-change))
      (advice-add cmd :after #'my-recenter-and-pulse))))


;; -----------------------------------------------------------------------------------------
;;
;;
(defun upd-compile-json()
  "update compile_commands.json"
  (interactive)
  (progn
    (shell-command-to-string (concat "cd $(git rev-parse --show-toplevel) && "
                                      "cd $(fd 'Makefile' | head -n1 | xargs -n1 dirname ) && "
                                      "if [ -f vs_search_board_cfg.py ]; then python vs_search_board_cfg.py; fi; "
                                      "make OS=X BUILD_DIR=objs-linux -Bnwk > compile.txt && python gen_compile_json.py && rm -f compile.txt;"))
    (lsp-restart-workspace)))

;; ------------------------------ vterm for make compile -------------------------------
(defun dark/vterm-run-cmd (arg) "open vterm window and run cmd"
       (interactive)
       (let* (
              (buffer-name
               (format "*doom:vterm-popup:%s*"
                       (if (bound-and-true-p persp-mode)
                           (safe-persp-name (get-current-persp))
                         "main")))

              (buffer (get-buffer buffer-name))
              (window (get-buffer-window buffer-name)))
         (unless (and (buffer-live-p buffer) (window-live-p window))
           (+vterm/toggle nil))
         (with-current-buffer buffer-name
           (let ((inhibit-read-only t))
             (vterm-send-string arg)))))

(defun dark/project-build() "switch board and make -j"
    (interactive)
    ;; (dark/vterm-run-cmd "make -j\n"))
    (dark/vterm-run-cmd (concat "cd $(git rev-parse --show-toplevel) && " (projectile-compilation-command (projectile-compilation-dir)) "\n")))
(defun dark/project-clean() "make clean"
    (interactive)
    (dark/vterm-run-cmd "make clean\n"))
(defun dark/project-rebuild() "clean build"
    (interactive)
    (dark/vterm-run-cmd (concat "cd $(git rev-parse --show-toplevel) && " (projectile-test-command (projectile-compilation-dir)) "\n")))
(defun dark/hide-vterm() "Hide vterm window"
    (interactive)
    (+vterm/toggle nil))

(global-set-key (kbd "<f7>") #'dark/project-build)
(global-set-key (kbd "<f6>") #'dark/hide-vterm)
(global-set-key (kbd "<f10>") #'dark/project-clean)
(global-set-key (kbd "<f8>")  #'dark/project-rebuild)
(after! vterm
  (map! :map vterm-mode-map :ni "<f7>" #'dark/project-build)
  (map! :map vterm-mode-map :ni "<f6>" #'dark/hide-vterm)
  (map! :map vterm-mode-map :ni "<f10>" #'dark/project-clean)
  (map! :map vterm-mode-map :ni "<f8>"  #'dark/project-rebuild))

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
