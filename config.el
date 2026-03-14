;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "darkhandz"
      user-mail-address "darkhandz0@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; (setq doom-font (font-spec :family "Fira Code Retina" :size 14))
(setq doom-font (font-spec :family "Maple Mono NF CN" :size 18 :weight 'semi-bold))
;; (setq doom-variable-pitch-font (font-spec :family "Noto Serif CJK SC" :size 16))
(setq doom-symbol-font (font-spec :family "Maple Mono NF CN"))


;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one-light)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; ----------------------- frame geometry persistence ------------------------
;; Restore last frame size/position on startup, save on exit.
(defvar my/frame-geometry-file (expand-file-name "frame-geometry.el" doom-cache-dir))

(defun my/save-frame-geometry ()
  "Persist current GUI frame position/size."
  (when (display-graphic-p)
    (with-temp-file my/frame-geometry-file
      (let ((frame (selected-frame)))
        (prin1
         `((top . ,(frame-parameter frame 'top))
           (left . ,(frame-parameter frame 'left))
           (width . ,(frame-parameter frame 'width))
           (height . ,(frame-parameter frame 'height))
           (fullscreen . ,(frame-parameter frame 'fullscreen)))
         (current-buffer))))))

(defun my/restore-frame-geometry ()
  "Restore GUI frame position/size from last session."
  (when (and (display-graphic-p)
             (file-exists-p my/frame-geometry-file))
    (with-temp-buffer
      (insert-file-contents my/frame-geometry-file)
      (let ((params (read (current-buffer))))
        (modify-frame-parameters (selected-frame) params)))))

(add-hook 'kill-emacs-hook #'my/save-frame-geometry)
(add-hook 'window-setup-hook #'my/restore-frame-geometry)


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
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
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(setq fancy-splash-image (expand-file-name "assets/doom-emacs-gray.svg" doom-user-dir))

;; ----------------------------- mouse scroll --------------------------------
;; scroll one line at a time (less “jumpy” than defaults)
;; (unless (modulep! :ui smooth-scroll)
  (setq mouse-wheel-scroll-amount '(4 ((shift) . 8))) ;; 4 lines / 8 columns at a time,
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
(add-hook 'c-ts-mode-hook (lambda () (c-ts-mode-toggle-comment-style -1)))

;; --------------------------------- indent -----------------------------------
;; 设置 c-ts-mode 默认缩进为 4 空格
;; 项目中的 .editorconfig 或 .dir-locals.el 仍会覆盖此设置
(setq c-ts-mode-indent-offset 4)
;; 如果你也想设置 tab-width 和使用空格
(add-hook 'c-ts-mode-hook
          (lambda ()
            (setq-local tab-width 4)
            (setq-local indent-tabs-mode nil)))

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
                               (t . 3)))

  ;; Use the current candidate list for woccur after `ivy-restrict-to-matches'.
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

  ;; Avoid selected candidates getting saved into history after `ivy-done`.
  ;; Ivy already updates history itself; we disable the minibuffer's own
  ;; history insertion for search-like callers so only the input remains.
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
;; ------------------------ make _ as part of word ----------------------------
;; For c
(add-hook 'c-ts-mode-hook
          (lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'c-mode-common-hook
          (lambda () (modify-syntax-entry ?_ "w")))
;; For python
(add-hook 'python-mode-hook
          (lambda () (modify-syntax-entry ?_ "w")))
;; For Javascript
(add-hook 'js2-mode-hook
          (lambda () (modify-syntax-entry ?_ "w")))
;; For ld script
(add-hook 'ld-script-mode-hook
          (lambda () (modify-syntax-entry ?_ "w")))
;; For elisp
(add-hook 'emacs-lisp-mode-hook
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
;; ccls
(after! ccls
  (setq ccls-initialization-options
        '(:index (:comments 2)
          :completion (:detailedLabel t)))
  ;; optional as ccls is the default in Doom, if you want to use clangd, let the priority smaller than clangd
  (set-lsp-priority! 'ccls 2))

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

;; SPC w i to delete other windows
(map!
 (:leader
    :desc "delete other windows" :n "w i" #'delete-other-windows))


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

;; Contract selection only when a region is active (avoid accidental deletes).
(defun +my/contract-region-maybe ()
  "Contract active region if present; otherwise do nothing."
  (interactive)
  (when (use-region-p)
    (er/contract-region 1)))

;; Recenter point to top + 4 lines (line 5 from top).
(defun +my/recenter-top-plus-5 ()
  "Recenter window so current line is 5th from top."
  (interactive)
  (recenter 5))

;; Backspace to expand region (word -> larger units)
(map! :n [backspace] #'er/expand-region
      :v [backspace] #'er/expand-region)

;; Delete to contract region when selection exists
(map! :nv [delete] #'+my/contract-region-maybe)

;; Enter to recenter current line to top + n in normal state
(map! :n "RET" #'+my/recenter-top-plus-5)

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

;; Hide ^M
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(add-hook 'text-mode-hook 'remove-dos-eol)
(add-hook 'c-mode-common-hook 'remove-dos-eol)
(add-hook 'c-ts-mode-hook 'remove-dos-eol)


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
  ;; visual line move
  (define-key evil-motion-state-map "j" 'evil-next-visual-line)
  (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
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
"静默加载项目根目录的 .dark.el，且不干扰 minibuffer 显示"
  (let* ((project-root (or (when (bound-and-true-p lsp-mode)
                             (lsp-workspace-root))
                           (doom-project-root)
                           default-directory))
         (dk-cfg-path (expand-file-name ".dark.el" project-root)))
    (when (and dk-cfg-path (file-exists-p dk-cfg-path))
      ;; 关键：不产生任何 message，避免覆盖 workspace tabline
      (let ((inhibit-message t)
            (message-log-max nil))
        (load dk-cfg-path nil 'nomessage 'nosuffix)))))


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
  (advice-add 'magit-stash-save :after #'dk-syncthing-after-magit)
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



(map! :map prog-mode-map :gni "TAB"   #'indent-for-tab-command
      :map prog-mode-map :gni "<tab>" #'indent-for-tab-command)

(after! corfu
  ;;(setq corfu-auto nil)
  (setq tab-always-indent t))

(after! tramp
        (add-to-list 'tramp-remote-path 'tramp-own-remote-path))



;; ---------------------------------- open sub git repo from list ----------------------------------------------

(defvar my/repo-submodules-cache nil
  "Cache for discovered git submodules in the project.")

(defvar my/repo-submodules-cache-root nil
  "Project root for which the cache was generated.")

(defun my/find-git-repos (root-dir &optional max-depth)
  "Find all git repositories under ROOT-DIR, including nested ones.
MAX-DEPTH limits how deep to search (default: 5)."
  (let ((max-depth (or max-depth 5))
        (repos '()))
    (cl-labels ((scan-dir (dir depth)
                  (when (<= depth max-depth)
                    (let ((git-dir (expand-file-name ".git" dir)))
                      ;; 如果找到 git 仓库，添加到列表
                      (when (file-exists-p git-dir)
                        (push (file-relative-name dir root-dir) repos)))
                    ;; 继续扫描子目录（即使当前目录是 git 仓库）
                    (dolist (file (directory-files dir t))
                      (when (and (file-directory-p file)
                                 (not (string-match-p "/\\.\\.?$" file)) ; 排除 . 和 ..
                                 (not (string-match-p "/\\.git$" file)) ; 排除 .git 目录
                                 (not (string-match-p "/\\.repo$" file)) ; 排除 .repo 目录
                                 ;; 排除常见大目录
                                 (not (string-match-p "/node_modules$" file))
                                 (not (string-match-p "/build$" file))
                                 (not (string-match-p "/dist$" file))
                                 (not (string-match-p "/target$" file))
                                 (not (string-match-p "/vendor$" file))
                                 (not (string-match-p "/\\.cache$" file))
                                 (not (string-match-p "/\\.ccls-cache$" file))
                                 (not (string-match-p "/\\.xmake$" file))
                                 (not (string-match-p "/\\.gradle$" file))
                                 (not (string-match-p "/\\.idea$" file))
                                 (not (string-match-p "/out$" file)))
                        (scan-dir file (1+ depth)))))))
      (scan-dir root-dir 0))
    (sort repos #'string<)))

(defun my/get-project-root ()
  "Get the project root directory."
  (or (doom-project-root)
      (and (fboundp 'lsp-workspace-root) (lsp-workspace-root))
      default-directory))

(defun my/refresh-repo-submodules-cache ()
  "Scan and refresh the cache of git submodules."
  (interactive)
  (let ((root (my/get-project-root)))
    (message "Scanning for git repositories in %s..." root)
    (setq my/repo-submodules-cache (my/find-git-repos root))
    (setq my/repo-submodules-cache-root root)
    (message "Found %d git repositories" (length my/repo-submodules-cache))
    my/repo-submodules-cache))

(defun my/get-repo-submodules (&optional force-refresh)
  "Get list of git submodules, using cache if available.
With FORCE-REFRESH or when project root changes, rescan the directory."
  (let ((current-root (my/get-project-root)))
    (when (or force-refresh
              (null my/repo-submodules-cache)
              (not (equal current-root my/repo-submodules-cache-root)))
      (my/refresh-repo-submodules-cache)))
  my/repo-submodules-cache)

(defun my/magit-status-submodule--action (submodule)
  "Open magit-status for SUBMODULE."
  (let* ((project-root (my/get-project-root))
         (full-path (expand-file-name submodule project-root)))
    (if (file-directory-p full-path)
        (magit-status full-path)
      (user-error "Directory not found: %s" full-path))))

(defun my/magit-status-submodule ()
  "Select a git submodule using ivy and open its magit-status.
Supports ivy-resume."
  (interactive)
  (let* ((project-root (my/get-project-root))
         (submodules (my/get-repo-submodules)))
    (if submodules
        (ivy-read "Select git repository: "
                  submodules
                  :require-match t
                  :action #'my/magit-status-submodule--action
                  :caller 'my/magit-status-submodule)
      (user-error "No git repositories found in %s" project-root))))

(defun my/magit-status-submodule-refresh ()
  "Refresh cache and select a git submodule."
  (interactive)
  (my/refresh-repo-submodules-cache)
  (my/magit-status-submodule))



;; Open .ignore in current project root
(defun my/open-project-ignore ()
  "Open .ignore in the current project root, or message if missing."
  (interactive)
  (let* ((root (or (doom-project-root)
                   (and (fboundp 'projectile-project-root)
                        (projectile-project-root))))
         (ignore-file (and root (expand-file-name ".ignore" root))))
    (cond
     ((not root)
      (message "No project root found"))
     ((file-exists-p ignore-file)
     (find-file ignore-file))
     (t
      (message "No .ignore found in %s" root)))))

;; Open .dark.el in current project root
(defun my/open-project-dark ()
  "Open .dark.el in the current project root, or message if missing."
  (interactive)
  (let* ((root (or (doom-project-root)
                   (and (fboundp 'projectile-project-root)
                        (projectile-project-root))))
         (dark-file (and root (expand-file-name ".dark.el" root))))
    (cond
     ((not root)
      (message "No project root found"))
     ((file-exists-p dark-file)
      (find-file dark-file))
     (t
      (message "No .dark.el found in %s" root)))))

;; 按键绑定
(map! :leader
      (:prefix ("g" . "git")
       :desc "Open repo list" "a" #'my/magit-status-submodule
       :desc "Refresh & select repo" "A" #'my/magit-status-submodule-refresh
       :desc "Open .ignore" "i" #'my/open-project-ignore
       :desc "Open .dark.el" "d" #'my/open-project-dark))



;; ------------------------- 切换workspace时重新加载.dark.el --------------------
(defun my/workspace-switch-hook (&rest _)
  (run-with-idle-timer 0.3 nil #'dk-load-cfg-file))

;; advice workspace 切换函数
(advice-add '+workspace/switch-to :after #'my/workspace-switch-hook)



;; -------------------- 查找所有文件，不理会ignore规则 ------------------------
(defun +projectile-find-file-all ()
  "Find any file in the current project, ignoring ignore rules.
Works with Ivy if available, otherwise falls back to completing-read."
  (interactive)
  (let* ((project-root (projectile-project-root))
         ;; 确保命令在 project-root 下执行
         (default-directory project-root)
         (cmd (cond
               ((executable-find "fd")
                "fd . --type f -I --hidden --follow")
               ((executable-find "fdfind")
                "fdfind . --type f --hidden --follow")
               (t "find . -type f")))
         (files (split-string
                 (shell-command-to-string cmd)
                 "\n" t)))
    (if (require 'ivy nil t) ;; 检查 ivy 是否可用
        (ivy-read "Find file (all): "
                  files
                  :action (lambda (f)
                            (find-file (expand-file-name f project-root)))
                  :caller '+projectile-find-file-all)
      ;; fallback: 用 completing-read
      (let ((f (completing-read "Find file (all): " files)))
        (find-file (expand-file-name f project-root))))))


;; 绑定快捷键 SPC _
(map! :leader
      :desc "Find all files (ignore rules)"
      "_" #'+projectile-find-file-all)



;; ivy 显示的workspace buffer里有太多无用的列了，这里只留文件名和路径
(after! ivy-rich
  (plist-put! ivy-rich-display-transformers-list
              'ivy-switch-buffer
              '(:columns
                ((ivy-switch-buffer-transformer (:width 0.45))  ; buffer 名 + 图标
                 ;; 注释掉下面三行 → 去掉 size/major-mode/project
                 ;; (ivy-rich-switch-buffer-size (:width 7))
                 ;; (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
                 ;; (ivy-rich-switch-buffer-project (:width 0.18 :face success))
                 (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                 (ivy-rich-switch-buffer-path
                  (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
                :delimiter "  ")))


;; ------------------- 为 eldoc 结果中的整数添加十六进制显示，十六进制则显示十进制 --------------------
(defun my/format-hex-padded (num)
  "格式化数字为偶数位的小写十六进制"
  (let* ((hex (format "%x" num))
         (len (length hex)))
    (if (cl-oddp len)
        (concat "0" hex)
      hex)))

(defun my/hex-to-decimal (hex-str)
  "将十六进制字符串转换为十进制数"
  (string-to-number (replace-regexp-in-string "^0[xX]" "" hex-str) 16))

(defun my/eldoc-add-hex-value (result)
  "为 eldoc 结果中的整数添加十六进制显示，十六进制则显示十进制"
  (when (stringp result)
    ;; 跳过已经包含括号转换的结果
    (unless (string-match-p "([0-9xXa-fA-F]+)" result)
      ;; 匹配 NAME = 0xHEX 格式，转换为十进制
      (setq result
            (replace-regexp-in-string
             "\\b\\([A-Za-z_][A-Za-z0-9_]*\\)\\s-*=\\s-*\\(0[xX][0-9a-fA-F]+\\)\\s-*$"
             (lambda (match)
               (let* ((name (match-string 1 match))
                      (hex-str (match-string 2 match))
                      (num (my/hex-to-decimal hex-str)))
                 (format "%s = %s (%d)" name hex-str num)))
             result))
      ;; 匹配 #define NAME 0xHEX 格式，转换为十进制
      (setq result
            (replace-regexp-in-string
             "#define\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)\\s-+\\(0[xX][0-9a-fA-F]+\\)\\s-*$"
             (lambda (match)
               (let* ((name (match-string 1 match))
                      (hex-str (match-string 2 match))
                      (num (my/hex-to-decimal hex-str)))
                 (format "#define %s %s (%d)" name hex-str num)))
             result))
      ;; 匹配 NAME = 十进制 格式，转换为十六进制
      (setq result
            (replace-regexp-in-string
             "\\b\\([A-Za-z_][A-Za-z0-9_]*\\)\\s-*=\\s-*\\(-?[1-9][0-9]*\\)\\s-*$"
             (lambda (match)
               (let* ((name (match-string 1 match))
                      (num-str (match-string 2 match))
                      (num (string-to-number num-str))
                      (abs-num (logand num #xFFFFFFFFFFFFFFFF)))
                 (if (or (> num 9) (< num -9))
                     (format "%s = %s (0x%s)" name num-str
                             (my/format-hex-padded abs-num))
                   match)))
             result))
      ;; 匹配 #define NAME 十进制 格式，转换为十六进制
      (setq result
            (replace-regexp-in-string
             "#define\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)\\s-+\\([1-9][0-9]*\\)\\s-*$"
             (lambda (match)
               (let* ((name (match-string 1 match))
                      (num-str (match-string 2 match))
                      (num (string-to-number num-str))
                      (abs-num (logand num #xFFFFFFFFFFFFFFFF)))
                 (if (> num 9)
                     (format "#define %s %s (0x%s)" name num-str
                             (my/format-hex-padded abs-num))
                   match)))
             result))))
  result)

(after! lsp-mode
  (advice-add 'lsp--render-element :filter-return #'my/eldoc-add-hex-value))






(defun my/copy-syncthing-path ()
  "将当前文件路径转换为 Windows 格式并复制到剪贴板。
如果 syncthing-folder-id 变量有值，将 /home/dark 替换为 d:，
并将路径分隔符转换为 Windows 格式。"
  (interactive)
  (if (and (boundp 'syncthing-folder-id) syncthing-folder-id)
      (let* ((file-path (buffer-file-name))
             (converted-path (if file-path
                                (progn
                                  ;; 替换 /home/dark 为 d:
                                  (setq file-path (replace-regexp-in-string "^/home/dark" "d:" file-path))
                                  ;; 将 / 替换为 \
                                  (replace-regexp-in-string "/" "\\\\" file-path))
                              nil)))
        (if converted-path
            (progn
              (kill-new converted-path)
              (message "已复制到剪贴板: %s" converted-path))
          (message "当前 buffer 没有关联的文件")))
    (message "syncthing-folder-id 未设置")))

(map! :leader
      :desc "Copy file path for win" "o c" #'my/copy-syncthing-path)
