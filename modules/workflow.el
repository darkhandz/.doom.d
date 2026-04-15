;;; modules/workflow.el -*- lexical-binding: t; -*-

(defun dk-term-buffer-name ()
  "Ensure the popup terminal buffer is visible and return its name."
  (let* ((use-eshell (modulep! :term eshell))
         (use-vterm (modulep! :term vterm))
         (buffer-name
          (format (if use-eshell "*doom:eshell-popup:%s*" "*doom:vterm-popup:%s*")
                  (if (bound-and-true-p persp-mode)
                      (safe-persp-name (get-current-persp))
                    "main")))
         (buffer (get-buffer buffer-name)))
    (cond
     ((buffer-live-p buffer)
      (pop-to-buffer buffer))
     (use-eshell
      (+eshell/toggle nil))
     (use-vterm
      (+vterm/toggle nil))
     (t
      (user-error "Neither eshell nor vterm is enabled")))
    buffer-name))

(defun dk-detect-os-by-term-prompt ()
  "Detect the remote shell type from the popup terminal prompt."
  (let ((buffer-name (dk-term-buffer-name)))
    (with-current-buffer buffer-name
      (save-excursion
        (goto-char (point-max))
        (when (re-search-backward "[^ \t\n]" nil t)
          (let ((char-at-point (char-after)))
            (cond
             ((eq char-at-point ?$) 'linux)
             ((eq char-at-point ?>) 'win)
             (t 'unknown))))))))

(defun dk-term-run-cmd (arg)
  "Open the popup terminal and run ARG."
  (interactive "sCommand: ")
  (let ((buffer-name (dk-term-buffer-name))
        (use-eshell (modulep! :term eshell))
        (use-vterm (modulep! :term vterm)))
    (with-current-buffer buffer-name
      (let ((inhibit-read-only t))
        (cond
         (use-vterm
          (vterm-send-string arg)
          (vterm-send-return))
         (use-eshell
          (+eshell-run-command arg (current-buffer)))
         (t
          (user-error "Neither eshell nor vterm is enabled")))))))

(defun dk-vterm-toggle ()
  "Toggle the popup vterm window."
  (interactive)
  (+vterm/toggle nil))

(defun dk-run-cmd-with-cfg (cmd-var)
  "Load project config and execute the command stored in CMD-VAR."
  (interactive)
  (dk-load-cfg-file)
  (if dk-run-at-remote-win
      (let ((os (dk-detect-os-by-term-prompt)))
        (cond
         ((eq os 'linux)
          (dk-term-run-cmd dk-login-remote))
         ((eq os 'win)
          (dk-term-run-cmd (symbol-value cmd-var)))
         (t
          (dk-term-run-cmd "\x03"))))
    (dk-term-run-cmd (symbol-value cmd-var))))

(defconst dk-key-cmd-alist
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

(defun dk-set-keybindings (keymap)
  "Set keybindings for KEYMAP."
  (dolist (key-cmd dk-key-cmd-alist)
    (let ((key (car key-cmd))
          (cmd-var (cdr key-cmd)))
      (define-key keymap (kbd key)
        (lambda ()
          (interactive)
          (dk-run-cmd-with-cfg cmd-var)))))
  (define-key keymap (kbd "<f6>") #'dk-vterm-toggle))

(dk-set-keybindings (current-global-map))

(after! vterm
  (dk-set-keybindings vterm-mode-map)
  (map! :map vterm-mode-map :ni "M-1" #'+workspace/switch-to-0
        :map vterm-mode-map :ni "M-2" #'+workspace/switch-to-1
        :map vterm-mode-map :ni "M-3" #'+workspace/switch-to-2
        :map vterm-mode-map :ni "M-4" #'+workspace/switch-to-3
        :map vterm-mode-map :ni "M-5" #'+workspace/switch-to-4
        :map vterm-mode-map :ni "M-6" #'+workspace/switch-to-5
        :map vterm-mode-map :ni "M-7" #'+workspace/switch-to-6
        :map vterm-mode-map :ni "M-8" #'+workspace/switch-to-7
        :map vterm-mode-map :ni "M-9" #'+workspace/switch-to-8))

(defun dk-syncthing-trigger-on-save ()
  "Send a Syncthing rescan request after save when configured."
  (let ((folder-id (bound-and-true-p syncthing-folder-id))
        (api-key (getenv "SYNCTHING_API_KEY"))
        (server (getenv "SYNCTHING_SERVER")))
    (when (and folder-id api-key)
      (start-process
       "curl-process" nil
       "curl" "-X" "POST" "-H" (concat "X-API-Key: " api-key)
       (concat server "/rest/db/scan?folder=" folder-id)))
    (let ((inhibit-message t))
      (message "syncthing: %s %s" server folder-id))))

(add-hook 'after-save-hook #'dk-syncthing-trigger-on-save)

(defun dk-syncthing-after-magit (&rest _args)
  (dk-syncthing-trigger-on-save))

(after! magit
  (setq magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
  (advice-add #'magit-file-checkout :after #'dk-syncthing-after-magit)
  (advice-add #'magit-file-delete :after #'dk-syncthing-after-magit)
  (advice-add #'magit-file-rename :after #'dk-syncthing-after-magit)
  (advice-add #'magit-branch-reset :after #'dk-syncthing-after-magit)
  (advice-add #'magit-checkout :after #'dk-syncthing-after-magit)
  (advice-add #'magit-merge-plain :after #'dk-syncthing-after-magit)
  (advice-add #'magit-reset-internal :after #'dk-syncthing-after-magit)
  (advice-add #'magit-stash--apply :after #'dk-syncthing-after-magit)
  (advice-add #'magit-stash-save :after #'dk-syncthing-after-magit)
  (advice-add #'git-rebase-merge :after #'dk-syncthing-after-magit)
  (advice-add #'magit-discard-apply :after #'dk-syncthing-after-magit)
  (advice-add #'magit-discard-files :after #'dk-syncthing-after-magit)
  (advice-add #'magit-reverse-apply :after #'dk-syncthing-after-magit)
  (advice-add #'magit-reverse-files :after #'dk-syncthing-after-magit)
  (advice-add #'magit-apply-patch :after #'dk-syncthing-after-magit))

(defun dk-open-file-in-explorer ()
  "Open the current file in Explorer, selecting it when possible."
  (interactive)
  (let ((file (or buffer-file-name default-directory)))
    (when file
      (setq file (expand-file-name file))
      (if (eq system-type 'gnu/linux)
          (if (and (boundp 'dk-proj-wsl-root) (boundp 'dk-proj-win-root))
              (setq file (replace-regexp-in-string
                          (regexp-quote dk-proj-wsl-root) dk-proj-win-root file))
            (setq file (shell-command-to-string
                        (concat "wslpath -w " (shell-quote-argument file))))))
      (setq file (replace-regexp-in-string "/" "\\\\" file))
      (start-process "explorer" nil "explorer.exe" (concat "/select," file)))))

(defun dk-open-file-manager-and-select ()
  "Open the default file manager and select the current file."
  (interactive)
  (let ((file (or (buffer-file-name)
                  (read-file-name "Select file: "))))
    (cond
     ((eq system-type 'windows-nt)
      (call-process-shell-command
       (format "explorer /select,%s" (replace-regexp-in-string "/" "\\\\" file)) nil 0))
     ((eq system-type 'darwin)
      (call-process-shell-command
       (format "open -R %s" (shell-quote-argument file)) nil 0))
     ((eq system-type 'gnu/linux)
      (let ((fm (cond
                 ((executable-find "dolphin") "dolphin --select")
                 ((executable-find "nautilus") "nautilus --no-desktop --browser")
                 ((executable-find "thunar") "thunar")
                 (t nil))))
        (if fm
            (call-process-shell-command
             (format "%s %s" fm (shell-quote-argument file)) nil 0)
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


(defvar dk-bcompare-temp-files nil
  "Temporary files created for Beyond Compare buffer diffs.")

(defun dk-bcompare--sanitize-name (name)
  "Return a filesystem-safe string derived from NAME."
  (replace-regexp-in-string "[^[:alnum:]._+-]+" "_" name))

(defun dk-bcompare--buffer-temp-file (buffer)
  "Write BUFFER contents to a temporary file and return its path."
  (with-current-buffer buffer
    (let* ((base-name (dk-bcompare--sanitize-name (buffer-name buffer)))
           (source-file (buffer-file-name buffer))
           (ext (or (and source-file
                         (file-name-extension source-file t))
                    ""))
           (temp-file (make-temp-file (format "bcompare-%s-" base-name) nil ext)))
      (write-region nil nil temp-file nil 'silent)
      (push temp-file dk-bcompare-temp-files)
      temp-file)))

(defun dk-bcompare--buffer-path (buffer)
  "Return a path for BUFFER suitable for Beyond Compare.

Use the real file path when BUFFER visits a file.
Otherwise, write the current buffer contents to a temporary file."
  (with-current-buffer buffer
    (if buffer-file-name
        buffer-file-name
      (dk-bcompare--buffer-temp-file buffer))))

(defun dk-bcompare-cleanup-temp-files ()
  "Delete temporary files created for Beyond Compare buffer diffs."
  (interactive)
  (setq dk-bcompare-temp-files
        (seq-filter
         (lambda (file)
           (when (file-exists-p file)
             (delete-file file))
           nil)
         dk-bcompare-temp-files)))

(defun dk-bcompare-buffers (buf-a buf-b)
  "Select two buffers and compare them with Beyond Compare."
  (interactive
   (let* ((current (buffer-name (current-buffer)))
          (other (buffer-name (other-buffer (current-buffer) t)))
          (buf-a (read-buffer "Buffer A: " current t))
          (buf-b (read-buffer "Buffer B: " other t)))
     (list buf-a buf-b)))
  (let ((bcompare (or (executable-find "bcompare")
                      (user-error "Cannot find `bcompare` in PATH")))
        (buffer-a (get-buffer buf-a))
        (buffer-b (get-buffer buf-b)))
    (unless buffer-a
      (user-error "Buffer does not exist: %s" buf-a))
    (unless buffer-b
      (user-error "Buffer does not exist: %s" buf-b))
    (let ((file-a (dk-bcompare--buffer-path buffer-a))
          (file-b (dk-bcompare--buffer-path buffer-b)))
      (start-process
       (format "bcompare-%s-%s" (buffer-name buffer-a) (buffer-name buffer-b))
       nil
       bcompare
       file-a
       file-b)
      (message "Beyond Compare: %s <-> %s" (buffer-name buffer-a) (buffer-name buffer-b)))))

(add-hook 'kill-emacs-hook #'dk-bcompare-cleanup-temp-files)

(map! :leader
      :desc "Compare buffers with Beyond Compare"
      "o b" #'dk-bcompare-buffers)

(after! tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(defun dk-copy-syncthing-path ()
  "Convert the current file path to a Windows path and copy it."
  (interactive)
  (if (and (boundp 'syncthing-folder-id) syncthing-folder-id)
      (let* ((file-path (buffer-file-name))
             (converted-path (if file-path
                                 (progn
                                   (setq file-path
                                         (replace-regexp-in-string "^/home/dark" "d:" file-path))
                                   (replace-regexp-in-string "/" "\\\\" file-path))
                               nil)))
        (if converted-path
            (progn
              (kill-new converted-path)
              (message "已复制到剪贴板: %s" converted-path))
          (message "当前 buffer 没有关联的文件")))
    (message "syncthing-folder-id 未设置")))

(map! :leader
      :desc "Copy file path for win"
      "o c" #'dk-copy-syncthing-path)
