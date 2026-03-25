;;; modules/project.el -*- lexical-binding: t; -*-

;; Do not add projects automatically.
(setq projectile-track-known-projects-automatically nil)

(defun dk-workspace-name-fun (project-root)
  "Return a workspace name for PROJECT-ROOT."
  (or (with-temp-buffer
        (setq default-directory project-root)
        (hack-dir-local-variables-non-file-buffer)
        (when (boundp 'projectile-project-name)
          projectile-project-name))
      (projectile-default-project-name project-root)))

(setq projectile-project-name-function #'dk-workspace-name-fun)

(defun dk-frame-title-context ()
  "Return the workspace or project name shown in the frame title."
  (let ((workspace-name (when (fboundp '+workspace-current-name)
                          (+workspace-current-name))))
    (cond
     ((and (stringp workspace-name)
           (not (equal workspace-name ""))
           (not (and (boundp '+workspaces-main)
                     (string= workspace-name +workspaces-main))))
      workspace-name)
     ((when-let* ((project-name (and (fboundp 'doom-project-name)
                                     (doom-project-name)))
                  ((not (string= project-name "-"))))
        project-name))
     ((when-let* ((root (or (and (fboundp 'doom-project-root)
                                 (doom-project-root))
                            default-directory)))
        (file-name-nondirectory (directory-file-name root))))
     ("Emacs"))))

(setq frame-title-format '("%b - " (:eval (dk-frame-title-context)))
      icon-title-format frame-title-format)

(defun dk-find-project-file (root filename)
  "Find FILENAME in ROOT or its immediate subdirectories."
  (when (and root (file-directory-p root))
    (let ((root-file (expand-file-name filename root)))
      (if (file-exists-p root-file)
          root-file
        (catch 'found
          (dolist (entry (directory-files root t))
            (when (and (file-directory-p entry)
                       (not (member (file-name-nondirectory entry) '("." ".."))))
              (let ((candidate (expand-file-name filename entry)))
                (when (file-exists-p candidate)
                  (throw 'found candidate)))))
          nil)))))

(defun dk-load-cfg-file ()
  "Silently load `.dark.el' from the project root or its first-level children."
  (let* ((project-root (or (when (bound-and-true-p lsp-mode)
                             (lsp-workspace-root))
                           (doom-project-root)
                           default-directory))
         (dk-cfg-path (dk-find-project-file project-root ".dark.el")))
    (when (and dk-cfg-path (file-exists-p dk-cfg-path))
      (let ((inhibit-message t)
            (message-log-max nil))
        (load dk-cfg-path nil 'nomessage 'nosuffix)))))

(defun dk-load-cfg-after-workspace-created (&rest _args)
  (run-with-timer 2 nil #'dk-load-cfg-file))

(advice-add #'+workspace-new :after #'dk-load-cfg-after-workspace-created)

(defun dk-update-project-files-by-ignore ()
  (when (and (buffer-file-name)
             (equal ".ignore" (file-name-nondirectory (buffer-file-name))))
    (projectile-invalidate-cache nil)))

(add-hook 'after-save-hook #'dk-update-project-files-by-ignore)

(defun dk-project-reload-dir-locals (proj)
  "Reload .dir-locals for all buffers in PROJ."
  (interactive (list (project-current)))
  (unless proj
    (user-error "There doesn't seem to be a project here"))
  (hack-dir-local-variables)
  (let ((locals dir-local-variables-alist))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (equal proj (project-current))
                   buffer-file-name)
          (setq-local dir-local-variables-alist locals)
          (hack-local-variables-apply))))))

(defun dk-enable-autoreload-for-dir-locals ()
  (when (and (buffer-file-name)
             (equal dir-locals-file (file-name-nondirectory (buffer-file-name))))
    (add-hook 'after-save-hook #'dk-project-reload-dir-locals nil t)))

(add-hook 'emacs-lisp-mode-hook #'dk-enable-autoreload-for-dir-locals)

(defvar dk-repo-submodules-cache nil
  "Cache for discovered git submodules in the project.")

(defvar dk-repo-submodules-cache-root nil
  "Project root for which the cache was generated.")

(defun dk-find-git-repos (root-dir &optional max-depth)
  "Find all git repositories under ROOT-DIR up to MAX-DEPTH."
  (let ((max-depth (or max-depth 5))
        (repos '()))
    (cl-labels ((scan-dir (dir depth)
                  (when (<= depth max-depth)
                    (let ((git-dir (expand-file-name ".git" dir)))
                      (when (file-exists-p git-dir)
                        (push (file-relative-name dir root-dir) repos)))
                    (dolist (file (directory-files dir t))
                      (when (and (file-directory-p file)
                                 (not (string-match-p "/\\.\\.?$" file))
                                 (not (string-match-p "/\\.git$" file))
                                 (not (string-match-p "/\\.repo$" file))
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

(defun dk-get-project-root ()
  "Get the current project root."
  (or (doom-project-root)
      (and (fboundp 'lsp-workspace-root) (lsp-workspace-root))
      default-directory))

(defun dk-refresh-repo-submodules-cache ()
  "Scan and refresh the cache of git submodules."
  (interactive)
  (let ((root (dk-get-project-root)))
    (message "Scanning for git repositories in %s..." root)
    (setq dk-repo-submodules-cache (dk-find-git-repos root)
          dk-repo-submodules-cache-root root)
    (message "Found %d git repositories" (length dk-repo-submodules-cache))
    dk-repo-submodules-cache))

(defun dk-get-repo-submodules (&optional force-refresh)
  "Get git submodules, refreshing on demand or when the root changes."
  (let ((current-root (dk-get-project-root)))
    (when (or force-refresh
              (null dk-repo-submodules-cache)
              (not (equal current-root dk-repo-submodules-cache-root)))
      (dk-refresh-repo-submodules-cache)))
  dk-repo-submodules-cache)

(defun dk-magit-status-submodule--action (submodule)
  "Open `magit-status' for SUBMODULE."
  (let* ((project-root (dk-get-project-root))
         (full-path (expand-file-name submodule project-root)))
    (if (file-directory-p full-path)
        (magit-status full-path)
      (user-error "Directory not found: %s" full-path))))

(defun dk-magit-status-submodule ()
  "Select a git submodule using ivy and open its magit-status."
  (interactive)
  (let* ((project-root (dk-get-project-root))
         (submodules (dk-get-repo-submodules)))
    (if submodules
        (ivy-read "Select git repository: "
                  submodules
                  :require-match t
                  :action #'dk-magit-status-submodule--action
                  :caller 'dk-magit-status-submodule)
      (user-error "No git repositories found in %s" project-root))))

(defun dk-magit-status-submodule-refresh ()
  "Refresh the git repository cache, then prompt."
  (interactive)
  (dk-refresh-repo-submodules-cache)
  (dk-magit-status-submodule))

(defun dk-open-project-ignore ()
  "Open `.ignore' in the current project root or its immediate subdirectories."
  (interactive)
  (let* ((root (or (doom-project-root)
                   (and (fboundp 'projectile-project-root)
                        (projectile-project-root))))
         (ignore-file (and root (dk-find-project-file root ".ignore"))))
    (cond
     ((not root)
      (message "No project root found"))
     ((and ignore-file (file-exists-p ignore-file))
      (find-file ignore-file))
     (t
      (message "No .ignore found in %s (or its immediate subdirectories)" root)))))

(defun dk-open-project-dark ()
  "Open `.dark.el' in the current project root or its immediate subdirectories."
  (interactive)
  (let* ((root (or (doom-project-root)
                   (and (fboundp 'projectile-project-root)
                        (projectile-project-root))))
         (dk-cfg-file (and root (dk-find-project-file root ".dark.el"))))
    (cond
     ((not root)
      (message "No project root found"))
     ((and dk-cfg-file (file-exists-p dk-cfg-file))
      (find-file dk-cfg-file))
     (t
      (message "No .dark.el found in %s (or its immediate subdirectories)" root)))))

(map! :leader
      (:prefix ("g" . "git")
       :desc "Open repo list" "a" #'dk-magit-status-submodule
       :desc "Refresh & select repo" "A" #'dk-magit-status-submodule-refresh
       :desc "Open .ignore" "i" #'dk-open-project-ignore
       :desc "Open .dark.el" "d" #'dk-open-project-dark))

(defun dk-workspace-switch-hook (&rest _)
  (run-with-idle-timer 0.3 nil #'dk-load-cfg-file))

(advice-add #'+workspace/switch-to :after #'dk-workspace-switch-hook)

(defun dk-projectile-find-file-all ()
  "Find any file in the current project, ignoring ignore rules."
  (interactive)
  (let* ((project-root (projectile-project-root))
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
    (if (require 'ivy nil t)
        (ivy-read "Find file (all): "
                  files
                  :action (lambda (f)
                            (find-file (expand-file-name f project-root)))
                  :caller 'dk-projectile-find-file-all)
      (let ((f (completing-read "Find file (all): " files)))
        (find-file (expand-file-name f project-root))))))

(map! :leader
      :desc "Find all files (ignore rules)"
      "_" #'dk-projectile-find-file-all)
