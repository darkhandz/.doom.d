;;; modules/workspace-session-guard.el -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar dk-workspace-session-debug nil
  "When non-nil, log verbose workspace/session restore diagnostics.")

(defvar dk-workspace-session-log-repairs t
  "When non-nil, log workspace/session repairs.")

(defvar dk-workspace-session-debug-idle-delays '(0.2 1.0 3.0)
  "Idle delays used for follow-up snapshots after session restoration.")

(defvar dk-workspace-session-debug--load-depth 0
  "Non-zero while a persp session is being loaded.")

(defvar dk-workspace-session-fix-foreign-window-buffers t
  "When non-nil, replace foreign window buffers after workspace activation.")

(defvar dk-workspace-session-clean-foreign-project-buffers t
  "When non-nil, remove project-owned buffers from foreign workspaces after load.")

(defvar dk-workspace-session-block-foreign-project-buffer-adds t
  "When non-nil, prevent project-owned buffers from joining foreign workspaces.")

(defun dk-workspace-session-debug--enabled-p ()
  "Return non-nil when workspace/session diagnostics should be logged."
  (and dk-workspace-session-debug
       (featurep 'persp-mode)))

(defun dk-workspace-session-debug--log (format-string &rest args)
  "Log FORMAT-STRING with ARGS for workspace/session diagnostics."
  (when dk-workspace-session-debug
    (apply #'message (concat "[dk-ws-session] " format-string) args)))

(defun dk-workspace-session-debug--repair-log (format-string &rest args)
  "Log FORMAT-STRING with ARGS for workspace/session repairs."
  (when dk-workspace-session-log-repairs
    (apply #'message (concat "[dk-ws-session] " format-string) args)))

(defun dk-workspace-session-debug-toggle ()
  "Toggle workspace/session restore diagnostics."
  (interactive)
  (setq dk-workspace-session-debug (not dk-workspace-session-debug))
  (message "dk-workspace-session-debug: %s"
           (if dk-workspace-session-debug "on" "off")))

(defun dk-workspace-session-repair-log-toggle ()
  "Toggle workspace/session repair logging."
  (interactive)
  (setq dk-workspace-session-log-repairs
        (not dk-workspace-session-log-repairs))
  (message "dk-workspace-session-log-repairs: %s"
           (if dk-workspace-session-log-repairs "on" "off")))

(defun dk-workspace-session-debug--persp-name (persp)
  "Return a printable name for PERSP."
  (cond
   ((null persp) "<nil>")
   ((and (fboundp 'safe-persp-name)
         (ignore-errors (safe-persp-name persp))))
   ((and (fboundp 'persp-name)
         (ignore-errors (persp-name persp))))
   (t (format "%S" persp))))

(defun dk-workspace-session-debug--current-persp (&optional frame window)
  "Return the active perspective for FRAME, WINDOW, or the selected frame."
  (cond
   ((and frame (fboundp 'get-frame-persp))
    (ignore-errors (get-frame-persp frame)))
   ((and window (fboundp 'get-current-persp))
    (ignore-errors
      (get-current-persp nil window)))
   ((fboundp 'get-current-persp)
    (ignore-errors (get-current-persp)))))

(defun dk-workspace-session-debug--persps ()
  "Return live perspectives in `*persp-hash*', excluding the nil workspace."
  (when (and (boundp '*persp-hash*) (hash-table-p *persp-hash*))
    (sort
     (cl-loop for name being the hash-keys of *persp-hash*
              for persp = (gethash name *persp-hash*)
              unless (and (boundp 'persp-nil-name)
                          (equal name persp-nil-name))
              collect persp)
     (lambda (a b)
       (string< (dk-workspace-session-debug--persp-name a)
                (dk-workspace-session-debug--persp-name b))))))

(defun dk-workspace-session-debug--persp-buffers (persp)
  "Return buffers belonging to PERSP."
  (when (and persp (fboundp 'persp-buffers))
    (delq nil (ignore-errors (persp-buffers persp)))))

(defun dk-workspace-session-debug--magit-buffer-p (buffer)
  "Return non-nil when BUFFER is a Magit buffer."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (derived-mode-p 'magit-mode))))

(defun dk-workspace-session-debug--buffer-file (buffer)
  "Return BUFFER's file name, or nil."
  (when (buffer-live-p buffer)
    (buffer-local-value 'buffer-file-name buffer)))

(defun dk-workspace-session-debug--magit-buffer-directory (buffer)
  "Return BUFFER's Magit repository directory, or nil."
  (when (dk-workspace-session-debug--magit-buffer-p buffer)
    (let ((dir (buffer-local-value 'magit--default-directory buffer)))
      (when (and (stringp dir)
                 (not (string= dir "")))
        (file-name-as-directory (expand-file-name dir))))))

(defun dk-workspace-session-debug--buffer-directory (buffer)
  "Return BUFFER's local default directory, or nil."
  (when (buffer-live-p buffer)
    (let ((dir (buffer-local-value 'default-directory buffer)))
      (when (and (stringp dir)
                 (not (string= dir "")))
        dir))))

(defun dk-workspace-session-debug--buffer-path (buffer)
  "Return the file or directory path that identifies BUFFER's project."
  (or (dk-workspace-session-debug--buffer-file buffer)
      (dk-workspace-session-debug--magit-buffer-directory buffer)
      (dk-workspace-session-debug--buffer-directory buffer)))

(defun dk-workspace-session-debug--buffer-mode (buffer)
  "Return BUFFER's major mode."
  (when (buffer-live-p buffer)
    (buffer-local-value 'major-mode buffer)))

(defun dk-workspace-session-debug--buffer-label (buffer)
  "Return a compact diagnostic label for BUFFER."
  (if (buffer-live-p buffer)
      (let ((file (dk-workspace-session-debug--buffer-file buffer))
            (repo (dk-workspace-session-debug--magit-buffer-directory buffer))
            (dir (dk-workspace-session-debug--buffer-directory buffer)))
        (format "%s mode=%s%s%s"
                (buffer-name buffer)
                (dk-workspace-session-debug--buffer-mode buffer)
                (if file
                    (format " file=%s" (abbreviate-file-name file))
                  "")
                (cond
                 (file "")
                 (repo
                  (concat " repo=" (abbreviate-file-name repo)
                          (if (and dir (not (string= repo dir)))
                              (concat " dir=" (abbreviate-file-name dir))
                            "")))
                 (dir
                  (format " dir=%s" (abbreviate-file-name dir)))
                 (t ""))))
    "<dead-buffer>"))

(defun dk-workspace-session-debug--selected-window-buffer ()
  "Return the selected window's buffer, falling back to `current-buffer'."
  (let ((window (selected-window)))
    (if (window-live-p window)
        (window-buffer window)
      (current-buffer))))

(defun dk-workspace-session-debug--sorted-strings (strings)
  "Return a sorted copy of STRINGS without nil entries."
  (sort (delq nil (copy-sequence strings)) #'string<))

(defun dk-workspace-session-debug--buffer-persps-by-scan (buffer)
  "Return names of perspectives whose buffer lists contain BUFFER."
  (dk-workspace-session-debug--sorted-strings
   (cl-loop for persp in (dk-workspace-session-debug--persps)
            when (memq buffer (dk-workspace-session-debug--persp-buffers persp))
            collect (dk-workspace-session-debug--persp-name persp))))

(defun dk-workspace-session-debug--buffer-persps-by-index (buffer)
  "Return names in BUFFER's internal persp reverse index."
  (dk-workspace-session-debug--sorted-strings
   (when (and (buffer-live-p buffer)
              (fboundp 'persp--buffer-in-persps))
     (mapcar #'dk-workspace-session-debug--persp-name
             (delq nil (ignore-errors (persp--buffer-in-persps buffer)))))))

(defun dk-workspace-session-debug--same-string-set-p (a b)
  "Return non-nil when string lists A and B contain the same values."
  (null (cl-set-exclusive-or a b :test #'string=)))

(defun dk-workspace-session-debug--workspace-project (persp)
  "Return the project root recorded on PERSP, if any."
  (when (and persp (fboundp 'persp-parameter))
    (ignore-errors (persp-parameter '+workspace-project persp))))

(defun dk-workspace-session-debug--workspace-project-dir (persp)
  "Return PERSP's project root as an expanded directory, or nil."
  (when-let* ((root (dk-workspace-session-debug--workspace-project persp))
              ((stringp root))
              ((not (string= root ""))))
    (file-name-as-directory (expand-file-name root))))

(defun dk-workspace-session-debug--local-path-in-dir-p (path dir)
  "Return non-nil when local PATH is under local DIR."
  (when (and path dir
             (not (file-remote-p path))
             (not (file-remote-p dir)))
    (let* ((path (expand-file-name path))
           (dir (file-name-as-directory (expand-file-name dir)))
           (dir* (directory-file-name dir)))
      (or (string= (directory-file-name path) dir*)
          (string-prefix-p dir path)))))

(defun dk-workspace-session-debug--path-related-p (path dir)
  "Return non-nil when PATH and DIR describe the same directory tree."
  (when (and path dir
             (not (file-remote-p path))
             (not (file-remote-p dir)))
    (let ((path (file-name-as-directory (expand-file-name path)))
          (dir (file-name-as-directory (expand-file-name dir))))
      (or (string-prefix-p dir path)
          (string-prefix-p path dir)))))

(defun dk-workspace-session-debug--workspace-buffer-names (persp)
  "Return printable buffer labels for PERSP."
  (mapcar #'dk-workspace-session-debug--buffer-label
          (dk-workspace-session-debug--persp-buffers persp)))

(defun dk-workspace-session-debug--frame-name (frame)
  "Return a printable name for FRAME."
  (or (frame-parameter frame 'name)
      (format "%S" frame)))

(defun dk-workspace-session-debug--log-window-mismatches ()
  "Log windows that display buffers outside their active workspace."
  (when (and (fboundp 'window-list)
             (fboundp 'persp-contain-buffer-p))
    (dolist (frame (frame-list))
      (let ((persp (dk-workspace-session-debug--current-persp frame)))
        (dolist (window (window-list frame 'no-minibuf))
          (let ((buffer (window-buffer window)))
            (unless (or (null persp)
                        (not (buffer-live-p buffer))
                        (persp-contain-buffer-p buffer persp))
              (dk-workspace-session-debug--log
               "WINDOW-MISMATCH frame=%s workspace=%s selected=%s buffer={%s} buffer-workspaces=%S"
               (dk-workspace-session-debug--frame-name frame)
               (dk-workspace-session-debug--persp-name persp)
               (eq window (selected-window))
               (dk-workspace-session-debug--buffer-label buffer)
               (dk-workspace-session-debug--buffer-persps-by-scan buffer)))))))))

(defun dk-workspace-session-debug--fix-window-mismatches (&optional frame persp)
  "Replace foreign buffers displayed in FRAME windows for PERSP.

This compensates for restored window states that refer to buffers by name only,
which is ambiguous when different projects contain files with the same basename."
  (when (and dk-workspace-session-fix-foreign-window-buffers
             (fboundp 'window-list)
             (fboundp 'persp-contain-buffer-p))
    (let* ((frame (or frame (selected-frame)))
           (persp (or persp (dk-workspace-session-debug--current-persp frame))))
      (when persp
        (let ((fixed 0))
          (dolist (window (window-list frame 'no-minibuf))
            (let ((buffer (window-buffer window)))
              (when (dk-workspace-session-debug--foreign-buffer-p buffer persp)
                (let ((replacement
                       (dk-workspace-session-debug--replacement-buffer buffer persp)))
                  (when (and (buffer-live-p replacement)
                             (not (eq replacement buffer)))
                    (dk-workspace-session-debug--repair-log
                     "FIX-WINDOW-MISMATCH frame=%s workspace=%s buffer={%s} buffer-workspaces=%S replacement={%s}"
                     (dk-workspace-session-debug--frame-name frame)
                     (dk-workspace-session-debug--persp-name persp)
                     (dk-workspace-session-debug--buffer-label buffer)
                     (dk-workspace-session-debug--buffer-persps-by-scan buffer)
                     (dk-workspace-session-debug--buffer-label replacement))
                    (set-window-buffer window replacement)
                    (cl-incf fixed))))))
          fixed)))))

(defun dk-workspace-session-debug-fix-selected-frame ()
  "Fix foreign buffers displayed in the selected frame."
  (interactive)
  (dk-workspace-session-debug--fix-window-mismatches))

(defun dk-workspace-session-debug-schedule-fix (&optional event)
  "Schedule a near-immediate mismatch fix for EVENT."
  (when dk-workspace-session-fix-foreign-window-buffers
    (run-with-idle-timer
     0 nil
     (lambda (event)
       (dk-workspace-session-debug--log "FIX-IDLE event=%s" event)
       (let ((fixed (or (dk-workspace-session-debug--fix-window-mismatches)
                        0))
             (removed (or (dk-workspace-session-debug-clean-foreign-project-buffers)
                          0)))
       (dk-workspace-session-debug--log
        "FIX-IDLE-DONE event=%s current=%s fixed=%d removed=%d selected-window-buffer={%s}"
        event
        (dk-workspace-session-debug--persp-name
         (dk-workspace-session-debug--current-persp))
        fixed
        removed
        (dk-workspace-session-debug--buffer-label
         (dk-workspace-session-debug--selected-window-buffer)))))
     (or event "manual"))))

(defun dk-workspace-session-debug--buffer-owner-persps (buffer current-persp)
  "Return loaded perspectives that own BUFFER's path, excluding CURRENT-PERSP."
  (when (dk-workspace-session-debug--buffer-path buffer)
    (cl-loop for persp in (dk-workspace-session-debug--persps)
             for root = (dk-workspace-session-debug--workspace-project-dir persp)
             when (and (not (eq persp current-persp))
                       (dk-workspace-session-debug--buffer-local-to-workspace-p
                        buffer persp))
             collect persp)))

(defun dk-workspace-session-debug--buffer-local-to-workspace-p (buffer persp)
  "Return non-nil when BUFFER belongs in PERSP."
  (let ((root (dk-workspace-session-debug--workspace-project-dir persp))
        (path (dk-workspace-session-debug--buffer-path buffer)))
    (and root path
         (if (dk-workspace-session-debug--magit-buffer-p buffer)
             (dk-workspace-session-debug--path-related-p path root)
           (dk-workspace-session-debug--local-path-in-dir-p path root)))))

(defun dk-workspace-session-debug--foreign-project-owners (buffer persp)
  "Return owner perspectives if BUFFER does not belong in PERSP."
  (when-let* ((root (dk-workspace-session-debug--workspace-project-dir persp))
              (path (dk-workspace-session-debug--buffer-path buffer))
              ((not (dk-workspace-session-debug--buffer-local-to-workspace-p
                     buffer persp))))
    (dk-workspace-session-debug--buffer-owner-persps buffer persp)))

(defun dk-workspace-session-debug--foreign-buffer-p (buffer persp)
  "Return non-nil when BUFFER should not be displayed in PERSP."
  (and (buffer-live-p buffer)
       persp
       (dk-workspace-session-debug--foreign-project-owners buffer persp)))

(defun dk-workspace-session-debug--replacement-buffer (old-buffer persp)
  "Return a suitable replacement for OLD-BUFFER in PERSP."
  (or (cl-find-if
       (lambda (buffer)
         (and (buffer-live-p buffer)
              (not (eq buffer old-buffer))
              (not (dk-workspace-session-debug--foreign-project-owners
                    buffer persp))))
       (dk-workspace-session-debug--persp-buffers persp))
      (car (cl-remove-if-not #'buffer-live-p
                             (dk-workspace-session-debug--persp-buffers persp)))
      (and (fboundp 'doom-fallback-buffer)
           (doom-fallback-buffer))
      (get-buffer "*scratch*")))

(defun dk-workspace-session-debug--filter-foreign-add-buffers
    (buffs-or-names persp)
  "Return BUFFS-OR-NAMES with foreign project buffers removed for PERSP."
  (if (or (not dk-workspace-session-block-foreign-project-buffer-adds)
          (> dk-workspace-session-debug--load-depth 0)
          (null buffs-or-names)
          (null persp)
          (not (fboundp 'persp-get-buffer-or-null)))
      buffs-or-names
    (let* ((was-list (listp buffs-or-names))
           (items (if was-list buffs-or-names (list buffs-or-names)))
           accepted)
      (dolist (item items)
        (let* ((buffer (persp-get-buffer-or-null item))
               (owners (and buffer
                            (dk-workspace-session-debug--foreign-project-owners
                             buffer persp))))
          (if (and buffer
                   (dk-workspace-session-debug--foreign-buffer-p buffer persp))
              (dk-workspace-session-debug--repair-log
               "BLOCK-ADD-PROJECT-MISMATCH workspace=%s buffer={%s} owner-workspaces=%S input=%S"
               (dk-workspace-session-debug--persp-name persp)
               (dk-workspace-session-debug--buffer-label buffer)
               (mapcar #'dk-workspace-session-debug--persp-name owners)
               item)
            (push item accepted))))
      (setq accepted (nreverse accepted))
      (if was-list
          accepted
        (car accepted)))))

(defun dk-workspace-session-debug-clean-foreign-project-buffers ()
  "Remove buffers from workspaces when another loaded workspace owns their file.

This targets session files polluted by basename-only window restoration: a file
buffer can be displayed in the wrong workspace, then later saved as belonging to
that workspace.  Cleanup only removes a buffer when its file is outside the
current workspace project root and inside another loaded workspace where that
same buffer is already present."
  (interactive)
  (when (and dk-workspace-session-clean-foreign-project-buffers
             (fboundp 'persp-remove-buffer))
    (let ((removed 0))
      (dolist (persp (dk-workspace-session-debug--persps))
        (let ((root (dk-workspace-session-debug--workspace-project-dir persp)))
          (when root
            (dolist (buffer (copy-sequence
                             (dk-workspace-session-debug--persp-buffers persp)))
              (let* ((path (dk-workspace-session-debug--buffer-path buffer))
                     (owners (and path
                                  (not (dk-workspace-session-debug--local-path-in-dir-p
                                        path root))
                                  (dk-workspace-session-debug--buffer-owner-persps
                                   buffer persp))))
                (when (and owners
                           (dk-workspace-session-debug--foreign-buffer-p
                            buffer persp))
                  (dk-workspace-session-debug--repair-log
                   "REMOVE-PROJECT-MISMATCH workspace=%s root=%s buffer={%s} owner-workspaces=%S"
                   (dk-workspace-session-debug--persp-name persp)
                   root
                   (dk-workspace-session-debug--buffer-label buffer)
                   (mapcar #'dk-workspace-session-debug--persp-name owners))
                  (when (ignore-errors
                          (persp-remove-buffer buffer persp nil nil nil nil)
                          t)
                    (cl-incf removed))))))))
      removed)))

(defun dk-workspace-session-debug--log-index-mismatches ()
  "Log differences between persp buffer lists and buffer reverse indexes."
  (dolist (buffer (buffer-list))
    (let ((by-scan (dk-workspace-session-debug--buffer-persps-by-scan buffer))
          (by-index (dk-workspace-session-debug--buffer-persps-by-index buffer)))
      (unless (dk-workspace-session-debug--same-string-set-p by-scan by-index)
        (dk-workspace-session-debug--log
         "INDEX-MISMATCH buffer={%s} scan=%S index=%S"
         (dk-workspace-session-debug--buffer-label buffer)
         by-scan
         by-index)))))

(defun dk-workspace-session-debug-snapshot (&optional event)
  "Log a full workspace/session diagnostic snapshot.

EVENT is a short string describing why the snapshot was taken."
  (interactive)
  (let ((event (or event "manual")))
    (when (dk-workspace-session-debug--enabled-p)
      (condition-case err
          (let* ((current-persp (dk-workspace-session-debug--current-persp))
                 (workspace-names (mapcar #'dk-workspace-session-debug--persp-name
                                          (dk-workspace-session-debug--persps)))
                 (orphaned (when (fboundp '+workspace-orphaned-buffer-list)
                             (ignore-errors (+workspace-orphaned-buffer-list)))))
            (dk-workspace-session-debug-clean-foreign-project-buffers)
            (dk-workspace-session-debug--log
             "SNAPSHOT-BEGIN event=%s persp-mode=%S load-depth=%s current=%s selected-buffer={%s} workspaces=%S buffers=%d orphaned=%d"
             event
             (bound-and-true-p persp-mode)
             dk-workspace-session-debug--load-depth
             (dk-workspace-session-debug--persp-name current-persp)
             (dk-workspace-session-debug--buffer-label
              (dk-workspace-session-debug--selected-window-buffer))
             workspace-names
             (length (buffer-list))
             (length orphaned))
            (dolist (persp (dk-workspace-session-debug--persps))
              (let ((buffers (dk-workspace-session-debug--persp-buffers persp)))
                (dk-workspace-session-debug--log
                 "WORKSPACE name=%s project=%s buffer-count=%d buffers=%S"
                 (dk-workspace-session-debug--persp-name persp)
                 (or (dk-workspace-session-debug--workspace-project persp) "<none>")
                 (length buffers)
                 (dk-workspace-session-debug--workspace-buffer-names persp))))
            (when orphaned
              (dk-workspace-session-debug--log
               "ORPHANED buffer-count=%d buffers=%S"
               (length orphaned)
               (mapcar #'dk-workspace-session-debug--buffer-label orphaned)))
            (dk-workspace-session-debug--log-window-mismatches)
            (dk-workspace-session-debug--log-index-mismatches)
            (dk-workspace-session-debug--log "SNAPSHOT-END event=%s" event))
        (error
         (dk-workspace-session-debug--log
          "SNAPSHOT-ERROR event=%s error=%S" event err))))))

(defun dk-workspace-session-debug-schedule (&optional event)
  "Schedule follow-up snapshots for EVENT."
  (when dk-workspace-session-debug
    (dolist (delay dk-workspace-session-debug-idle-delays)
      (run-with-idle-timer
       delay nil
       #'dk-workspace-session-debug-snapshot
       (format "%s+%ss" (or event "scheduled") delay)))))

(defun dk-workspace-session-debug--session-file (file)
  "Return the effective session FILE, if it can be resolved."
  (ignore-errors
    (expand-file-name
     (or file
         (and (fboundp 'doom-session-file)
              (doom-session-file))))))

(defun dk-workspace-session-debug--session-names (file)
  "Return perspective names stored in session FILE."
  (when (and file
             (file-readable-p file)
             (fboundp 'persp-list-persp-names-in-file))
    (ignore-errors (persp-list-persp-names-in-file file))))

(defun dk-workspace-session-debug--doom-load-session-a (fn &optional file)
  "Log diagnostics around `doom-load-session'."
  (let* ((session-file (dk-workspace-session-debug--session-file file))
         (session-names (dk-workspace-session-debug--session-names session-file)))
    (dk-workspace-session-debug--log
     "doom-load-session BEFORE arg=%S file=%s readable=%S saved-workspaces=%S"
     file
     (or session-file "<unknown>")
     (and session-file (file-readable-p session-file))
     session-names)
    (dk-workspace-session-debug-snapshot "before-doom-load-session")
    (prog1 (funcall fn file)
      (dk-workspace-session-debug--log
       "doom-load-session AFTER file=%s"
       (or session-file "<unknown>"))
      (dk-workspace-session-debug-snapshot "after-doom-load-session")
      (dk-workspace-session-debug-schedule "after-doom-load-session"))))

(defun dk-workspace-session-debug--quickload-session-a (fn &optional force)
  "Log diagnostics around `doom/quickload-session'."
  (dk-workspace-session-debug--log
   "doom/quickload-session BEFORE force=%S" force)
  (prog1 (funcall fn force)
    (dk-workspace-session-debug--log
     "doom/quickload-session AFTER force=%S" force)))

(defun dk-workspace-session-debug--persp-load-state-a (fn &rest args)
  "Log diagnostics around `persp-load-state-from-file'."
  (let* ((file (dk-workspace-session-debug--session-file (car args)))
         (names (dk-workspace-session-debug--session-names file))
         (dk-workspace-session-debug--load-depth
          (1+ dk-workspace-session-debug--load-depth)))
    (dk-workspace-session-debug--log
     "persp-load-state-from-file BEFORE file=%s args=%S saved-workspaces=%S"
     (or file "<unknown>")
     args
     names)
    (dk-workspace-session-debug-snapshot "before-persp-load-state")
    (prog1 (apply fn args)
      (dk-workspace-session-debug--log
       "persp-load-state-from-file AFTER file=%s"
       (or file "<unknown>"))
      (dk-workspace-session-debug-snapshot "after-persp-load-state"))))

(defun dk-workspace-session-debug--persp-add-buffer-a (fn &rest args)
  "Log buffer additions while a session is being restored."
  (let* ((persp (or (cadr args)
                    (dk-workspace-session-debug--current-persp)))
         (filtered (dk-workspace-session-debug--filter-foreign-add-buffers
                    (car args) persp))
         (args (if (eq filtered (car args))
                   args
                 (cons filtered (cdr args))))
         (result (when (or filtered (null (car args)))
                   (apply fn args))))
    (when (> dk-workspace-session-debug--load-depth 0)
      (dk-workspace-session-debug--log
       "RESTORE-ADD-BUFFER workspace=%s input=%S result=%S"
       (dk-workspace-session-debug--persp-name persp)
       (car args)
       result))
    result))

(defun dk-workspace-session-debug--after-load-state-h (file phash persp-names)
  "Log after persp-mode has loaded perspectives from FILE."
  (dk-workspace-session-debug--log
   "persp-after-load-state file=%s phash-current=%S loaded=%S"
   file
   (and (boundp '*persp-hash*) (eq phash *persp-hash*))
   persp-names)
  (dk-workspace-session-debug-clean-foreign-project-buffers)
  (dk-workspace-session-debug-snapshot "persp-after-load-state")
  (dk-workspace-session-debug-schedule "persp-after-load-state"))

(defun dk-workspace-session-debug--activated-h (&rest args)
  "Log after a perspective is activated."
  (dk-workspace-session-debug--log
   "persp-activated args=%S current=%s selected-buffer={%s}"
   args
   (dk-workspace-session-debug--persp-name
    (dk-workspace-session-debug--current-persp))
   (dk-workspace-session-debug--buffer-label
    (dk-workspace-session-debug--selected-window-buffer)))
  (dk-workspace-session-debug--fix-window-mismatches)
  (dk-workspace-session-debug-clean-foreign-project-buffers)
  (dk-workspace-session-debug-schedule-fix "persp-activated")
  (dk-workspace-session-debug--log
   "persp-activated-post-fix current=%s selected-buffer={%s}"
   (dk-workspace-session-debug--persp-name
    (dk-workspace-session-debug--current-persp))
   (dk-workspace-session-debug--buffer-label
    (dk-workspace-session-debug--selected-window-buffer)))
  (dk-workspace-session-debug--log-window-mismatches))

(defun dk-workspace-session-debug--created-h (persp phash)
  "Log after PERSP has been created."
  (dk-workspace-session-debug--log
   "persp-created name=%s phash-current=%S"
   (dk-workspace-session-debug--persp-name persp)
   (and (boundp '*persp-hash*) (eq phash *persp-hash*))))

(after! persp-mode
  (advice-remove #'persp-load-state-from-file
                 #'dk-workspace-session-debug--persp-load-state-a)
  (advice-add #'persp-load-state-from-file
              :around #'dk-workspace-session-debug--persp-load-state-a)
  (advice-remove #'persp-add-buffer
                 #'dk-workspace-session-debug--persp-add-buffer-a)
  (advice-add #'persp-add-buffer
              :around #'dk-workspace-session-debug--persp-add-buffer-a)
  (add-hook 'persp-after-load-state-functions
            #'dk-workspace-session-debug--after-load-state-h)
  (add-hook 'persp-activated-functions
            #'dk-workspace-session-debug--activated-h)
  (add-hook 'persp-created-functions
            #'dk-workspace-session-debug--created-h))

(after! doom-lib
  (when (fboundp 'doom-load-session)
    (advice-remove #'doom-load-session
                   #'dk-workspace-session-debug--doom-load-session-a)
    (advice-add #'doom-load-session
                :around #'dk-workspace-session-debug--doom-load-session-a))
  (when (fboundp 'doom/quickload-session)
    (advice-remove #'doom/quickload-session
                   #'dk-workspace-session-debug--quickload-session-a)
    (advice-add #'doom/quickload-session
                :around #'dk-workspace-session-debug--quickload-session-a)))
