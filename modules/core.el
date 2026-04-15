;;; modules/core.el -*- lexical-binding: t; -*-

;; Identity
(setq user-full-name "darkhandz"
      user-mail-address "darkhandz0@gmail.com")

;; Appearance
(setq doom-font (font-spec :family "Maple Mono NF CN" :size 18 :weight 'semi-bold)
      doom-symbol-font (font-spec :family "Maple Mono NF CN")
      doom-theme 'doom-one-light
      doom-themes-enable-italic nil
      display-line-numbers-type t
      fancy-splash-image (expand-file-name "assets/doom-emacs-gray.svg" doom-user-dir))

;; Org
(setq org-directory "~/org/")

;; Restore last frame size/position on startup, save on exit.
(defvar dk-frame-geometry-file
  (expand-file-name "frame-geometry.el" doom-cache-dir))

(defun dk-save-frame-geometry ()
  "Persist current GUI frame position/size."
  (when (display-graphic-p)
    (with-temp-file dk-frame-geometry-file
      (let ((frame (selected-frame)))
        (prin1
         `((top . ,(frame-parameter frame 'top))
           (left . ,(frame-parameter frame 'left))
           (width . ,(frame-parameter frame 'width))
           (height . ,(frame-parameter frame 'height))
           (fullscreen . ,(frame-parameter frame 'fullscreen)))
         (current-buffer))))))

(defun dk-restore-frame-geometry ()
  "Restore GUI frame position/size from last session."
  (when (and (display-graphic-p)
             (file-exists-p dk-frame-geometry-file))
    (with-temp-buffer
      (insert-file-contents dk-frame-geometry-file)
      (let ((params (read (current-buffer))))
        (modify-frame-parameters (selected-frame) params)))))

(add-hook 'kill-emacs-hook #'dk-save-frame-geometry)
(add-hook 'window-setup-hook #'dk-restore-frame-geometry)

;; Scroll one line at a time (less jumpy than defaults).
(setq mouse-wheel-scroll-amount '(4 ((shift) . 8))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t
      mouse-wheel-tilt-scroll t)
