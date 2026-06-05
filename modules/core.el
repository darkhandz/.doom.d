;;; modules/core.el -*- lexical-binding: t; -*-

;; Identity
(setq user-full-name "darkhandz"
      user-mail-address "darkhandz0@gmail.com")

;; Appearance
(defconst dk-preferred-mono-font "Maple Mono NF CN"
  "Preferred monospace font family.")

(defun dk-font-available-p (family)
  "Return non-nil when font FAMILY is available to Emacs."
  (when (and (stringp family)
             (not (string= family "")))
    (condition-case nil
        (or (member family (font-family-list))
            (find-font (font-spec :family family)))
      (error nil))))

(defun dk-system-mono-font-fallbacks ()
  "Return monospace font fallbacks for the current operating system."
  (cond
   ((eq system-type 'darwin)
    '("SF Mono" "Menlo" "Monaco" "Courier"))
   ((eq system-type 'windows-nt)
    '("Cascadia Mono" "Consolas" "Lucida Console" "Courier New"))
   ((eq system-type 'gnu/linux)
    '("JetBrainsMono Nerd Font Mono"
      "JetBrains Mono"
      "DejaVu Sans Mono"
      "Liberation Mono"
      "Noto Sans Mono"
      "Source Code Pro"
      "monospace"))
   (t
    '("monospace"))))

(defun dk-first-available-font (families)
  "Return the first installed font from FAMILIES, or the last fallback."
  (let ((remaining families)
        font)
    (while (and remaining (not font))
      (when (dk-font-available-p (car remaining))
        (setq font (car remaining)))
      (setq remaining (cdr remaining)))
    (or font (car (last families)))))

(defun dk-mono-font-family ()
  "Return the configured monospace font family with OS-specific fallbacks."
  (dk-first-available-font
   (cons dk-preferred-mono-font (dk-system-mono-font-fallbacks))))

(let ((mono-font-family (dk-mono-font-family)))
  (setq doom-font (font-spec :family mono-font-family :size 18 :weight 'semi-bold)
        doom-symbol-font (font-spec :family mono-font-family)))

(setq doom-theme 'doom-one-light
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
