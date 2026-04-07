;;; modules/navigation.el -*- lexical-binding: t; -*-

;; Map extra mouse buttons to jump between buffers.
(map! :after better-jumper
      :nv [mouse-8] #'better-jumper-jump-backward
      :nv [mouse-9] #'better-jumper-jump-forward)

(map! :leader
      :desc "evil-avy-goto-char-2"
      "j j" #'evil-avy-goto-char-2)

;; Make avy work across all visible windows.
(setq avy-all-windows t
      avy-keys-alist
      `((avy-goto-char . (?a ?s ?d ?f ?g ?q ?w ?e ?r ?u ?i ?o ?p ?c ?v ?n ?m ?h ?j ?k ?l))
        (avy-goto-char-in-line . (?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?u ?i ?o ?p))))

(use-package! winum
  :init
  (setq-default winum-scope 'frame-local)
  (setq winum-auto-setup-mode-line nil)
  (winum-mode))

;; Make Treemacs accessible as window 0.
(after! (treemacs winum)
  (when-let* ((prefix (or (and (boundp 'treemacs-buffer-name-prefix)
                               treemacs-buffer-name-prefix)
                          (and (boundp 'treemacs--buffer-name-prefix)
                               treemacs--buffer-name-prefix))))
    (setq winum-ignored-buffers-regexp
          (delete (regexp-quote (format "%sFramebuffer-" prefix))
                  winum-ignored-buffers-regexp))))

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

(map! (:leader
       :desc "delete other windows" :n "w i" #'delete-other-windows))

(map! (:leader
       :desc "Next buffer" "l" #'next-buffer
       :desc "Previous buffer" "k" #'previous-buffer))

(defun dk-contract-region-maybe ()
  "Contract active region if present; otherwise do nothing."
  (interactive)
  (when (use-region-p)
    (er/contract-region 1)))

(defun dk-recenter-top-plus-5 ()
  "Recenter window so current line is 5th from top."
  (interactive)
  (recenter 5))

(map! :n [backspace] #'er/expand-region
      :v [backspace] #'er/expand-region
      :nv [delete] #'dk-contract-region-maybe
      :n "RET" #'dk-recenter-top-plus-5)

(map! (:leader
       :desc "vc next hunk" "]" #'+vc-gutter/next-hunk
       :desc "vc next hunk" "\\" #'+vc-gutter/next-hunk
       :desc "vc prev hunk" "[" #'+vc-gutter/previous-hunk))

(map! (:leader
       :desc "imenu" "e" #'counsel-imenu))

;; Avy is more efficient than evil-snipe s/S.
(after! (evil avy)
  (evil-define-key '(normal visual) evil-snipe-local-mode-map
    (kbd "S") #'evil-avy-goto-char-in-line
    (kbd "s") #'evil-avy-goto-char)
  (define-key evil-visual-state-map (kbd "S") #'evil-avy-goto-char-in-line)
  (define-key evil-normal-state-map (kbd "s") #'evil-avy-goto-char))
