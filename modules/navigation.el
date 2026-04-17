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

(map! :n "C-." #'+vc-gutter/next-hunk
      :n "C-," #'+vc-gutter/previous-hunk)

(map! (:leader
       :desc "vc next hunk" "]" #'+vc-gutter/next-hunk
       :desc "vc next hunk" "\\" #'+vc-gutter/next-hunk
       :desc "vc prev hunk" "[" #'+vc-gutter/previous-hunk))

(map! (:leader
       :desc "imenu" "e" #'dk/treesit-counsel-imenu))

(defun dk/imenu-function-icon ()
  "Return the icon prefix used for function Imenu entries."
  (or (when (require 'nerd-icons nil t)
        (cond
         ((fboundp 'nerd-icons-codicon)
          (concat (nerd-icons-codicon "nf-cod-symbol_method") " "))
         ((fboundp 'nerd-icons-mdicon)
         (concat (nerd-icons-mdicon "nf-md-function") " "))))
      "ƒ "))

(defun dk/treesit-node-field-text (node field)
  "Return NODE's FIELD text, or nil if FIELD doesn't exist."
  (when-let ((child (treesit-node-child-by-field-name node field)))
    (treesit-node-text child t)))

(defun dk/treesit-function-name (node)
  "Return the function name for NODE."
  (or (treesit-defun-name node)
      (dk/treesit-node-field-text node "name")
      "Anonymous"))

(defun dk/treesit-function-signature (node)
  "Return a one-line signature string for function NODE."
  (let* ((body (treesit-node-child-by-field-name node "body"))
         (start (treesit-node-start node))
         (end (if body (treesit-node-start body) (treesit-node-end node)))
         (text (string-trim
                (replace-regexp-in-string
                 "[ \t\n\r]+"
                 " "
                 (buffer-substring-no-properties start end)))))
    (replace-regexp-in-string "[[:space:]]*\\'" "" text)))

(defun dk/imenu-function-label (node)
  "Return a propertized Imenu label for function NODE."
  (let* ((icon (dk/imenu-function-icon))
         (name (dk/treesit-function-name node))
         (signature (dk/treesit-function-signature node))
         (case-fold-search nil)
         (escaped (regexp-quote name)))
    (if (or (string-empty-p signature)
            (not (string-match (format "\\_<%s\\_>" escaped) signature)))
        (concat icon signature)
      (let ((prefix (substring signature 0 (match-beginning 0)))
            (match (match-string 0 signature))
            (suffix (substring signature (match-end 0))))
        (concat
         icon
         (propertize prefix 'face 'shadow)
         (propertize match 'face 'font-lock-function-name-face)
         (propertize suffix 'face 'shadow))))))

(defun dk/treesit-function-node-regexp ()
  "Return a regexp matching function-like tree-sitter nodes for this mode."
  (cond
   ((derived-mode-p 'c-ts-base-mode)
    (rx bos (or "function_definition" "preproc_function_def") eos))
   ((derived-mode-p 'python-ts-mode)
    (rx bos "function_definition" eos))
   ((derived-mode-p 'js-ts-mode 'typescript-ts-base-mode 'tsx-ts-mode)
    (rx bos (or "function_declaration" "method_definition") eos))
   ((derived-mode-p 'bash-ts-mode)
    (rx bos "function_definition" eos))))

(defun dk/treesit-function-imenu--flatten (item)
  "Flatten one Imenu ITEM subtree into a plain function list."
  (require 'imenu)
  (cond
   ((null item) nil)
   ((and (imenu--subalist-p item)
         (stringp (car item)))
    (let ((name (car item))
          (jump (cadr item))
          (children (cddr item))
          entries)
      (when (and (consp jump) (or (markerp (cdr jump)) (integer-or-marker-p (cdr jump))))
        (push (cons name (cdr jump)) entries))
      (dolist (child children)
        (setq entries (nconc entries (dk/treesit-function-imenu--flatten child))))
      entries))
   ((and (consp item)
         (stringp (car item))
         (or (markerp (cdr item)) (integer-or-marker-p (cdr item))))
    (list item))
   (t nil)))

(defun dk/treesit-function-imenu-create-index ()
  "Return a flat Imenu index containing only functions from tree-sitter."
  (when-let* ((regexp (dk/treesit-function-node-regexp))
              (root (treesit-buffer-root-node))
              (tree (treesit-induce-sparse-tree root regexp)))
    (mapcar (lambda (item)
              (let* ((marker (cdr item))
                     (node (treesit-defun-at-point)))
                (save-excursion
                  (goto-char marker)
                  (setq node (treesit-defun-at-point)))
                (cons (if node
                          (dk/imenu-function-label node)
                        (concat (dk/imenu-function-icon) (car item)))
                      marker)))
            (mapcan #'dk/treesit-function-imenu--flatten
                    (treesit--simple-imenu-1 tree nil #'treesit-defun-name)))))

(defun dk/treesit-imenu-create-index-function ()
  "Return a tree-sitter backed Imenu builder for the current buffer, or nil."
  (when (and (featurep 'treesit)
             (treesit-parser-list))
    (or (and (dk/treesit-function-node-regexp)
             (fboundp 'treesit-induce-sparse-tree)
             (fboundp 'treesit--simple-imenu-1)
             #'dk/treesit-function-imenu-create-index)
        (and (boundp 'treesit-simple-imenu-settings)
             treesit-simple-imenu-settings
             (fboundp 'treesit-simple-imenu)
             #'treesit-simple-imenu))))

(defun dk/treesit-imenu-available-p ()
  "Return non-nil when the current buffer can build Imenu from tree-sitter."
  (not (null (dk/treesit-imenu-create-index-function))))

(defun dk/treesit-counsel-imenu ()
  "Open `counsel-imenu' with a tree-sitter index when available."
  (interactive)
  (require 'counsel)
  (if (not (dk/treesit-imenu-available-p))
      (call-interactively #'counsel-imenu)
    (let ((imenu-create-index-function (dk/treesit-imenu-create-index-function)))
      (call-interactively #'counsel-imenu))))

;; Avy is more efficient than evil-snipe s/S.
(after! (evil avy)
  (evil-define-key '(normal visual) evil-snipe-local-mode-map
    (kbd "S") #'evil-avy-goto-char-in-line
    (kbd "s") #'evil-avy-goto-char)
  (define-key evil-visual-state-map (kbd "S") #'evil-avy-goto-char-in-line)
  (define-key evil-normal-state-map (kbd "s") #'evil-avy-goto-char))
