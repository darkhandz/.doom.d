;;; modules/lsp.el -*- lexical-binding: t; -*-

(after! lsp-mode
  (setq lsp-headerline-breadcrumb-enable t
        lsp-headerline-breadcrumb-icons-enable nil
        lsp-headerline-breadcrumb-enable-diagnostics nil
        lsp-ui-sideline-show-hover t
        lsp-lens-enable nil
        lsp-file-watch-threshold 5000))

(after! ccls
  (setq ccls-initialization-options
        '(:index (:comments 2)
          :completion (:detailedLabel t)))
  (set-lsp-priority! 'ccls 2))

(defun dk-forward-defun ()
  "Jump to the beginning of the next defun."
  (interactive)
  (beginning-of-defun -1))

(defun dk-backward-defun ()
  "Jump to the beginning of the previous defun."
  (interactive)
  (beginning-of-defun 1))

(defmacro dk-structural-nav-bindings-for-map! (keymap)
  "Install structural navigation bindings into KEYMAP."
  `(map! :map ,keymap
         :n "C-h" (cmd! (beginning-of-defun))
         :n "C-j" #'dk-forward-defun
         :n "C-k" #'dk-backward-defun
         :n "C-l" (cmd! (end-of-defun))))

(after! c-ts-mode
  (dk-structural-nav-bindings-for-map! c-ts-mode-map))

(after! c++-ts-mode
  (dk-structural-nav-bindings-for-map! c++-ts-mode-map))

(after! python-ts-mode
  (dk-structural-nav-bindings-for-map! python-ts-mode-map))

(after! js
  (when (boundp 'js-ts-mode-map)
    (dk-structural-nav-bindings-for-map! js-ts-mode-map)))

(after! typescript-ts-mode
  (dk-structural-nav-bindings-for-map! typescript-ts-mode-map)
  (when (boundp 'tsx-ts-mode-map)
    (dk-structural-nav-bindings-for-map! tsx-ts-mode-map)))

(after! css-mode
  (when (boundp 'css-ts-mode-map)
    (dk-structural-nav-bindings-for-map! css-ts-mode-map)))

(after! sh-script
  (when (boundp 'bash-ts-mode-map)
    (dk-structural-nav-bindings-for-map! bash-ts-mode-map)))

(after! json-ts-mode
  (dk-structural-nav-bindings-for-map! json-ts-mode-map))

(after! yaml-ts-mode
  (dk-structural-nav-bindings-for-map! yaml-ts-mode-map))

(after! toml-ts-mode
  (dk-structural-nav-bindings-for-map! toml-ts-mode-map))

(defun dk-format-hex-padded (num)
  "Format NUM as an even-length lowercase hexadecimal string."
  (let* ((hex (format "%x" num))
         (len (length hex)))
    (if (cl-oddp len)
        (concat "0" hex)
      hex)))

(defun dk-hex-to-decimal (hex-str)
  "Convert HEX-STR to a decimal number."
  (string-to-number (replace-regexp-in-string "^0[xX]" "" hex-str) 16))

(defun dk-eldoc-add-hex-value (result)
  "Add hex/decimal conversions to supported eldoc RESULT strings."
  (when (stringp result)
    (unless (string-match-p "([0-9xXa-fA-F]+)" result)
      (setq result
            (replace-regexp-in-string
             "\\b\\([A-Za-z_][A-Za-z0-9_]*\\)\\s-*=\\s-*\\(0[xX][0-9a-fA-F]+\\)\\s-*$"
             (lambda (match)
               (let* ((name (match-string 1 match))
                      (hex-str (match-string 2 match))
                      (num (dk-hex-to-decimal hex-str)))
                 (format "%s = %s (%d)" name hex-str num)))
             result))
      (setq result
            (replace-regexp-in-string
             "#define\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)\\s-+\\(0[xX][0-9a-fA-F]+\\)\\s-*$"
             (lambda (match)
               (let* ((name (match-string 1 match))
                      (hex-str (match-string 2 match))
                      (num (dk-hex-to-decimal hex-str)))
                 (format "#define %s %s (%d)" name hex-str num)))
             result))
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
                             (dk-format-hex-padded abs-num))
                   match)))
             result))
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
                             (dk-format-hex-padded abs-num))
                   match)))
             result))))
  result)

(after! lsp-mode
  (advice-add #'lsp--render-element :filter-return #'dk-eldoc-add-hex-value))
