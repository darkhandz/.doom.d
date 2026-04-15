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
