;;; dokument-elisp.el --- `Elisp' backend for dokument. -*- lexical-binding: t; -*-

(require 'help)
(require 'help-fns)
(require 'help-fns+ 'noerror)

(require 's)
(require 'dash)

(require 'dokument)

;; TODO: capture directly using analysis, messing with help-buffers is a pain
(defmacro dokument-elisp--capture-to-string (&rest body)
  "Capture output written to `standard-output' (help functions,
etc.), and return it as a string."
  `(with-temp-buffer
     (let ((inhibit-message t)
           (standard-output (current-buffer))
           (help-xref-following t))
       (setq major-mode 'help-mode)
       (progn ,@body)
       (buffer-string))))

(defun dokument-elisp--fontify-as-doc (doc)
  "Fontify a string as if it was a doc-string in
`emacs-lisp-mode'."
  (with-temp-buffer
    ;; we do some juggling here, inserting a comment-placeholder,
    ;; fontifying the documentation, and removing the placeholder again to
    ;; get the proper fontification for documentation.
    (let* ((inhibit-message t)
           (placeholder ";dap;   ")
           (lines (s-lines doc))
           (commented-lines (-map (lambda (l) (s-prepend placeholder l)) lines))
           (result (s-join "\n" commented-lines)))
      (insert result)
      (delay-mode-hooks (emacs-lisp-mode))
      (font-lock-fontify-buffer)

      ;; now the buffer is fontified, remove comment-placeholder
      (with-temp-message ""
        (replace-regexp placeholder "" nil (point-min) (point-max)))

      (buffer-string))))

(defun dokument-elisp--fontify-as-code (code)
  "Fontify a string as if it was elisp in `emacs-lisp-mode'."
  (let ((inhibit-message t))
    (with-temp-buffer
      (insert (if (symbolp code) (symbol-name code) code))
      (delay-mode-hooks (emacs-lisp-mode))
      (when (boundp 'highlight-defined-mode)
        (highlight-defined-mode +1))
      (font-lock-fontify-buffer)
      (buffer-string))))

(defun dokument-elisp--locate-source-file (sym type)
  "Try to locate where a SYM with TYPE is defined."
  (let ((source (find-lisp-object-file-name sym type)))
    (cond
     ((eq source 'C-source) "C source code")
     (source (format "%s" source))
     (t nil))))

(defun dokument-elisp--construct-description (symbol doc source)
  "Construct a description of SYMBOL with DOC and SOURCE."
  (format "%s\n\n%s%s"
          (dokument-elisp--fontify-as-code symbol)
          (dokument-elisp--fontify-as-doc (or doc "no documentation found"))
          (if source (format "\n\ndefined in %s" source) "")))

(defun dokument--analyze-function (fn-sym)
  (pcase-let* ((`(,real-function ,def ,_aliased ,real-def)
                (help-fns--analyze-function fn-sym)))
    '(real-function def aliased real-def)
    )
  )

(defun dokument-elisp--arglist (fn doc)
  "Return the arglist for FN, extracted from documentation and
function analysis."
  (pcase-let* ((`(,real-function ,def ,_aliased ,real-def)
                (help-fns--analyze-function fn)))
    (dokument-elisp--capture-to-string
     (help-fns--signature fn doc real-def real-function nil)
     (with-temp-message ""
       ;; fix for symbols having special characters, like questions marks.
       (replace-regexp "\\\\=\\\\" "" nil (point-min) (point-max))
       ;; remove trailing newline
       (goto-char (point-max))
       (skip-chars-backward "\n")
       (kill-region (point) (point-max))))))

;; FIXME: does not work with aliased functions
(defun dokument-elisp--describe-function (fn)
  "Return description of FN."
  (let* ((raw-doc (documentation fn 'RAW))
         (arglist (dokument-elisp--arglist fn raw-doc))
         (source-file (dokument-elisp--locate-source-file
                       fn (symbol-function fn))))
    (dokument-elisp--construct-description
     arglist
     ;; don't include superfluous arglist, we've already have one.
     (when raw-doc (replace-regexp-in-string "\n\n(fn.*)" "" raw-doc))
     source-file)))

(defun dokument-elisp--describe-variable (symbol)
  "Return documentation for elisp variable."
  (let ((doc (documentation-property symbol 'variable-documentation t))
        (source-file (dokument-elisp--locate-source-file symbol 'defvar)))
    (dokument-elisp--construct-description symbol doc source-file)))

(defun dokument-elisp--describe-face (symbol)
  "Return documentation for elisp face."
  (let ((doc (documentation-property symbol 'face-documentation t))
        (source-file (dokument-elisp--locate-source-file symbol 'defface)))
    (dokument-elisp--construct-description symbol doc source-file)))

(defun dokument-elisp--describe-group (symbol)
  "Return documentation for elisp group."
  (let ((doc (documentation-property symbol 'group-documentation t)))
    (dokument-elisp--construct-description symbol doc nil)))

;;;###autoload
(defun dokument-elisp (symbol)
  "Return documentation for elisp symbol."
  (if (stringp symbol)
      (setq symbol (intern-soft symbol)))
  (ignore-errors
    (cond
     ((fboundp symbol)
      (dokument-elisp--describe-function symbol))
     ((and (boundp symbol) (not (facep symbol)))
      (dokument-elisp--describe-variable symbol))
     ((facep symbol)
      (dokument-elisp--describe-face symbol))
     (t
      (dokument-elisp--describe-group symbol)))))

;; register the default elisp handler for dokument
(dokument-register
  :id "default emacs lisp backend"
  :modes '(emacs-lisp-mode lisp-interaction-mode)
  :symbol-fn #'symbol-at-point
  :doc-fn #'dokument-elisp
  :should-run-p t
  :order 99)

(provide 'dokument-elisp)

;; TODO: truncate documentation if it's too big, make sure that definition string is shown if available.
