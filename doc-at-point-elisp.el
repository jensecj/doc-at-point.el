;;; doc-at-point-elisp.el --- register a backend for `elisp'. -*- lexical-binding: t; -*-

(require 'help)
(require 'help-fns)

(require 's)
(require 'dash)

(require 'doc-at-point)

(defmacro doc-at-point-elisp--capture-to-string (&rest body)
  "Capture output written to `standard-output' (help functions,
etc.), and return it as a string."
  `(with-temp-buffer
     (let ((standard-output (current-buffer))
           (help-xref-following t))
       (setq major-mode 'help-mode)
       (progn ,@body)
       (buffer-string))))

(defun doc-at-point-elisp--arglist (fn doc)
  "Return the arglist for FN, extracted from documentation and
function analysis."
  (pcase-let* ((`(,real-function ,def ,_aliased ,real-def)
                (help-fns--analyze-function fn)))
    (doc-at-point-elisp--capture-to-string
     (help-fns--signature fn doc real-def real-function nil)
     ;; fix for symbols having special characters, like questions marks.
     (replace-regexp "\\\\=\\\\" "" nil (point-min) (point-max)))))

(defun doc-at-point-elisp--describe-function (fn)
  "Return description of FN."
  (let* ((doc (documentation fn 'RAW))
         (arglist (doc-at-point-elisp--arglist fn doc))
         (source (find-lisp-object-file-name fn nil)))
    (doc-at-point-elisp--capture-to-string
     (insert arglist)

     (emacs-lisp-mode)
     (font-lock-fontify-buffer)

     (insert "\n\n")

     (let ((p (point)))
       (cond
        ((not doc) (insert "this function is not documented."))
        ((s-blank-str? doc) (insert "this function is not documented."))
        (t
         ;; don't print superfluous arglist, we've already printed one.
         (let* ((str (replace-regexp-in-string "\n\n(fn.*)" "" doc))
                ;; we do some juggling here, inserting a comment-placeholder,
                ;; fontifying the documentation, and removing the placeholder again to
                ;; get the proper fontification for documentation.
                (placeholder ";;;   ")
                (lines (s-lines str))
                (commented-lines (-map #'(lambda (l) (s-prepend placeholder l)) lines))
                (result (s-join "\n" commented-lines)))
           (insert result)
           (font-lock-fontify-region p (point-max))
           (replace-regexp placeholder "" nil p (point))))))

     (goto-char (point-max))

     (when source
       (insert "\n\n")
       (insert (format "defined in %s" source))))))

(defun doc-at-point-elisp--describe-variable (symbol)
  "Return documentation for elisp variable."
  (doc-at-point-elisp--capture-to-string
   (let ((file-name  (find-lisp-object-file-name symbol 'defvar)))
     (princ symbol)
     (princ " is a variable")
     (when file-name
       (princ " defined in `")
       (princ (if (eq file-name 'C-source)
                  "C source code"
                (file-name-nondirectory file-name)))
       (princ "'."))
     (princ "\n\n")
     (princ (or (documentation-property symbol 'variable-documentation t)
                "no documentation.")))))

(defun doc-at-point-elisp--describe-face (symbol)
  "Return documentation for elisp face."
  (doc-at-point-elisp--capture-to-string
   (let ((file-name  (find-lisp-object-file-name symbol 'defface)))
     (princ symbol)
     (princ " is a face ")
     (when file-name
       (princ " defined in `")
       (princ (if (eq file-name 'C-source)
                  "C source code"
                (file-name-nondirectory file-name)))
       (princ "'."))
     (princ "\n\n")
     (princ (or (documentation-property symbol 'face-documentation t)
                "no documentation.")))))

(defun doc-at-point-elisp--describe-group (symbol)
  "Return documentation for elisp group."
  (doc-at-point-elisp--capture-to-string
   (let ((doc (documentation-property symbol 'group-documentation t)))
     (when doc
       (princ symbol)
       (princ " is a group.\n\n")
       (princ doc)))))

;;;###autoload
(defun doc-at-point-elisp (symbol)
  "Return documentation for elisp symbol."
  (if (stringp symbol)
      (setq symbol (intern-soft symbol)))

  (ignore-errors
    (cond
     ((fboundp symbol) (doc-at-point-elisp--describe-function symbol))
     ((boundp symbol) (doc-at-point-elisp--describe-variable symbol))
     ((facep symbol) (doc-at-point-elisp--describe-face symbol))
     (t (doc-at-point-elisp--describe-group symbol)))))

;; register the default elisp handler for doc-at-point
(doc-at-point-register
 :mode '(emacs-lisp-mode lisp-interaction-mode)
 :symbol-fn #'symbol-at-point
 :doc-fn #'doc-at-point-elisp
 :should-run t)

(provide 'doc-at-point-elisp)
