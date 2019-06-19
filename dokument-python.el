;;; dokument-python.el --- register a backend for `python'. -*- lexical-binding: t; -*-

(require 'dokument)

;;;###autoload
(defun dokument-python (symbol)
  "Return documentation for python symbol."
  (ignore-errors
    (setq doc (elpy-rpc-get-docstring))

    (when (not doc)
      (setq doc (elpy-rpc-get-pydoc-documentation symbol)))

    doc)

  (if doc doc
    (format "No documentation found for %s" symbol)))

;; register the default python handler for dokument
(dokument-register
  :id "python elpy backend"
  :modes 'python-mode
  :symbol-fn #'elpy-doc--symbol-at-point
  :doc-fn #'dokument-python
  :should-run-p (lambda () (bound-and-true-p elpy-mode))
  :order 99)

(provide 'dokument-python)
