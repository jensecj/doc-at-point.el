;;; dokumat-python.el --- register a backend for `python'. -*- lexical-binding: t; -*-

(require 'dokumat)

;;;###autoload
(defun dokumat-python (symbol)
  "Return documentation for python symbol."
  (ignore-errors
    (setq doc (elpy-rpc-get-docstring))

    (when (not doc)
      (setq doc (elpy-rpc-get-pydoc-documentation symbol)))

    doc)

  (if doc doc
    (format "No documentation found for %s" symbol)))

;; register the default python handler for dokumat
(dokumat-register
  :id "python elpy backend"
  :modes 'python-mode
  :symbol-fn #'elpy-doc--symbol-at-point
  :doc-fn #'dokumat-python
  :should-run-p (lambda () (bound-and-true-p elpy-mode))
  :order 99)

(provide 'dokumat-python)
