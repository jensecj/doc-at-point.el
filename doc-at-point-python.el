(require 'doc-at-point-core)

;;;###autoload
(defun doc-at-point-python (symbol)
  "Return documentation for python symbol."
  (ignore-errors
    (setq doc (elpy-rpc-get-docstring))

    (when (not doc)
      (setq doc (elpy-rpc-get-pydoc-documentation symbol)))

    doc)

  (if doc doc
    (format "No documentation found for %s" symbol)))

;; register the default python handler for doc-at-point
(doc-at-point-register
 :mode 'python-mode
 :symbol-fn #'elpy-doc--symbol-at-point
 :doc-fn #'doc-at-point-python
 :should-run #'(lambda nil (bound-and-true-p elpy-mode)))

(provide 'doc-at-point-python)
