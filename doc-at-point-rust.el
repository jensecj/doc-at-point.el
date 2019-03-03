;;; doc-at-point-rust.el --- register a backend for `rust'. -*- lexical-binding: t; -*-

(require 'doc-at-point)

(defun doc-at-point-rust--symbol-at-point ()
  "Return best guess of the rust symbol at point."
  (thing-at-point 'symbol))

;;;###autoload
(defun doc-at-point-rust (symbol)
  "Return documentation for rust symbol."
  (let ((doc-buf (racer--describe symbol)))
    (if doc-buf
        (with-current-buffer doc-buf
          (buffer-string))
      (format "No documentation found for %s" symbol))))

;; register the default rust handler for doc-at-point
(doc-at-point-register
  :id "rust racer backend"
  :modes 'rust-mode
  :symbol-fn #'doc-at-point-rust--symbol-at-point
  :doc-fn #'doc-at-point-rust
  :should-run-p #'(lambda nil (bound-and-true-p racer-mode))
  :order 99)

(provide 'doc-at-point-rust)
