;;; dokument-rust.el --- register a backend for `rust'. -*- lexical-binding: t; -*-

(require 'dokument)

(defun dokument-rust--symbol-at-point ()
  "Return best guess of the rust symbol at point."
  (thing-at-point 'symbol))

;;;###autoload
(defun dokument-rust (symbol)
  "Return documentation for rust symbol."
  (let ((doc-buf (racer--describe symbol)))
    (if doc-buf
        (with-current-buffer doc-buf
          (buffer-string))
      (format "No documentation found for %s" symbol))))

;; register the default rust handler for dokument
(dokument-register
  :id "rust racer backend"
  :modes 'rust-mode
  :symbol-fn #'dokument-rust--symbol-at-point
  :doc-fn #'dokument-rust
  :should-run-p (lambda nil (bound-and-true-p racer-mode))
  :order 99)

(provide 'dokument-rust)
