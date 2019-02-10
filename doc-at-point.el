;;; doc-at-point.el --- documentation for the symbol-at-point. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jens Christian Jensen

;; Author: Jens Christian Jensen <jensecj@gmail.com>
;; Keywords: documentation, help
;; Package-Version: 20190210
;; Version: 0.2.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Simple functionality for showing documentation for the symbol at point.

;; Customizing the information show is done by registering backends using
;; `doc-at-point-register', which takes a mode (or a list of modes) where that
;; backend should be active, and two functions `symbol-fn', and `doc-fn', which
;; recognize the symbol-at-point, and fetch the documentation-string for that
;; symbol, respectively.

;;; Code:

(require 'map)

(defvar doc-at-point-alist '()
  "Alist of plists.
KEY is the mode registred with a backend.

VALUE is a plist of `(:symbol-fn :doc-fn :should-run-p :order)'.

:SYMBOL-FN [() -> symbol]: returns the symbol-at-point.

:DOC-FN [symbol -> string]: returns documentation for a symbol.

:SHOULD-RUN-P [() -> nil | t]: returns whether the backend should be used.

:ORDER [number]: The ordering of the backend. Backends are
checked for if they should run from lowest order to highest. ")

(defun doc-at-point--display-with-message (doc-string)
  "Show DOC-STRING in the minibuffer."
  (message doc-string))

(defun doc-at-point--display-with-buffer (doc-string)
  "Show DOC-STRING in a view-mode buffer in another window."
  (let ((buf (get-buffer-create "*doc-at-point-documentation*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert doc-string)
      (view-buffer-other-window (current-buffer)))))

(defvar doc-at-point-display-fn #'doc-at-point--display-with-buffer
  "Function used to display documentation.")

(with-eval-after-load 'popup
  (defun doc-at-point--display-with-popup (doc-string)
    "Shows DOC-STRING in a `popup.el' tooltip."
    (popup-tip doc-string :margin-left 1 :margin-right 1))

  (setq doc-at-point-display-fn #'doc-at-point--display-with-popup))


(defun doc-at-point--should-run-p (should-run-sym-or-fn)
  "Return whether or not SHOULD-RUN-SYM-OR-FN indicates if
should or not."
  (if (functionp should-run-sym-or-fn)
      (funcall should-run-sym-or-fn)
    should-run-sym-or-fn))

(defun doc-at-point--with-entry (entry)
  "Lookup documentation using ENTRY."
  (let* ((symbol-fn (plist-get entry :symbol-fn))
         (doc-fn (plist-get entry :doc-fn))
         (sym (funcall symbol-fn))
         (doc (funcall doc-fn sym)))
    (if doc
        (funcall doc-at-point-display-fn doc)
      (message "No documentation found for %s" (symbol-name sym)))))

;;;###autoload
(cl-defun doc-at-point-register (&key mode symbol-fn doc-fn (should-run t) (order 1))
  "Register a new documentation backend."
  (let ((entry (list `(
                       :symbol-fn ,symbol-fn
                       :doc-fn ,doc-fn
                       :should-run ,should-run
                       :order ,order))))
    (cond
     ((symbolp mode) (map-put doc-at-point-alist mode entry))
     ((listp mode) (mapcar (lambda (m)
                             (map-put doc-at-point-alist m entry))
                           mode)))))

;;;###autoload
(defun doc-at-point ()
  "Show documentation for the symbol at point, based on relevant
backend."
  (interactive)
  (let* ((current-mode major-mode)
         (entry (car (map-elt doc-at-point-alist current-mode)))
         (should-run (plist-get entry :should-run)))
    (if (and entry (doc-at-point--should-run-p should-run))
        (doc-at-point--with-entry entry)
      (message "No doc-at-point backend for %s" current-mode))))

;;;###autoload
(defun doc-at-point-setup-defaults ()
  "Setup default handlers for doc-at-point."
  (interactive)
  (require 'doc-at-point-elisp)
  (require 'doc-at-point-python)
  (require 'doc-at-point-rust))

(provide 'doc-at-point)
