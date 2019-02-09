;;; doc-at-point.el --- documentation for the symbol-at-point. -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Jens Christian Jensen

;; Author: Jens Christian Jensen <jensecj@gmail.com>
;; Keywords: doc-at-point
;; Package-Version: 20190209
;; Version: 0.2

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
;; `doc-at-point-register', which takes two functions `symbol-fn', and `doc-fn',
;; which recognize the symbol-at-point, and fetch the documentation-string for
;; that symbol, respectively.

;;; Code:

(require 'map)

(defcustom doc-at-point-alist '()
  "Alist of plists, containing metadata for documentation."
  :group 'doc-at-point
  :type 'alist)

(defcustom doc-at-point-display-fn (if (fboundp 'popup-tip)
                                       #'doc-at-point--display-fn-popup-tip
                                     #'message)
  "Function used to display documentation."
  :type 'function
  :group 'doc-at-point)

(defun doc-at-point--display-fn-popup-tip (doc-string)
  "Shows `doc-string' in a popup tooltip."
  (popup-tip doc-string :margin-left 1 :margin-right 1))

(defun doc-at-point--should-run-p (should-run-sym-or-fn)
  "Return whether or not SHOULD-RUN-FUNCTION indicates if
should or not."
  (if (functionp should-run-sym-or-fn)
      (funcall should-run-sym-or-fn)
    should-run-sym-or-fn))

(defun doc-at-point--with-entry (entry)
  "Lookup documentation using `entry'."
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
