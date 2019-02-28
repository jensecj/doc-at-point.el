;;; doc-at-point.el --- documentation for the symbol-at-point. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jens Christian Jensen

;; Author: Jens Christian Jensen <jensecj@gmail.com>
;; Keywords: documentation, help
;; Package-Version: 20190228
;; Version: 0.4.0
;; Package-Requires: ((emacs "25.1") (dash "2.14.1") (s "1.12.0") (ht "2.3"))

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

(require 'dash)
(require 'ht)
(require 's)

(defvar doc-at-point-map (ht)
  "Map from mode-symbol to backend-property-map.

KEY is the mode registred with a backend.
VALUE is a map with keys :id, :symbol-fn, :doc-fn, :should-run-p, :order.

:ID = string: identifier for the backend

:SYMBOL-FN = () -> symbol: function that returns the symbol-at-point.

:DOC-FN = symbol -> string: function that returns documentation for a symbol.

:SHOULD-RUN-P = nil | t | (() -> nil | t): predicate for whether
the backend should be used, can be a symbol or a function.

:ORDER = number: The ordering of the backend. Backends are
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
      (goto-char (point-min))
      (view-buffer-other-window (current-buffer)))))

(defvar doc-at-point-display-fn #'doc-at-point--display-with-buffer
  "Function used to display documentation.")

(defun doc-at-point--get-id (backend) (ht-get backend :id))
(defun doc-at-point--get-modes (backend) (ht-get backend :modes))
(defun doc-at-point--get-symbol-fn (backend) (ht-get backend :symbol-fn))
(defun doc-at-point--get-doc-fn (backend) (ht-get backend :doc-fn))
(defun doc-at-point--get-should-run-p (backend) (ht-get backend :should-run-p))
(defun doc-at-point--get-order (backend) (ht-get backend :order))
(defun doc-at-point--get-backends (mode) (ht-get doc-at-point-map mode))

(defun doc-at-point--build-backend (id modes symbol-fn doc-fn should-run-p order)
  "Build a backend structure from arguments."
  (let ((backend (ht
                  (:id id)
                  (:modes modes)
                  (:symbol-fn symbol-fn)
                  (:doc-fn doc-fn)
                  (:should-run-p should-run-p)
                  (:order order))))
    backend))

(defun doc-at-point--valid-backend-p (backend)
  "Checks if a BACKEND is valid."
  (let ((id (doc-at-point--get-id backend))
        (modes (doc-at-point--get-modes backend))
        (symbol-fn (doc-at-point--get-symbol-fn backend))
        (doc-fn (doc-at-point--get-doc-fn backend))
        (should-run-p (doc-at-point--get-should-run-p backend))
        (order (doc-at-point--get-order backend)))
    (cond
     ((not id)
      (error "Invalid doc-at-point backend, missing key :id"))
     ((not (stringp id))
      (error "Invalid doc-at-point backend, :id should be a string, got %s: %s"
             (type-of id) id))

     ((not modes)
      (error "Invalid doc-at-point backend, missing key :modes %s" modes))
     ((not (or (symbolp modes) (listp modes)))
      (error "Invalid doc-at-point backend, :modes should be a symbol or list of symbols, got %s: %s"
             (type-of modes) modes))

     ((not symbol-fn)
      (error "Invalid doc-at-point backend, missing key :symbol-fn"))
     ((not (or (functionp symbol-fn) (symbolp symbol-fn)))
      (error "Invalid doc-at-point backend, :symbol-fn should be a function, got a %s: %s"
             (type-of symbol-fn) symbol-fn))

     ((not doc-fn)
      (error "Invalid doc-at-point backend, missing key :doc-fn"))
     ((not (or (functionp doc-fn) (symbolp doc-fn)))
      (error "Invalid doc-at-point backend, :doc-fn should be a function, got %s: %s"
             (type-of doc-fn) doc-fn))

     ((not should-run-p)
      (error "Invalid doc-at-point backend, missing key :should-run-p"))
     ((not (or (functionp should-run-p) (symbolp should-run-p)))
      (error "Invalid doc-at-point backend, :should-run-p should be a symbol or a function, got %s: %s"
             (type-of should-run-p) should-run-p))

     ((not order)
      (error "Invalid doc-at-point backend, missing key :order"))
     ((not (numberp order))
      (error "Invalid doc-at-point backend, :order should be a number, got %s: %s"
             (type-of order) order))
     (t backend))))

(defun doc-at-point--should-run (sym-or-fn)
  "Return whether or not SYM-OR-FN indicates if it should or not."
  (if (functionp sym-or-fn)
      (funcall sym-or-fn)
    sym-or-fn))

(defun doc-at-point--backend-exists-p (mode id)
  "Checks if a backend with ID exists for MODE."
  (let* ((registered-backends (doc-at-point--get-backends mode))
         (ids (-map
               #'(lambda (b) (doc-at-point--get-id b))
               registered-backends)))
    (member id ids)))

(defun doc-at-point--sort-backend-predicate (a b)
  "Predicate for sorting documentation backends by order."
  (< (doc-at-point--get-order a)
     (doc-at-point--get-order b)))

(defun doc-at-point--add-backend (mode backend)
  "Add a new documentation BACKEND for MODE."
  (cond
   ((not (doc-at-point--valid-backend-p backend))
    (message "Invalid backend: %s" backend))
   ((doc-at-point--backend-exists-p mode (doc-at-point--get-id backend))
    (message
     "A backend with id '%s' already exists for %s"
     (doc-at-point--get-id backend)
     mode))
   (t
    (let ((backends (doc-at-point--get-backends mode)))
      (ht-set doc-at-point-map mode
              ;; make sure that the entries sorted
              (sort (cons backend backends)
                    #'doc-at-point--sort-backend-predicate))))))


(defun doc-at-point--suitable-backend (mode)
  "Try to find a suitable documentation backend to use for MODE."
  (when-let ((backends (doc-at-point--get-backends mode)))
    (let* ((backend (-first
                     #'(lambda (b)
                         (doc-at-point--should-run
                          (doc-at-point--get-should-run-p b)))
                     backends)))
      (when backend
        backend))))

(defun doc-at-point--with-backend (backend)
  "Lookup documentation using BACKEND."
  (let* ((symbol-fn (doc-at-point--get-symbol-fn backend))
         (doc-fn (doc-at-point--get-doc-fn backend))
         (sym (funcall symbol-fn))
         (doc (funcall doc-fn sym)))
    (if doc
        (funcall doc-at-point-display-fn doc)
      (message "No documentation found for %s" sym))))

;;;###autoload
(cl-defun doc-at-point-register (&key id modes symbol-fn doc-fn (should-run-p t) (order 1))
  "Register a new documentation backend."
  (declare (indent defun))
  (let ((backend (doc-at-point--build-backend id modes symbol-fn doc-fn should-run-p order)))
    (when (doc-at-point--valid-backend-p backend)
      (cond
       ((symbolp modes)
        (doc-at-point--add-backend modes backend))
       ((listp modes)
        (-map
         #'(lambda (m) (doc-at-point--add-backend m backend))
         modes))))))

;;;###autoload
(defun doc-at-point ()
  "Show documentation for the symbol at point, based on relevant
backend."
  (interactive)
  (let* ((current-mode major-mode)
         (backend (doc-at-point--suitable-backend current-mode)))
    (if backend
        (doc-at-point--with-backend backend)
      (message "No doc-at-point backend for %s" current-mode))))

;;;###autoload
(defun doc-at-point-setup-defaults ()
  "Setup default handlers for doc-at-point."
  (interactive)
  ;; set default keybinding if the key is not already used
  (let ((default-key (kbd "C-+")))
    (unless (or (local-key-binding default-key)
                (global-key-binding default-key))
      (global-set-key default-key #'doc-at-point))
    (define-key company-active-map default-key #'doc-at-point-company-menu-selection-quickhelp))

  (require 'doc-at-point-extra)
  (require 'doc-at-point-elisp)
  (require 'doc-at-point-python)
  (require 'doc-at-point-rust))

(provide 'doc-at-point)
