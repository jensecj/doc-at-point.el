;;; dokumat.el --- documentation for the symbol-at-point. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jens Christian Jensen

;; Author: Jens Christian Jensen <jensecj@gmail.com>
;; URL: http://github.com/jensecj/dokumat.el
;; Keywords: documentation, help
;; Package-Version: 20190303
;; Version: 0.4.2
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
;; `dokumat-register', which takes a mode (or a list of modes) where that
;; backend should be active, and two functions `symbol-fn', and `doc-fn', which
;; recognize the symbol-at-point, and fetch the documentation-string for that
;; symbol, respectively.

;;; Code:

(require 'dash)
(require 'ht)
(require 's)
(require 'subr-x)

(require 'dokumat-extra)

(defvar dokumat-backends (ht)
  "Map from mode-symbol to backend-property-map.

KEY is the mode registered with a backend.
VALUE is a map with keys :id, :symbol-fn, :doc-fn, :should-run-p, :order.

:ID = string: identifier for the backend

:SYMBOL-FN = () -> symbol: function that returns the symbol at point.

:DOC-FN = symbol -> string: function that returns documentation for a symbol.

:SHOULD-RUN-P = nil | t | (() -> nil | t): predicate for whether
the backend should be used, can be a symbol or a function.

:ORDER = number: The ordering of the backend.  Backends are
checked for if they should run from lowest order to highest.")

(defun dokumat--display-with-message (doc-string)
  "Show DOC-STRING in the minibuffer."
  (message doc-string))

(defun dokumat--display-with-buffer (doc-string)
  "Show DOC-STRING in a buffer in another window."
  (let ((buf (get-buffer-create "*dokumat-documentation*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert doc-string)
      (goto-char (point-min))
      (view-buffer-other-window (current-buffer)))))

(defvar dokumat-display-fn #'dokumat--display-with-buffer
  "Function used to display documentation.")

(defun dokumat--get-id (backend) (ht-get backend :id))
(defun dokumat--get-modes (backend) (ht-get backend :modes))
(defun dokumat--get-symbol-fn (backend) (ht-get backend :symbol-fn))
(defun dokumat--get-doc-fn (backend) (ht-get backend :doc-fn))
(defun dokumat--get-should-run-p (backend) (ht-get backend :should-run-p))
(defun dokumat--get-order (backend) (ht-get backend :order))
(defun dokumat--get-backends (mode) (ht-get dokumat-backends mode))

(defun dokumat--build-backend (id modes symbol-fn doc-fn should-run-p order)
  "Build a backend from arguments."
  (let ((backend (ht
                  (:id id)
                  (:modes modes)
                  (:symbol-fn symbol-fn)
                  (:doc-fn doc-fn)
                  (:should-run-p should-run-p)
                  (:order order))))
    backend))

(defun dokumat--valid-backend-p (backend)
  "Check if BACKEND is valid."
  (let ((id (dokumat--get-id backend))
        (modes (dokumat--get-modes backend))
        (symbol-fn (dokumat--get-symbol-fn backend))
        (doc-fn (dokumat--get-doc-fn backend))
        (should-run-p (dokumat--get-should-run-p backend))
        (order (dokumat--get-order backend)))
    (cond
     ((not id)
      (error "Invalid dokumat backend, missing key :id"))
     ((not (stringp id))
      (error "Invalid dokumat backend, :id should be a string, got %s: %s"
             (type-of id) id))

     ((not modes)
      (error "Invalid dokumat backend, missing key :modes %s" modes))
     ((not (or (symbolp modes) (listp modes)))
      (error "Invalid dokumat backend, :modes should be a symbol or list of symbols, got %s: %s"
             (type-of modes) modes))

     ((not symbol-fn)
      (error "Invalid dokumat backend, missing key :symbol-fn"))
     ((not (or (functionp symbol-fn) (symbolp symbol-fn)))
      (error "Invalid dokumat backend, :symbol-fn should be a function, got a %s: %s"
             (type-of symbol-fn) symbol-fn))

     ((not doc-fn)
      (error "Invalid dokumat backend, missing key :doc-fn"))
     ((not (or (functionp doc-fn) (symbolp doc-fn)))
      (error "Invalid dokumat backend, :doc-fn should be a function, got %s: %s"
             (type-of doc-fn) doc-fn))

     ((not should-run-p)
      (error "Invalid dokumat backend, missing key :should-run-p"))
     ((not (or (functionp should-run-p) (symbolp should-run-p)))
      (error "Invalid dokumat backend, :should-run-p should be a symbol or a function, got %s: %s"
             (type-of should-run-p) should-run-p))

     ((not order)
      (error "Invalid dokumat backend, missing key :order"))
     ((not (numberp order))
      (error "Invalid dokumat backend, :order should be a number, got %s: %s"
             (type-of order) order))
     (t backend))))

(defun dokumat--should-run (sym-or-fn)
  "Return whether or not SYM-OR-FN indicates if it should or not."
  (if (functionp sym-or-fn)
      (funcall sym-or-fn)
    sym-or-fn))

(defun dokumat--backend-exists-p (mode id)
  "Check if a backend with ID exists for MODE."
  (let* ((registered-backends (dokumat--get-backends mode))
         (ids (-map
               (lambda (b) (dokumat--get-id b))
               registered-backends)))
    (member id ids)))

(defun dokumat--sort-backend-predicate (a b)
  "Predicate for sorting backends by order."
  (< (dokumat--get-order a)
     (dokumat--get-order b)))

(defun dokumat--add-backend (mode backend)
  "Add a new documentation BACKEND for MODE."
  (cond
   ((not (dokumat--valid-backend-p backend))
    (message "Invalid backend: %s" backend))
   ((dokumat--backend-exists-p mode (dokumat--get-id backend))
    (message "A backend with id '%s' already exists for %s"
             (dokumat--get-id backend) mode))
   (t
    (let ((backends (dokumat--get-backends mode)))
      (ht-set dokumat-backends mode
              ;; make sure that the backends are sorted, so when we're iterating
              ;; over them later `car' is always the backend with the lowest
              ;; order
              (sort (cons backend backends)
                    #'dokumat--sort-backend-predicate))))))

(defun dokumat--remove-backend (mode id)
  "Remove a documentation backend with ID from MODE."
  (if (dokumat--backend-exists-p mode id)
      (let* ((backends (ht-get dokumat-backends mode))
             (filtered (-remove
                        (lambda (e) (string= id (ht-get e :id)))
                        backends)))
        (ht-set dokumat-backends mode filtered))
    (message "backend `%s-%s' does not exist." mode id)))

(defun dokumat--suitable-backend (mode)
  "Try to find a suitable documentation backend to use for MODE."
  (when-let ((backends (dokumat--get-backends mode)))
    (let* ((backend (-first
                     (lambda (b)
                       (dokumat--should-run
                        (dokumat--get-should-run-p b)))
                     backends)))
      (when backend
        backend))))

(defun dokumat--with-backend (backend &optional sym)
  "Lookup documentation using BACKEND.

Optionally lookup documentation for SYM using BACKEND."
  (let* ((symbol-fn (dokumat--get-symbol-fn backend))
         (doc-fn (dokumat--get-doc-fn backend))
         (sym (if sym sym (funcall symbol-fn)))
         (doc (funcall doc-fn sym)))
    (if doc
        (funcall dokumat-display-fn doc)
      (message "No documentation found for %s" sym))))

;;;###autoload
(cl-defun dokumat-register (&key id modes symbol-fn doc-fn (should-run-p t) (order 1))
  "Register a new documentation backend."
  (declare (indent defun))
  (let ((backend (dokumat--build-backend id modes symbol-fn doc-fn should-run-p order)))
    (when (dokumat--valid-backend-p backend)
      (cond
       ((symbolp modes)
        (dokumat--add-backend modes backend))
       ((listp modes)
        (-map
         (lambda (m) (dokumat--add-backend m backend))
         modes))))))

;;;###autoload
(defun dokumat-other-buffer (&optional sym)
  "Show documentation for the symbol at point in another buffer.

Optionally show documentation for SYM."
  (interactive)
  (let ((dokumat-display-fn #'dokumat--display-with-buffer))
    (funcall #'dokumat sym)))

;;;###autoload
(defun dokumat-echo-area (&optional sym)
  "Show documentation for the symbol at point in echo area.

Optionally show documentation for SYM."
  (interactive)
  (let ((dokumat-display-fn #'dokumat--display-with-message))
    (funcall #'dokumat sym)))

;;;###autoload
(defun dokumat (&optional sym)
  "Show documentation for the symbol at point, based on relevant backend.

Optionally show documentation for SYM."
  (interactive)
  (let* ((current-mode major-mode)
         (backend (dokumat--suitable-backend current-mode)))
    (if backend
        (if sym
            (dokumat--with-backend backend sym)
          (dokumat--with-backend backend))
      (message "No dokumat backend for %s" current-mode))))

;;;###autoload
(defun dokumat-setup-defaults ()
  "Setup default handlers for dokumat."
  (interactive)
  ;; set default keybinding if the key is not already used
  (let ((default-key (kbd "C-+")))
    (unless (or (local-key-binding default-key)
                (global-key-binding default-key))
      (global-set-key default-key #'dokumat))

    (when (boundp 'company-mode)
      (define-key company-active-map default-key #'doc-at-point-company-menu-selection-quickhelp)))

  (require 'dokumat-elisp)
  (require 'dokumat-python)
  (require 'dokumat-rust))

;; TODO: add backend for `lsp' / `eglot'

(provide 'dokumat)
;;; dokumat.el ends here
