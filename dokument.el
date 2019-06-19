;;; dokument.el --- documentation for the symbol-at-point. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jens Christian Jensen

;; Author: Jens Christian Jensen <jensecj@gmail.com>
;; URL: http://github.com/jensecj/dokument.el
;; Keywords: documentation, help
;; Package-Version: 20190303
;; Version: 0.5.0
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
;; `dokument-register', which takes a mode (or a list of modes) where that
;; backend should be active, and two functions `symbol-fn', and `doc-fn', which
;; recognize the symbol-at-point, and fetch the documentation-string for that
;; symbol, respectively.

;;; Code:

(require 'dash)
(require 'ht)
(require 's)
(require 'subr-x)

(defvar dokument-backends (ht)
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

(defun dokument--display-with-message (doc-string)
  "Show DOC-STRING in the minibuffer."
  (message doc-string))

(defun dokument--display-with-buffer (doc-string)
  "Show DOC-STRING in a buffer in another window."
  ;; TODO: make this for for multiple buffers, kill when closed.
  (let ((buf (get-buffer-create "*dokument-documentation*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert doc-string)
      (goto-char (point-min))
      (view-buffer-other-window (current-buffer)))))

(defvar dokument-display-fn #'dokument--display-with-buffer
  "Function used to display documentation.")

(defun dokument--get-id (backend) (ht-get backend :id))
(defun dokument--get-modes (backend) (ht-get backend :modes))
(defun dokument--get-symbol-fn (backend) (ht-get backend :symbol-fn))
(defun dokument--get-doc-fn (backend) (ht-get backend :doc-fn))
(defun dokument--get-should-run-p (backend) (ht-get backend :should-run-p))
(defun dokument--get-order (backend) (ht-get backend :order))
(defun dokument--get-backends (mode) (ht-get dokument-backends mode))

(defun dokument--build-backend (id modes symbol-fn doc-fn should-run-p order)
  "Build a backend from arguments."
  (let ((backend (ht
                  (:id id)
                  (:modes modes)
                  (:symbol-fn symbol-fn)
                  (:doc-fn doc-fn)
                  (:should-run-p should-run-p)
                  (:order order))))
    backend))

(defun dokument--valid-backend-p (backend)
  "Check if BACKEND is valid."
  (let ((id (dokument--get-id backend))
        (modes (dokument--get-modes backend))
        (symbol-fn (dokument--get-symbol-fn backend))
        (doc-fn (dokument--get-doc-fn backend))
        (should-run-p (dokument--get-should-run-p backend))
        (order (dokument--get-order backend)))
    (cond
     ((not id)
      (error "Invalid dokument backend, missing key :id"))
     ((not (stringp id))
      (error "Invalid dokument backend, :id should be a string, got %s: %s"
             (type-of id) id))

     ((not modes)
      (error "Invalid dokument backend, missing key :modes %s" modes))
     ((not (or (symbolp modes) (listp modes)))
      (error "Invalid dokument backend, :modes should be a symbol or list of symbols, got %s: %s"
             (type-of modes) modes))

     ((not symbol-fn)
      (error "Invalid dokument backend, missing key :symbol-fn"))
     ((not (or (functionp symbol-fn) (symbolp symbol-fn)))
      (error "Invalid dokument backend, :symbol-fn should be a function, got a %s: %s"
             (type-of symbol-fn) symbol-fn))

     ((not doc-fn)
      (error "Invalid dokument backend, missing key :doc-fn"))
     ((not (or (functionp doc-fn) (symbolp doc-fn)))
      (error "Invalid dokument backend, :doc-fn should be a function, got %s: %s"
             (type-of doc-fn) doc-fn))

     ((not should-run-p)
      (error "Invalid dokument backend, missing key :should-run-p"))
     ((not (or (functionp should-run-p) (symbolp should-run-p)))
      (error "Invalid dokument backend, :should-run-p should be a symbol or a function, got %s: %s"
             (type-of should-run-p) should-run-p))

     ((not order)
      (error "Invalid dokument backend, missing key :order"))
     ((not (numberp order))
      (error "Invalid dokument backend, :order should be a number, got %s: %s"
             (type-of order) order))
     (t backend))))

(defun dokument--should-run (sym-or-fn)
  "Return whether or not SYM-OR-FN indicates if it should or not."
  (if (functionp sym-or-fn)
      (funcall sym-or-fn)
    sym-or-fn))

(defun dokument--backend-exists-p (mode id)
  "Check if a backend with ID exists for MODE."
  (let* ((registered-backends (dokument--get-backends mode))
         (ids (-map
               (lambda (b) (dokument--get-id b))
               registered-backends)))
    (member id ids)))

(defun dokument--sort-backend-predicate (a b)
  "Predicate for sorting backends by order."
  (< (dokument--get-order a)
     (dokument--get-order b)))

(defun dokument--add-backend (mode backend)
  "Add a new documentation BACKEND for MODE."
  (cond
   ((not (dokument--valid-backend-p backend))
    (message "Invalid backend: %s" backend))
   ((dokument--backend-exists-p mode (dokument--get-id backend))
    (message "A backend with id '%s' already exists for %s"
             (dokument--get-id backend) mode))
   (t
    (let ((backends (dokument--get-backends mode)))
      (ht-set dokument-backends mode
              ;; make sure that the backends are sorted, so when we're iterating
              ;; over them later `car' is always the backend with the lowest
              ;; order
              (sort (cons backend backends)
                    #'dokument--sort-backend-predicate))))))

(defun dokument--remove-backend (mode id)
  "Remove a documentation backend with ID from MODE."
  (if (dokument--backend-exists-p mode id)
      (let* ((backends (ht-get dokument-backends mode))
             (filtered (-remove
                        (lambda (e) (string= id (ht-get e :id)))
                        backends)))
        (ht-set dokument-backends mode filtered))
    (message "backend `%s-%s' does not exist." mode id)))

(defun dokument--suitable-backend (mode)
  "Try to find a suitable documentation backend to use for MODE."
  (when-let ((backends (dokument--get-backends mode)))
    (let* ((backend (-first
                     (lambda (b)
                       (dokument--should-run
                        (dokument--get-should-run-p b)))
                     backends)))
      (when backend
        backend))))

(defun dokument--with-backend (backend &optional sym)
  "Lookup documentation using BACKEND.

Optionally lookup documentation for SYM using BACKEND."
  (let* ((symbol-fn (dokument--get-symbol-fn backend))
         (doc-fn (dokument--get-doc-fn backend))
         (sym (if sym sym (funcall symbol-fn)))
         (doc (funcall doc-fn sym)))
    (if doc
        (funcall dokument-display-fn doc)
      (message "No documentation found for %s" sym))))

;;;###autoload
(cl-defun dokument-register (&key id modes symbol-fn doc-fn (should-run-p t) (order 1))
  "Register a new documentation backend."
  (declare (indent defun))
  (let ((backend (dokument--build-backend id modes symbol-fn doc-fn should-run-p order)))
    (when (dokument--valid-backend-p backend)
      (cond
       ((symbolp modes)
        (dokument--add-backend modes backend))
       ((listp modes)
        (-map
         (lambda (m) (dokument--add-backend m backend))
         modes))))))

;;;###autoload
(defun dokument-other-buffer (&optional sym)
  "Show documentation for the symbol at point in another buffer.

Optionally show documentation for SYM."
  (interactive)
  (let ((dokument-display-fn #'dokument--display-with-buffer))
    (funcall #'dokument sym)))

;;;###autoload
(defun dokument-echo-area (&optional sym)
  "Show documentation for the symbol at point in echo area.

Optionally show documentation for SYM."
  (interactive)
  (let ((dokument-display-fn #'dokument--display-with-message))
    (funcall #'dokument sym)))

;;;###autoload
(defun dokument (&optional sym)
  "Show documentation for the symbol at point, based on relevant backend.

Optionally show documentation for SYM."
  (interactive)
  (let* ((current-mode major-mode)
         (backend (dokument--suitable-backend current-mode)))
    (if backend
        (if sym
            (dokument--with-backend backend sym)
          (dokument--with-backend backend))
      (message "No dokument backend for %s" current-mode))))

;;;###autoload
(defun dokument-use-defaults ()
  "Setup default handlers for dokument."
  (interactive)
  ;; set default keybinding if the key is not already used
  (let ((default-key (kbd "C-+")))
    (unless (or (local-key-binding default-key)
                (global-key-binding default-key))
      (global-set-key default-key #'dokument))

    (with-eval-after-load 'company
      (define-key company-active-map default-key #'doc-at-point-company-menu-selection-quickhelp)))

  (require 'dokument-elisp)
  (require 'dokument-python)
  (require 'dokument-rust))

;; TODO: add backend for `lsp' / `eglot'

(require 'dokument-extra)

(provide 'dokument)
;;; dokument.el ends here
