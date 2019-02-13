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

(require 'ht)


(defvar doc-at-point-map (ht)
  "Alist of plists.
KEY is the mode registred with a backend.

VALUE is a plist of `(:symbol-fn :doc-fn :should-run-p :order)'.

:SYMBOL-FN = () -> symbol: returns the symbol-at-point.

:DOC-FN = symbol -> string: returns documentation for a symbol.

:SHOULD-RUN-P = nil | t | (() -> nil | t): whether the backend
should be used, can be a symbol or a predicate.

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
      (goto-char (point-min))
      (view-buffer-other-window (current-buffer)))))

(defvar doc-at-point-display-fn #'doc-at-point--display-with-buffer
  "Function used to display documentation.")

(with-eval-after-load 'popup
  (defun doc-at-point--display-with-popup (doc-string)
    "Shows DOC-STRING in a `popup.el' tooltip."
    (popup-tip doc-string :margin-left 1 :margin-right 1))

  ;; if `popup.el' is loaded, use it as the default display function
  (setq doc-at-point-display-fn #'doc-at-point--display-with-popup))

(with-eval-after-load 'posframe
  (defvar doc-at-point--posframe "*doc-at-point-posframe*"
    "Posframe used for displaying documentation.")

  (defvar doc-at-point--posframe-font (frame-parameter (selected-frame) 'font)
    "Font used in child frame, default is the same font as the
    main-frame.")

  (defvar doc-at-point--posframe-format "\n%s\n﻿"
    "Format string for doc-strings to be displayed in the
    posframe.  By default there's a zero-width non-breaking space
    at the end of the format-string, to create a newline before
    and after the doc-string, and internally the height of the
    frame is set using `fit-frame-to-buffer', which ignores
    newlines and whitespace at the end of buffers. ")

  (defvar doc-at-point--internal-border-width 1
    "Width of the border around `doc-at-point--posframe', in pixels.")

  (defvar doc-at-point--fringe-widths '(6 . 6)
    "Width of the fringes of `doc-at-point--posframe', as a pair
    of `(left-fringe-width . right-fringe-width)'")

  (defvar doc-at-point--posframe-hide-hooks '(pre-command-hook)
    "Hooks where the documentation frame should hide.")

  (defun doc-at-point--posframe-one-shot-kill ()
    "Kills `doc-at-point--posframe' once a hook from
`doc-at-point--posframe-hide-hooks' triggers, and removes then removes
trigger from those hooks."
    (unless (eq this-command 'doc-at-point)
      (posframe-delete doc-at-point--posframe)
      (dolist (hook doc-at-point--posframe-hide-hooks)
        (remove-hook hook #'doc-at-point--posframe-one-shot-kill))))

  (defun doc-at-point--display-with-posframe (doc-string)
    "Shows DOC-STRING in a `posframe.el' frame."
    (when (not (s-blank-str? doc-string))
      (posframe-show doc-at-point--posframe
                     :string (format doc-at-point--posframe-format doc-string)
                     :internal-border-width doc-at-point--internal-border-width
                     :font doc-at-point--posframe-font
                     :left-fringe (car doc-at-point--fringe-widths)
                     :right-fringe (cdr doc-at-point--fringe-widths))

      ;; kill the frame once we trigger a hook from
      ;; `doc-at-point--posframe-hide-hooks'.
      (dolist (hook doc-at-point--posframe-hide-hooks)
        (add-hook hook #'doc-at-point--posframe-one-shot-kill nil t))))

  ;; if `posframe.el' is loaded, use it as the default display function
  (setq doc-at-point-display-fn #'doc-at-point--display-with-posframe))

(defun doc-at-point--should-run (sym-or-fn)
  "Return whether or not SYM-OR-FN indicates if should or not."
  (if (functionp sym-or-fn)
      (funcall sym-or-fn)
    sym-or-fn))

(defun doc-at-point--with-backend (entry)
  "Lookup documentation using ENTRY."
  (let* ((symbol-fn (ht-get entry :symbol-fn))
         (doc-fn (ht-get entry :doc-fn))
         (sym (funcall symbol-fn))
         (doc (funcall doc-fn sym)))
    (if doc
        (funcall doc-at-point-display-fn doc)
      (message "No documentation found for %s" (symbol-name sym)))))

(defun doc-at-point--exists? (mode id)
  (let* ((registered-backends (ht-get doc-at-point-map mode '()))
         (ids (-map #'(lambda (m) (ht-get m :id)) registered-backends)))
    (member id ids)))

(defun doc-at-point--sort-backend-predicate (a b)
  "Predicate for sorting documentation backends by order."
  (< (ht-get a :order) (ht-get b :order)))

(defun doc-at-point--valid-backend-p (backend)
  "Checks if a BACKEND is valid."
  (and
   (stringp (ht-get backend :id))
   (or (symbolp (ht-get backend :modes))
       (listp (ht-get backend :modes)))
   (functionp (ht-get backend :symbol-fn))
   (functionp (ht-get backend :doc-fn))
   (or (symbolp (ht-get backend :should-fun))
       (functionp (ht-get backend :should-fun)))
   (numberp (ht-get backend :order))))

(defun doc-at-point--add-backend (mode backend)
  "Add a new documentation BACKEND for MODE."
  (cond
   ((not (doc-at-point--valid-backend-p backend))
    (error "Invalid backend: %s" backend))
   ((doc-at-point--exists? mode (ht-get backend :id))
    (error "A backend with id '%s' already exists for %s" id modes))
   (t
    (let ((backends (ht-get doc-at-point-map mode '())))
      (ht-set doc-at-point-map mode
              ;; make sure that the entries sorted
              (sort (cons backend backends)
                    #'doc-at-point--sort-backend-predicate))))))


(defun doc-at-point--suitable-backend (mode)
  "Try to find a suitable documentation backend to use for MODE."
  (when-let ((backends (ht-get doc-at-point-map mode)))
    (let* ((backend (-first
                     #'(lambda (b)
                         (doc-at-point--should-run
                          (ht-get b :should-run)))
                     backends)))
      (when backend
        backend))))

;;;###autoload
(cl-defun doc-at-point-register (&key id modes symbol-fn doc-fn (should-run t) (order 1))
  "Register a new documentation backend."
  (let ((backend (ht
                  (:id id)
                  (:symbol-fn symbol-fn)
                  (:doc-fn doc-fn)
                  (:should-run should-run)
                  (:order order))))
    (cond
     ((symbolp modes)
      (doc-at-point--add-backend modes backend))
     ((listp modes)
      (-map
       #'(lambda (m) (doc-at-point--add-backend m backend))
       modes)))))

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
      (global-set-key default-key #'doc-at-point)))

  (require 'doc-at-point-elisp)
  (require 'doc-at-point-python)
  (require 'doc-at-point-rust))

(provide 'doc-at-point)
