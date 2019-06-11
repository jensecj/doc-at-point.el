;;; dokumat-extra.el --- Extras for dokumat, integrations, etc. -*- lexical-binding: t; -*-

;; TODO: don't use `with-eval-after-load', its for personal use, not for libraries

(with-eval-after-load 'popup
  (defun dokumat--display-with-popup (doc-string)
    "Shows DOC-STRING in a `popup.el' tooltip."
    (popup-tip doc-string :margin-left 1 :margin-right 1))

  ;; if `popup.el' is loaded, use it as the default display function
  (setq dokumat-display-fn #'dokumat--display-with-popup))

;; TODO: see company-box for better frame config
(with-eval-after-load 'posframe
  (defvar dokumat--posframe "*dokumat-posframe*"
    "Posframe used for displaying documentation.")

  (defvar dokumat--posframe-font (frame-parameter (selected-frame) 'font)
    "Font used in child frame, default is the same font as the
    main-frame.")

  (defvar dokumat--posframe-format "\n%s\n﻿"
    "Format string for doc-strings to be displayed in the
    posframe.  By default there's a zero-width non-breaking space
    at the end of the format-string, to create a newline before
    and after the doc-string, and internally the height of the
    frame is set using `fit-frame-to-buffer', which ignores
    newlines and whitespace at the end of buffers. ")

  (defvar dokumat--internal-border-width 1
    "Width of the border around `dokumat--posframe', in pixels.")

  (defvar dokumat--fringe-widths '(6 . 6)
    "Width of the fringes of `dokumat--posframe', as a pair
    of `(left-fringe-width . right-fringe-width)'")

  (defvar dokumat--posframe-hide-hooks '(pre-command-hook)
    "Hooks where the documentation frame should hide.")

  (defun dokumat--posframe-one-shot-kill ()
    "Kills `dokumat--posframe' once a hook from
`dokumat--posframe-hide-hooks' triggers, and removes then removes
trigger from those hooks."
    (unless (eq this-command 'dokumat)
      (posframe-delete dokumat--posframe)
      (dolist (hook dokumat--posframe-hide-hooks)
        (remove-hook hook #'dokumat--posframe-one-shot-kill))))

  (defun dokumat--display-with-posframe (doc-string)
    "Shows DOC-STRING in a `posframe.el' frame."
    (when (not (s-blank-str? doc-string))
      (posframe-show dokumat--posframe
                     :string (format dokumat--posframe-format doc-string)
                     :internal-border-width dokumat--internal-border-width
                     :font dokumat--posframe-font
                     :left-fringe (car dokumat--fringe-widths)
                     :right-fringe (cdr dokumat--fringe-widths))

      ;; kill the frame once we trigger a hook from
      ;; `dokumat--posframe-hide-hooks'.
      (dolist (hook dokumat--posframe-hide-hooks)
        (add-hook hook #'dokumat--posframe-one-shot-kill nil t))))

  ;; if `posframe.el' is loaded, use it as the default display function
  (setq dokumat-display-fn #'dokumat--display-with-posframe))

(with-eval-after-load 'company
  (defun dokumat-company-menu-selection-quickhelp ()
    "Show documentation for the current selection in company-menu."
    (interactive)
    (let ((candidate (nth company-selection company-candidates)))
      (funcall dokumat-display-fn
               (dokumat (intern candidate))))))

(provide 'dokumat-extra)
