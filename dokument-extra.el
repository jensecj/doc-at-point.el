;;; dokument-extra.el --- Extras for dokument, integrations, etc. -*- lexical-binding: t; -*-

;; TODO: don't use `with-eval-after-load', its for personal use, not for libraries

;; instead make users require this specific files
;; (e.g. `dokument-posframe.el'), and set the display function them
;; selves

(with-eval-after-load 'popup
  (defun dokument--display-with-popup (doc-string)
    "Shows DOC-STRING in a `popup.el' tooltip."
    (popup-tip doc-string :margin-left 1 :margin-right 1))

  ;; if `popup.el' is loaded, use it as the default display function
  (setq dokument-display-fn #'dokument--display-with-popup))

;; TODO: see company-box for better frame config
;; TODO: don't rely on `posframe.el'?
(with-eval-after-load 'posframe
  (defvar dokument--posframe "*dokument-posframe*"
    "Posframe used for displaying documentation.")

  (defvar dokument--posframe-font (frame-parameter (selected-frame) 'font)
    "Font used in child frame, default is the same font as the
    main-frame.")

  (defvar dokument--posframe-format "\n%s\n﻿"
    "Format string for doc-strings to be displayed in the
    posframe.  By default there's a zero-width non-breaking space
    at the end of the format-string, to create a newline before
    and after the doc-string, and internally the height of the
    frame is set using `fit-frame-to-buffer', which ignores
    newlines and whitespace at the end of buffers. ")

  (defvar dokument--internal-border-width 1
    "Width of the border around `dokument--posframe', in pixels.")

  (defvar dokument--fringe-widths '(6 . 6)
    "Width of the fringes of `dokument--posframe', as a pair
    of `(left-fringe-width . right-fringe-width)'")

  (defvar dokument--posframe-hide-hooks '(pre-command-hook)
    "Hooks where the documentation frame should hide.")

  (defun dokument--posframe-one-shot-kill ()
    "Kills `dokument--posframe' once a hook from
`dokument--posframe-hide-hooks' triggers, and removes then removes
trigger from those hooks."
    (unless (eq this-command 'dokument)
      (posframe-delete dokument--posframe)
      (dolist (hook dokument--posframe-hide-hooks)
        (remove-hook hook #'dokument--posframe-one-shot-kill))))

  (defun dokument--display-with-posframe (doc-string)
    "Shows DOC-STRING in a `posframe.el' frame."
    (when (not (s-blank-str? doc-string))
      (posframe-show dokument--posframe
                     :string (format dokument--posframe-format doc-string)
                     :internal-border-width dokument--internal-border-width
                     :font dokument--posframe-font
                     :left-fringe (car dokument--fringe-widths)
                     :right-fringe (cdr dokument--fringe-widths))

      ;; kill the frame once we trigger a hook from
      ;; `dokument--posframe-hide-hooks'.
      (dolist (hook dokument--posframe-hide-hooks)
        (add-hook hook #'dokument--posframe-one-shot-kill nil t))))

  ;; if `posframe.el' is loaded, use it as the default display function
  (setq dokument-display-fn #'dokument--display-with-posframe))

(with-eval-after-load 'company
  (defun dokument-company-menu-selection-quickhelp ()
    "Show documentation for the current selection in company-menu."
    (interactive)
    (let ((candidate (nth company-selection company-candidates)))
      (funcall dokument-display-fn
               (dokument (intern candidate))))))

(provide 'dokument-extra)
