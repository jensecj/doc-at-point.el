;;; doc-at-point-extra.el --- Extras for doc-at-point, integrations, etc. -*- lexical-binding: t; -*-

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

(with-eval-after-load 'company
  (defun doc-at-point-company-menu-selection-quickhelp ()
    "Show documentation for the current selection in company-menu."
    (interactive)
    (let ((candidate (nth company-selection company-candidates)))
      (funcall doc-at-point-display-fn
               (doc-at-point (intern candidate))))))

(provide 'doc-at-point-extra)
