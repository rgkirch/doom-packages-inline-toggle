;;; doom-packages-inline-toggle.el --- On-hover overlays for Doom `package!` and `unpin!` forms. -*- lexical-binding: t; -*-

;;; Commentary:
;; This package provides a minor mode that displays clickable buttons
;; at the end of a line containing `package!` or `unpin!` declarations.
;; The buttons allow for quickly toggling the pin status (`package!` <->
;; `unpin!`) and the disabled status for each package on the line.
;; This package uses the standard `button.el` library.

;;; Code:

(require 'cl-lib)
(require 'button)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section 1: State Management and Configuration
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local doom-packages-inline-toggle--overlays '()
  "A buffer-local list that tracks all overlays created by the mode.")

(defvar-local doom-packages-inline-toggle--timer nil
  "The buffer-local timer used for hover detection.")

(defvar-local doom-packages-inline-toggle--last-line-number -1
  "The line number last processed. Used to prevent flickering.")

(defvar-local doom-packages-inline-toggle--last-pixel-position nil
  "The raw mouse pixel position last processed. Used for performance.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section 2: Core Logic - Finding and Parsing Forms
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun doom-packages-inline-toggle--get-sexp-at-point (pt)
  "Safely return the S-expression starting at point PT."
  (save-excursion
    (goto-char pt)
    (ignore-errors (read (current-buffer)))))

(defun doom-packages-inline-toggle--is-target-form-p (form)
  "Return non-nil if FORM is a `package!` or `unpin!` form."
  (and (listp form) (memq (car form) '(package! unpin!))))

(defun doom-packages-inline-toggle--collect-candidate-forms (line-start line-end)
  "Collect all `package!` or `unpin!` forms on the current line.
This version correctly handles nested forms."
  (let ((forms '())
        (search-bound line-end))
    (save-excursion
      (goto-char line-start)
      (while (re-search-forward "\\b\\(package!\\|unpin!\\)" search-bound t)
        (let ((match-pos (match-beginning 0)))
          (save-excursion
            (goto-char match-pos)
            (ignore-errors
              ;; Go to the start of the containing list.
              (backward-up-list 1)
              (let* ((form-start (point))
                     (form (doom-packages-inline-toggle--get-sexp-at-point form-start)))
                ;; Check if the found keyword is actually the function call.
                (when (doom-packages-inline-toggle--is-target-form-p form)
                  (push (cons form form-start) forms))))))))
    (nreverse forms)))

(defun doom-packages-inline-toggle--analyze-form (form-info)
  "Analyze a candidate form to get name, type, status, and boundaries.
Returns a plist on success, otherwise nil."
  (cl-destructuring-bind ((declaration package-name . rest) . form-start) form-info
    (when package-name
      `(:package-name ,package-name
        :is-pinned ,(not (eq declaration 'unpin!))
        :is-disabled ,(plist-get rest :disable)
        :form-start ,form-start
        :form-end ,(save-excursion (goto-char form-start) (forward-sexp 1) (point))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section 3: Buffer Editing Actions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun doom-packages-inline-toggle--add-disable-flag (form-end)
  "Navigate to the end of a form and insert ' :disable t'."
  (goto-char (1- form-end))
  (insert " :disable t"))

(defun doom-packages-inline-toggle--remove-disable-flag (form-start form-end)
  "Find and remove ':disable t' within a form's bounds."
  (save-excursion
    (goto-char form-start)
    (when (re-search-forward "[[:space:]]*:disable[[:space:]]+t" form-end t)
      (delete-region (match-beginning 0) (match-end 0)))))

(defun doom-packages-inline-toggle--replace-form-symbol (form-start new-symbol-name)
  "Replace the first symbol in the form at FORM-START with NEW-SYMBOL-NAME."
  (save-excursion
    (goto-char form-start)
    (forward-char) ; Move past '('
    (let ((p1 (point)))
      (forward-sexp) ; Move past the symbol and any whitespace after it
      (delete-region p1 (point))
      (insert new-symbol-name))))

(defun doom-packages-inline-toggle--pin-package (form-start)
  "Change an `unpin!` form to a `package!` form."
  (doom-packages-inline-toggle--replace-form-symbol form-start "package!"))

(defun doom-packages-inline-toggle--unpin-package (form-start)
  "Change a `package!` form to an `unpin!` form."
  (doom-packages-inline-toggle--replace-form-symbol form-start "unpin!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section 4: Button Generation
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun doom-packages-inline-toggle--get-disable-button-def (analysis)
  "Return a (LABEL . ACTION) cons for the disable/enable button."
  (cl-destructuring-bind (&key is-disabled form-start form-end &allow-other-keys) analysis
    (if is-disabled
        (list "enable" (lambda () (doom-packages-inline-toggle--remove-disable-flag form-start form-end)))
      (list "disable" (lambda () (doom-packages-inline-toggle--add-disable-flag form-end))))))

(defun doom-packages-inline-toggle--get-pin-button-def (analysis)
  "Return a (LABEL . ACTION) cons for the pin/unpin button."
  (cl-destructuring-bind (&key is-pinned form-start &allow-other-keys) analysis
    (if is-pinned
        (list "unpin" (lambda () (doom-packages-inline-toggle--unpin-package form-start)))
      (list "pin" (lambda () (doom-packages-inline-toggle--pin-package form-start))))))

(defvar doom-packages-inline-toggle--button-generators
  (list #'doom-packages-inline-toggle--get-disable-button-def
        #'doom-packages-inline-toggle--get-pin-button-def)
  "A list of functions that generate button definitions.
Each function takes an analysis plist and returns a list of
\(LABEL ACTION-LAMBDA).")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section 5: Overlay Management and Rendering
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defun doom-packages-inline-toggle--custom-buttonize (string callback &key (face 'lsp-lens-face) (mouse-face 'lsp-lens-mouse-face) data help-echo)
  "Create a button from STRING with custom faces.
When clicked, run CALLBACK. `face' is the default text face,
and `mouse-face' is the face used on mouse hover."
  (propertize string
              'face face
              'mouse-face mouse-face
              'keymap button-map
              'action callback
              'button t
              'follow-link t
              'help-echo help-echo
              'button-data data))

(defun doom-packages-inline-toggle--cleanup ()
  "Clear all overlays and reset the hover state variables."
  (doom-packages-inline-toggle--clear-overlays)
  (setq doom-packages-inline-toggle--last-line-number -1)
  (setq doom-packages-inline-toggle--last-pixel-position nil))

(defun doom-packages-inline-toggle--clear-overlays ()
  "Delete all overlays managed by this mode in the current buffer."
  (dolist (ov doom-packages-inline-toggle--overlays)
    (delete-overlay ov))
  (setq doom-packages-inline-toggle--overlays '()))

(defun doom-packages-inline-toggle--render-overlays-for-line (line-number)
  "Create an individual overlay for each button, with proper spacing."
  (doom-packages-inline-toggle--clear-overlays)
  (let* ((spacing-str (propertize " " 'display '(space :width 1)))
         (line-start (save-excursion (goto-char (point-min)) (forward-line (1- line-number)) (point)))
         (line-end (save-excursion (goto-char line-start) (line-end-position)))
         (candidate-forms (doom-packages-inline-toggle--collect-candidate-forms line-start line-end))
         (priority 0))
    ;; Process forms from right-to-left, assigning increasing priority so that
    ;; the final display appears left-to-right.
    (dolist (form-info (nreverse candidate-forms))
      (when-let ((analysis (doom-packages-inline-toggle--analyze-form form-info)))
        ;; Iterate over the registered generator functions to create the buttons.
        (dolist (generator doom-packages-inline-toggle--button-generators)
          (when-let ((button-def (funcall generator analysis)))
            (cl-destructuring-bind (label action) button-def
              (let* ((button-str (doom-packages-inline-toggle--custom-buttonize
                                  label
                                  `(lambda (_)
                                     (funcall ,action)
                                     (doom-packages-inline-toggle--cleanup))))
                     (ov (make-overlay line-end line-end)))
                (overlay-put ov 'after-string (concat spacing-str button-str))
                (overlay-put ov 'priority (cl-incf priority))
                (push ov doom-packages-inline-toggle--overlays)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section 6: Timer and Minor Mode Definition
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun doom-packages-inline-toggle--get-mouse-line-number (mouse-data)
  "Return the line number under the mouse from MOUSE-DATA, or nil."
  (when mouse-data
    (cl-destructuring-bind (frame x . y) mouse-data
      (when (and frame (>= x 0) (>= y 0))
        (let* ((posn (posn-at-x-y x y frame))
               (point (and posn (posn-point posn)))
               (line-num (and point (line-number-at-pos point))))
          line-num)))))

(defun doom-packages-inline-toggle--timer-callback ()
  "The main timer function that orchestrates the hover effect."
  (let ((current-pos (mouse-pixel-position))
        current-line)
    (cond
     ;; If mode isn't active in the current window, do nothing.
     ((not (buffer-local-value 'doom-packages-inline-toggle (window-buffer (frame-selected-window)))))
     ;; If mouse is completely still, do nothing.
     ((equal current-pos doom-packages-inline-toggle--last-pixel-position))
     ;; If mouse is outside a window, clean up the UI.
     ((not (setq current-line (doom-packages-inline-toggle--get-mouse-line-number current-pos)))
      (when doom-packages-inline-toggle--overlays
        (doom-packages-inline-toggle--cleanup)))
     ;; If mouse is on the same line, just update pixel position and do nothing else.
     ((= current-line doom-packages-inline-toggle--last-line-number)
      (setq doom-packages-inline-toggle--last-pixel-position current-pos))
     ;; Otherwise, we're on a new line, so render overlays.
     (t
      (setq doom-packages-inline-toggle--last-pixel-position current-pos)
      (setq doom-packages-inline-toggle--last-line-number current-line)
      (doom-packages-inline-toggle--render-overlays-for-line current-line)))))

(define-minor-mode doom-packages-inline-toggle
  "On hover, show overlays for `package!` and `unpin!` forms."
  :init-value nil
  :lighter " Pkg-Hov"
  :keymap nil
  (if doom-packages-inline-toggle
      (progn
        (doom-packages-inline-toggle--cleanup) ; Reset on activation
        (setq doom-packages-inline-toggle--timer
              (run-with-timer 0.2 0.2 #'doom-packages-inline-toggle--timer-callback)))
    (when (timerp doom-packages-inline-toggle--timer)
      (cancel-timer doom-packages-inline-toggle--timer))
    (doom-packages-inline-toggle--cleanup))) ; Reset on deactivation

(provide 'doom-packages-inline-toggle)

;;; doom-packages-inline-toggle.el ends here
