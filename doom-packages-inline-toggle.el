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

(defvar-local my-pkg-hover--overlays '()
  "A buffer-local list that tracks all overlays created by the mode.")

(defvar-local my-pkg-hover--timer nil
  "The buffer-local timer used for hover detection.")

(defvar-local my-pkg-hover--last-line-number -1
  "The line number last processed. Used to prevent flickering.")

(defvar-local my-pkg-hover--last-pixel-position nil
  "The raw mouse pixel position last processed. Used for performance.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section 2: Core Logic - Finding and Parsing Forms
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-pkg-hover--get-sexp-at-point (pt)
  "Safely return the S-expression starting at point PT."
  (save-excursion
    (goto-char pt)
    (ignore-errors (read (current-buffer)))))

(defun my-pkg-hover--is-target-form-p (form)
  "Return non-nil if FORM is a `package!` or `unpin!` form."
  (and (listp form) (memq (car form) '(package! unpin!))))

(defun my-pkg-hover--collect-candidate-forms (line-start line-end)
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
                     (form (my-pkg-hover--get-sexp-at-point form-start)))
                ;; Check if the found keyword is actually the function call.
                (when (my-pkg-hover--is-target-form-p form)
                  (push (cons form form-start) forms))))))))
    (nreverse forms)))

(defun my-pkg-hover--analyze-form (form-info)
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

(defun my-pkg-hover--add-disable-flag (form-end)
  "Navigate to the end of a form and insert ' :disable t'."
  (goto-char (1- form-end))
  (insert " :disable t"))

(defun my-pkg-hover--remove-disable-flag (form-start form-end)
  "Find and remove ':disable t' within a form's bounds."
  (save-excursion
    (goto-char form-start)
    (when (re-search-forward "[[:space:]]*:disable[[:space:]]+t" form-end t)
      (delete-region (match-beginning 0) (match-end 0)))))

(defun my-pkg-hover--replace-form-symbol (form-start new-symbol-name)
  "Replace the first symbol in the form at FORM-START with NEW-SYMBOL-NAME."
  (save-excursion
    (goto-char form-start)
    (forward-char) ; Move past '('
    (let ((p1 (point)))
      (forward-sexp) ; Move past the symbol and any whitespace after it
      (delete-region p1 (point))
      (insert new-symbol-name))))

(defun my-pkg-hover--pin-package (form-start)
  "Change an `unpin!` form to a `package!` form."
  (my-pkg-hover--replace-form-symbol form-start "package!"))

(defun my-pkg-hover--unpin-package (form-start)
  "Change a `package!` form to an `unpin!` form."
  (my-pkg-hover--replace-form-symbol form-start "unpin!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section 4: Button Generation
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-pkg-hover--get-disable-button-def (analysis)
  "Return a (LABEL . ACTION) cons for the disable/enable button."
  (cl-destructuring-bind (&key is-disabled form-start form-end &allow-other-keys) analysis
    (if is-disabled
        (list "enable" (lambda () (my-pkg-hover--remove-disable-flag form-start form-end)))
      (list "disable" (lambda () (my-pkg-hover--add-disable-flag form-end))))))

(defun my-pkg-hover--get-pin-button-def (analysis)
  "Return a (LABEL . ACTION) cons for the pin/unpin button."
  (cl-destructuring-bind (&key is-pinned form-start &allow-other-keys) analysis
    (if is-pinned
        (list "unpin" (lambda () (my-pkg-hover--unpin-package form-start)))
      (list "pin" (lambda () (my-pkg-hover--pin-package form-start))))))

(defvar my-pkg-hover--button-generators
  (list #'my-pkg-hover--get-disable-button-def
        #'my-pkg-hover--get-pin-button-def)
  "A list of functions that generate button definitions.
Each function takes an analysis plist and returns a list of
\(LABEL ACTION-LAMBDA).")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section 5: Overlay Management and Rendering
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defun my-pkg-hover--custom-buttonize (string callback &key (face 'lsp-lens-face) (mouse-face 'lsp-lens-mouse-face) data help-echo)
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

(defun my-pkg-hover--cleanup ()
  "Clear all overlays and reset the hover state variables."
  (my-pkg-hover--clear-overlays)
  (setq my-pkg-hover--last-line-number -1)
  (setq my-pkg-hover--last-pixel-position nil))

(defun my-pkg-hover--clear-overlays ()
  "Delete all overlays managed by this mode in the current buffer."
  (dolist (ov my-pkg-hover--overlays)
    (delete-overlay ov))
  (setq my-pkg-hover--overlays '()))

(defun my-pkg-hover--render-overlays-for-line (line-number)
  "Create an individual overlay for each button, with proper spacing."
  (my-pkg-hover--clear-overlays)
  (let* ((spacing-str (propertize " " 'display '(space :width 1)))
         (line-start (save-excursion (goto-char (point-min)) (forward-line (1- line-number)) (point)))
         (line-end (save-excursion (goto-char line-start) (line-end-position)))
         (candidate-forms (my-pkg-hover--collect-candidate-forms line-start line-end))
         (priority 0))
    ;; Process forms from right-to-left, assigning increasing priority so that
    ;; the final display appears left-to-right.
    (dolist (form-info (nreverse candidate-forms))
      (when-let ((analysis (my-pkg-hover--analyze-form form-info)))
        ;; Iterate over the registered generator functions to create the buttons.
        (dolist (generator my-pkg-hover--button-generators)
          (when-let ((button-def (funcall generator analysis)))
            (cl-destructuring-bind (label action) button-def
              (let* ((button-str (my-pkg-hover--custom-buttonize
                                  label
                                  `(lambda (_)
                                     (funcall ,action)
                                     (my-pkg-hover--cleanup))))
                     (ov (make-overlay line-end line-end)))
                (overlay-put ov 'after-string (concat spacing-str button-str))
                (overlay-put ov 'priority (cl-incf priority))
                (push ov my-pkg-hover--overlays)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section 6: Timer and Minor Mode Definition
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-pkg-hover--get-mouse-line-number (mouse-data)
  "Return the line number under the mouse from MOUSE-DATA, or nil."
  (when mouse-data
    (cl-destructuring-bind (frame x . y) mouse-data
      (when (and frame (>= x 0) (>= y 0))
        (let* ((posn (posn-at-x-y x y frame))
               (point (and posn (posn-point posn)))
               (line-num (and point (line-number-at-pos point))))
          line-num)))))

(defun my-pkg-hover--timer-callback ()
  "The main timer function that orchestrates the hover effect."
  (let ((current-pos (mouse-pixel-position))
        current-line)
    (cond
     ;; If mode isn't active in the current window, do nothing.
     ((not (buffer-local-value 'doom-packages-inline-toggle (window-buffer (frame-selected-window)))))
     ;; If mouse is completely still, do nothing.
     ((equal current-pos my-pkg-hover--last-pixel-position))
     ;; If mouse is outside a window, clean up the UI.
     ((not (setq current-line (my-pkg-hover--get-mouse-line-number current-pos)))
      (when my-pkg-hover--overlays
        (my-pkg-hover--cleanup)))
     ;; If mouse is on the same line, just update pixel position and do nothing else.
     ((= current-line my-pkg-hover--last-line-number)
      (setq my-pkg-hover--last-pixel-position current-pos))
     ;; Otherwise, we're on a new line, so render overlays.
     (t
      (setq my-pkg-hover--last-pixel-position current-pos)
      (setq my-pkg-hover--last-line-number current-line)
      (my-pkg-hover--render-overlays-for-line current-line)))))

(define-minor-mode doom-packages-inline-toggle
  "On hover, show overlays for `package!` and `unpin!` forms."
  :init-value nil
  :lighter " Pkg-Hov"
  :keymap nil
  (if doom-packages-inline-toggle
      (progn
        (my-pkg-hover--cleanup) ; Reset on activation
        (setq my-pkg-hover--timer
              (run-with-timer 0.2 0.2 #'my-pkg-hover--timer-callback)))
    (when (timerp my-pkg-hover--timer)
      (cancel-timer my-pkg-hover--timer))
    (my-pkg-hover--cleanup))) ; Reset on deactivation

(provide 'doom-packages-inline-toggle)

;;; doom-packages-inline-toggle.el ends here
