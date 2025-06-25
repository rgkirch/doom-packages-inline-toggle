;;; doom-packages-inline-toggle-test.el --- Tests for `doom-packages-inline-toggle` -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains ERT (Emacs Lisp Regression Testing) tests for the
;; `doom-packages-inline-toggle.el` package.

;;; Code:

(require 'ert)
(require 'doom-packages-inline-toggle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Test Helper Macro
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro dpit-test--with-buffer (content &rest body)
  "Create a temp emacs-lisp-mode buffer with CONTENT and run BODY."
  `(with-temp-buffer
     (emacs-lisp-mode)
     (insert ,content)
     (goto-char (point-min))
     dpit-body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section 1: Buffer Modification Tests
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest dpit--unpin-package-test ()
  "Test changing `package!` to `unpin!`."
  (let ((initial-content "(package! some-pkg)")
        (expected-content "(unpin! some-pkg)"))
    (dpit-test--with-buffer initial-content
      (dpit--unpin-package (point-min))
      (should (string= (buffer-string) expected-content)))))

(ert-deftest dpit--pin-package-test ()
  "Test changing `unpin!` to `package!`."
  (let ((initial-content "(unpin! some-pkg)")
        (expected-content "(package! some-pkg)"))
    (dpit-test--with-buffer initial-content
      (dpit--pin-package (point-min))
      (should (string= (buffer-string) expected-content)))))

(ert-deftest dpit--unpin-package-with-whitespace-test ()
  "Test changing `( package! ...)` to `( unpin! ...)`."
  (let ((initial-content "( package! some-pkg :recipe (:host github))")
        (expected-content "(unpin! some-pkg :recipe (:host github))"))
    (dpit-test--with-buffer initial-content
      (dpit--unpin-package (point-min))
      (should (string= (buffer-string) expected-content)))))

(ert-deftest dpit--pin-package-with-whitespace-test ()
  "Test changing `( unpin! ...)` to `( package! ...)`."
  (let ((initial-content "( unpin! some-pkg :disable t)")
        (expected-content "(package! some-pkg :disable t)"))
    (dpit-test--with-buffer initial-content
      (dpit--pin-package (point-min))
      (should (string= (buffer-string) expected-content)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section 2: Rendering and Interaction Tests
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest dpit--render-buttons-for-multiple-forms-test ()
  "Test that buttons for multiple forms are ordered correctly."
  (dpit-test--with-buffer "(package! a) (unpin! b :disable t)"
    (doom-packages-inline-toggle 1) (doom-packages-inline-toggle -1)
    (dpit--render-overlays-for-line 1)
    ;; We now expect 4 overlays, one for each button.
    (should (= (length dpit--overlays) 4))
    ;; The overlays are created from right to left with increasing priority.
    ;; We can check the text of the overlays to confirm order.
    ;; `doom-packages-inline-toggle--overlays` is built with `push`, so it's in reverse order of creation.
    (let ((button-texts (mapcar (lambda (ov) (substring-no-properties (overlay-get ov 'after-string)))
                                dpit--overlays)))
      ;; The visual order is "unpin disable pin enable".
      ;; The creation order (and thus reverse of the final list) is "enable pin disable unpin".
      (should (equal button-texts '(" unpin" " disable" " pin" " enable"))))))

(defun ert-get-button-action (overlay-string)
  "Extract the action lambda for a button from an OVERLAY-STRING."
  (get-text-property 1 'action overlay-string))

(ert-deftest dpit--simulated-click-on-multiple-forms-test ()
  "Test clicking a specific button when multiple are present."
  (let ((initial-buffer "(package! a) (package! b)"))
    (dpit-test--with-buffer initial-buffer
      (doom-packages-inline-toggle 1) (doom-packages-inline-toggle -1)
      (dpit--render-overlays-for-line 1)
      ;; We expect 4 overlays: unpin/disable for 'a', and unpin/disable for 'b'
      (should (= (length dpit--overlays) 4))
      (let* ((disable-b-overlay
              ;; The overlays are created for 'b' first, then 'a'. The disable
              ;; button is the first one created for 'b'. So it's the last
              ;; element in `doom-packages-inline-toggle--overlays`.
              (car (last dpit--overlays)))
             (after-string (overlay-get disable-b-overlay 'after-string))
             (disable-b-action (ert-get-button-action after-string)))
        (should (string-match-p " disable" after-string))
        (should disable-b-action)
        ;; Simulate the click
        (funcall disable-b-action nil)) ; Pass a dummy arg for `_`
      ;; Verify that ONLY package b was modified
      (should (string= (buffer-string) "(package! a) (package! b :disable t)")))))

(ert-deftest dpit--unpin-nested-package-test ()
  "Test changing a nested `package!` to `unpin!`."
  (let ((initial-content "(when t\n  (package! tree-inspector))")
        (expected-content "(when t\n  (unpin! tree-inspector))"))
    (dpit-test--with-buffer initial-content
      (doom-packages-inline-toggle 1) ; Activate to set up internals
      (doom-packages-inline-toggle -1) ; Deactivate to stop timer

      ;; Go to the line with the package form to simulate the mouse being there
      (goto-char (point-min))
      (forward-line 1)

      ;; Manually trigger the render for that line
      (dpit--render-overlays-for-line (line-number-at-pos))

      ;; There should be two overlays: "unpin" and "disable"
      (should (= (length dpit--overlays) 2))

      ;; Find the 'unpin' action. It has the higher priority, so it's created last
      ;; and will be the first element in `doom-packages-inline-toggle--overlays`.
      (let* ((unpin-overlay (car dpit--overlays))
             (unpin-action (ert-get-button-action (overlay-get unpin-overlay 'after-string))))
        (should (string-match-p " unpin" (overlay-get unpin-overlay 'after-string)))
        (should unpin-action)
        ;; Simulate the user clicking the 'unpin' button
        (funcall unpin-action nil))

      ;; Verify the buffer content was changed correctly
      (should (string= (buffer-string) expected-content)))))


(provide 'doom-packages-inline-toggle-test)

;; Local Variables:
;; read-symbol-shorthands: (("dpit-" . "doom-packages-inline-toggle-"))
;; End:

;;; doom-packages-inline-toggle-test.el ends here
