;;; brushup-test.el --- Tests for brushup -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for brushup color palette functions.

;;; Code:

(require 'ert)
(require 'color)
(require 'brushup)

;;;; brushup--color-valid-p

(ert-deftest brushup-test-color-valid-p/valid-hex ()
  (should (brushup--color-valid-p "#ff0000")))

(ert-deftest brushup-test-color-valid-p/valid-name ()
  (should (brushup--color-valid-p "white")))

(ert-deftest brushup-test-color-valid-p/nil ()
  (should-not (brushup--color-valid-p nil)))

(ert-deftest brushup-test-color-valid-p/unspecified ()
  (should-not (brushup--color-valid-p "unspecified-fg")))

(ert-deftest brushup-test-color-valid-p/invalid ()
  (should-not (brushup--color-valid-p "not-a-color")))

;;;; brushup--generate-gradient

(ert-deftest brushup-test-generate-gradient/returns-list ()
  (let ((result (brushup--generate-gradient "#808080" 6 1)))
    (should (listp result))
    (should (= 6 (length result)))))

(ert-deftest brushup-test-generate-gradient/lighter-direction ()
  "Gradient with direction 1 should produce lighter colors from a mid-tone."
  (let* ((base "#808080")
         (result (brushup--generate-gradient base 3 1))
         (base-lum (nth 2 (apply #'color-rgb-to-hsl (color-name-to-rgb base))))
         (first-lum (nth 2 (apply #'color-rgb-to-hsl (color-name-to-rgb (nth 0 result))))))
    (should (> first-lum base-lum))))

(ert-deftest brushup-test-generate-gradient/darker-direction ()
  "Gradient with direction -1 should produce darker colors from a mid-tone."
  (let* ((base "#808080")
         (result (brushup--generate-gradient base 3 -1))
         (base-lum (nth 2 (apply #'color-rgb-to-hsl (color-name-to-rgb base))))
         (first-lum (nth 2 (apply #'color-rgb-to-hsl (color-name-to-rgb (nth 0 result))))))
    (should (< first-lum base-lum))))

(ert-deftest brushup-test-generate-gradient/monotonic ()
  "Each step should move further from the base color."
  (let* ((result (brushup--generate-gradient "#808080" 6 1))
         (lums (mapcar (lambda (c)
                         (nth 2 (apply #'color-rgb-to-hsl (color-name-to-rgb c))))
                       result)))
    (should (equal lums (sort (copy-sequence lums) #'<)))))

(ert-deftest brushup-test-generate-gradient/respects-step ()
  "Changing brushup-gradient-step should affect output."
  (let ((brushup-gradient-step 7))
    (let ((result-7 (brushup--generate-gradient "#808080" 3 1)))
      (let ((brushup-gradient-step 14))
        (let ((result-14 (brushup--generate-gradient "#808080" 3 1)))
          (should-not (equal result-7 result-14)))))))

;;;; brushup--eval-style

(defvar brushup-test--eval-target 0
  "Target variable for eval-style tests, visible at top level.")

(ert-deftest brushup-test-eval-style/evaluates-form ()
  (setq brushup-test--eval-target 0)
  (brushup--eval-style '(setq brushup-test--eval-target 42))
  (should (= 42 brushup-test--eval-target)))

(ert-deftest brushup-test-eval-style/handles-error ()
  "Should not signal an error."
  (brushup--eval-style '(error "test error")))

;;;; brushup-init

(ert-deftest brushup-test-init/sets-palette-variables ()
  "After init, palette variables should be strings."
  (brushup-init)
  (should (stringp brushup-fg))
  (should (stringp brushup-bg))
  (should (stringp brushup-fg-1))
  (should (stringp brushup-bg-1))
  (should (stringp brushup-bg-1_0))
  (should (booleanp brushup-dark-p)))

(ert-deftest brushup-test-init/fg-gradient-count ()
  "Should produce 6 foreground gradient levels."
  (brushup-init)
  (let ((fg-vars (list brushup-fg-1 brushup-fg-2 brushup-fg-3
                       brushup-fg-4 brushup-fg-5 brushup-fg-6)))
    (should (= 6 (length fg-vars)))
    (dolist (v fg-vars)
      (should (stringp v)))))

(ert-deftest brushup-test-init/bg-gradient-count ()
  "Should produce 6 background gradient levels."
  (brushup-init)
  (let ((bg-vars (list brushup-bg-1 brushup-bg-2 brushup-bg-3
                       brushup-bg-4 brushup-bg-5 brushup-bg-6)))
    (should (= 6 (length bg-vars)))
    (dolist (v bg-vars)
      (should (stringp v)))))

;;;; brushup (main entry point)

(defvar brushup-test--marker nil
  "Marker variable for brushup style evaluation tests.")

(ert-deftest brushup-test-brushup/evaluates-styles ()
  "brushup should evaluate all registered styles."
  (setq brushup-test--marker nil)
  (let ((brushup-styles '((setq brushup-test--marker t))))
    (brushup)
    (should brushup-test--marker)))

(ert-deftest brushup-test-brushup/empty-styles ()
  "brushup should handle empty styles list."
  (let ((brushup-styles '()))
    (brushup)))

;;; brushup-test.el ends here
