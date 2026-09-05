;;; brushup.el --- Dynamic theme-aware color palette -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Charlie Holland

;; Author: Charlie Holland <mister.chiply@gmail.com>
;; Maintainer: Charlie Holland <mister.chiply@gmail.com>
;; URL: https://github.com/chiply/brushup
;; x-release-please-start-version
;; Version: 0.1.8
;; x-release-please-end
;; Package-Requires: ((emacs "29.1"))
;; Keywords: faces, themes
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Brushup provides a dynamic color palette that adapts to any Emacs theme.
;; It generates gradient colors from the current theme's foreground and
;; background, allowing package face customizations to work consistently
;; across themes.
;;
;; To enable, add to your init file:
;;   (brushup-mode 1)
;;
;; Register style forms that use brushup palette variables:
;;   (add-to-list 'brushup-styles
;;     '(set-face-attribute 'some-face nil :background brushup-bg-2))
;;
;; Or with use-package:
;;   :brushup
;;   (add-to-list 'brushup-styles '(...))
;;
;; The styles are re-evaluated whenever the theme changes.
;;
;; Available palette variables:
;;   brushup-fg, brushup-bg       - Theme foreground/background
;;   brushup-fg-1 to brushup-fg-6 - Foreground gradient (toward bg)
;;   brushup-bg-1 to brushup-bg-6 - Background gradient (toward fg)
;;   brushup-bg-1_0               - Subtle bg shift (for solaire-like effects)
;;   brushup-dark-p               - t if current theme is dark

;;; Code:

(require 'cl-lib)
(require 'color)

;;;; Configuration

(defgroup brushup nil
  "Dynamic theme-aware color palette."
  :group 'faces
  :prefix "brushup-")

(defcustom brushup-gradient-step 7
  "Percentage step for each gradient level."
  :type 'integer
  :group 'brushup)

;;;; Palette variables

(defvar brushup-dark-p t "Non-nil if current theme is dark.")
(defvar brushup-fg "#ffffff" "Theme foreground color.")
(defvar brushup-bg "#000000" "Theme background color.")

;; Foreground gradient (1=slight, 6=strong shift toward bg)
(defvar brushup-fg-1 "#e6e6e6"
  "Foreground gradient level 1 (shifted toward background).")
(defvar brushup-fg-2 "#cccccc"
  "Foreground gradient level 2 (shifted toward background).")
(defvar brushup-fg-3 "#b3b3b3"
  "Foreground gradient level 3 (shifted toward background).")
(defvar brushup-fg-4 "#999999"
  "Foreground gradient level 4 (shifted toward background).")
(defvar brushup-fg-5 "#808080"
  "Foreground gradient level 5 (shifted toward background).")
(defvar brushup-fg-6 "#666666"
  "Foreground gradient level 6 (shifted toward background).")

;; Background gradient (1=slight, 6=strong shift toward fg)
(defvar brushup-bg-1 "#1a1a1a"
  "Background gradient level 1 (shifted toward foreground).")
(defvar brushup-bg-1_0 "#0d0d0d" "Subtle bg shift for solaire-like effects.")
(defvar brushup-bg-2 "#333333"
  "Background gradient level 2 (shifted toward foreground).")
(defvar brushup-bg-3 "#4d4d4d"
  "Background gradient level 3 (shifted toward foreground).")
(defvar brushup-bg-4 "#666666"
  "Background gradient level 4 (shifted toward foreground).")
(defvar brushup-bg-5 "#808080"
  "Background gradient level 5 (shifted toward foreground).")
(defvar brushup-bg-6 "#999999"
  "Background gradient level 6 (shifted toward foreground).")

;;;; Core functions

(defun brushup--color-valid-p (color)
  "Return non-nil if COLOR is a valid, specified color."
  (and color
       (not (string-prefix-p "unspecified" (format "%s" color)))
       (color-name-to-rgb color)))

(defun brushup--theme-dark-p ()
  "Return non-nil if current theme appears dark."
  (let ((bg (face-background 'default)))
    (when (brushup--color-valid-p bg)
      (let ((lum (nth 2 (apply #'color-rgb-to-hsl (color-name-to-rgb bg)))))
        (< lum 0.5)))))

(defun brushup--generate-gradient (base-color steps direction)
  "Generate gradient colors from BASE-COLOR.
STEPS is the number of gradient levels.
DIRECTION is 1 for lighter, -1 for darker."
  (let ((step brushup-gradient-step))
    (cl-loop for i from 1 to steps
             collect (color-lighten-name base-color (* i step direction)))))

;;;###autoload
(defun brushup-init ()
  "Initialize brushup palette from current theme colors.
Called automatically on startup and theme changes."
  (let ((fg (face-attribute 'default :foreground))
        (bg (face-attribute 'default :background)))
    (when (and (brushup--color-valid-p fg)
               (brushup--color-valid-p bg))
      (setq brushup-fg (if (equal fg "black") "#000000" fg)
            brushup-bg (if (equal bg "white") "#ffffff" bg)
            brushup-dark-p (brushup--theme-dark-p))
      (let ((fg-gradient (brushup--generate-gradient
                          brushup-fg 6 (if brushup-dark-p -1 1))))
        (setq brushup-fg-1 (nth 0 fg-gradient)
              brushup-fg-2 (nth 1 fg-gradient)
              brushup-fg-3 (nth 2 fg-gradient)
              brushup-fg-4 (nth 3 fg-gradient)
              brushup-fg-5 (nth 4 fg-gradient)
              brushup-fg-6 (nth 5 fg-gradient)))
      (let ((bg-gradient (brushup--generate-gradient
                          brushup-bg 6 (if brushup-dark-p 1 -1))))
        (setq brushup-bg-1 (nth 0 bg-gradient)
              brushup-bg-2 (nth 1 bg-gradient)
              brushup-bg-3 (nth 2 bg-gradient)
              brushup-bg-4 (nth 3 bg-gradient)
              brushup-bg-5 (nth 4 bg-gradient)
              brushup-bg-6 (nth 5 bg-gradient)))
      (setq brushup-bg-1_0 (color-lighten-name brushup-bg -3)))))

;;;; Style system

(defvar brushup-styles '()
  "List of forms to evaluate when brushup runs.
Each form typically calls `set-face-attribute' using brushup palette variables.")

(defun brushup--eval-style (form)
  "Safely evaluate a brushup style FORM."
  (condition-case err
      (eval form t)
    (error (message "brushup: error in style: %s" (error-message-string err)))))

;;;###autoload
(defun brushup ()
  "Refresh all brushup styles.
Re-initializes the palette, then evaluates all registered styles.

`brushup-init' is called directly rather than being left to its place in
`brushup-styles'.  Styles are added with `add-to-list', which PREPENDS, so
every style registered after this file loaded ran BEFORE the palette was
recomputed -- reading the previous theme's `brushup-fg' and friends, and
leaving faces one theme change behind.  Callers that re-registered a style
by appending happened to work; everything else did not."
  (interactive)
  (brushup-init)
  (mapc #'brushup--eval-style brushup-styles))

;; Kept registered for callers that evaluate `brushup-styles' themselves,
;; and harmless now that `brushup' runs it up front: re-initializing the
;; palette twice is idempotent.
(add-to-list 'brushup-styles '(brushup-init))

;;;; Font normalization

(defun brushup--normalize-fonts ()
  "Remove italic slant from all faces."
  (mapc (lambda (face)
          (when (memq (face-attribute face :slant nil t) '(italic oblique))
            (set-face-attribute face nil :slant 'normal)))
        (face-list)))

(add-to-list 'brushup-styles '(brushup--normalize-fonts))

;;;; use-package integration
;; `use-package' is built-in since Emacs 29.1.

(require 'use-package-core)

(defun use-package-handler/:brushup (name _keyword arg rest state)
  "Handler for the `:brushup' `use-package' keyword.
NAME, ARG, REST, and STATE are as required by `use-package'."
  (use-package-concat
   (use-package-process-keywords name rest state)
   arg))

(defalias 'use-package-normalize/:brushup #'use-package-normalize-forms)
(add-to-list 'use-package-keywords :brushup t)

;;;; Minor mode

(defun brushup--on-theme-change (_theme)
  "Re-apply brushup on theme change."
  (brushup))

;;;###autoload
(define-minor-mode brushup-mode
  "Global minor mode for theme-aware color palette.
When enabled, initializes the brushup palette and re-applies
styles automatically on startup and theme changes."
  :global t
  :group 'brushup
  :lighter nil
  (if brushup-mode
      (progn
        (add-hook 'enable-theme-functions #'brushup--on-theme-change)
        (brushup))
    (remove-hook 'enable-theme-functions #'brushup--on-theme-change)))

(provide 'brushup)

;;; brushup.el ends here
