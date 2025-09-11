;;; poimandres-theme.el --- A dark theme inspired by poimandres dark theme. -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: kamm3r <https://github.com/kamm3r>
;; Maintainer: kamm3r <https://github.com/kamm3r>
;; Source: https://github.com/drcmda/poimandres-theme
;;
;;; Commentary:
;; Poimandres is a minimal, frameless dark-theme inspired mostly
;; by blueberry. This theme tries to focus on semantic meaning instead
;; of color variety. You'll find that it colors things like errors, voids, 
;; throws and deletes in red, types are slighty darker so that the spotlight 
;; is on the code, green new's, etc.
;;; Code:

(deftheme poimandres
  "A dark theme inspired by poimandres dark theme.")

(defun poimandres-color-clamp-lab (lab)
  "Restricts a LAB colorspace color if it is out of bounds."
  (list (min (max (nth 0 lab) 0.0) 100.0)
        (min (max (nth 1 lab) -128) 127)
        (min (max (nth 2 lab) -128) 127)))

(defun poimandres-color-rgb-to-hex (red green blue &optional digits-per-component round)
  "Return hexadecimal #RGB notation for the color specified by RED GREEN BLUE.
RED, GREEN, and BLUE should be numbers between 0.0 and 1.0, inclusive.
Optional argument DIGITS-PER-COMPONENT can be either 4 (the default)
or 2; use the latter if you need a 24-bit specification of a color.
Optional argument ROUND rounds values which probably is what you usually want."
  (or digits-per-component (setq digits-per-component 4))
  (let* ((maxval (if (= digits-per-component 2) 255 65535))
         (fmt (if (= digits-per-component 2) "#%02x%02x%02x" "#%04x%04x%04x")))
    (if round
        (format fmt (+ 0.5 (* red maxval)) (+ 0.5 (* green maxval)) (+ 0.5(* blue maxval)))
        (format fmt (* red maxval) (* green maxval) (* blue maxval)))))

;;;###autoload
(defun poimandres-color-blend (color1 color2 alpha &optional digits-per-component)
  "Blends COLOR1 onto COLOR2 with ALPHA.

COLOR1 and COLOR2 should be color names (e.g. \"white\") or RGB
triplet strings (e.g. \"#ff12ec\").

Alpha should be a float between 0 and 1.

Optional argument DIGITS-PER-COMPONENT can be either 4 (the default) or 2;
use the latter if you need a 24-bit specification of a color."
  (let ((args (mapcar 'color-clamp
                      (apply 'color-lab-to-srgb
                             (poimandres-color-clamp-lab
                              (cl-mapcar
                               (lambda (v1 v2) (+ v1 (* alpha (- v2 v1))))
                               (apply 'color-srgb-to-lab (color-name-to-rgb color2))
                               (apply 'color-srgb-to-lab (color-name-to-rgb color1))))))))
    (apply 'poimandres-color-rgb-to-hex `(,@args ,digits-per-component t))))



(let* ((class '((class color) (min-colors 89)))
       (256color (eq (display-color-cells (selected-frame)) 256))
   (brightYellow       "#fffac2")
   (brightMint         "#5DE4c7")
   (lowerMint          "#5fb3a1")
   (blueishGreen       "#42675A")
   (lowerBlue          "#89ddff")
   (lightBlue          "#ADD7FF")
   (desaturatedBlue    "#91B4D5")
   (bluishGrayBrighter "#7390AA")
   (hotRed             "#d0679d")
   (pink               "#f087bd")
   (gray               "#a6accd")
   (darkerGray         "#767c9d")
   (bluishGray         "#506477")
   (focus              "#303340")
   (bg                 "#1b1e28")
   (offWhite           "#e4f0fb")
   (selection          (poimandres-color-blend "#ffffff" "#717cb4" 0.25 2))
   (white              "#ffffff")
   (black              "#000000")
   (transparent        (poimandres-color-blend "#000000" "#ffffff" 0.00 2)))


(custom-theme-set-faces
 'poimandres
 `(default ((,class (:foreground ,gray :background ,bg))))
 `(cursor ((,class (:foreground ,gray))))
 `(fringe ((,class (:background ,bg))))
 `(hl-line ((,class (:background ,bg))))
 `(region ((,class (:background ,selection)))) ;; selection
 `(secondary-selection ((,class (:background ,selection))))
 `(minibuffer-prompt ((,class (:foreground ,lowerBlue))))
 `(vertical-border ((,class (:foreground ,lowerBlue))))
 `(internal-border ((,class (:foreground ,lowerBlue))))
 `(window-divider ((,class (:foreground ,lowerBlue))))
 `(link ((,class (:foreground ,lowerBlue))))
 `(shadow ((,class (:foreground ,darkerGray))))
 `(bold ((,class (:weight bold))))
 `(bold-italic ((,class (:slant italic :weight bold))))
 `(underline ((,class (:underline t))))
 `(italic ((,class (:slant italic))))

 ;;; Built-in syntax
 `(font-lock-builtin-face ((,class (:foreground ,gray))))
 `(font-lock-comment-face ((,class (:foreground ,darkerGray))))
 `(font-lock-constant-face ((,class (:foreground ,bluishGrayBrighter))))
 `(font-lock-function-name-face ((,class (:foreground ,lightBlue))))
 `(font-lock-keyword-face ((,class (:foreground ,brightMint))))
 `(font-lock-string-face ((,class (:foreground ,brightMint))))
 `(font-lock-number-face ((,class (:foreground ,brightMint))))
 `(font-lock-variable-name-face ((,class (:foreground ,white))))
 `(font-lock-type-face ((,class (:foreground ,lightBlue))))
 `(font-lock-property-face ((,class (:foreground ,bluishGrayBrighter))))
 `(font-lock-warning-face ((,class (:weight bold :foreground ,hotRed))))

 ;;; Basic faces
 `(error ((,class (:foreground ,hotRed))))
 `(success ((,class (:foreground ,brightMint))))
 `(warning ((,class (:foreground ,brightYellow))))
 `(alert-low-face ((,class (:foreground ,lightBlue))))
 `(trailing-whitespace ((,class (:foreground ,pink))))
 `(escape-glyph ((,class (:foreground ,lowerMint))))
 `(header-line ((,class (:background ,focus))))
 `(highlight ((,class (:background ,transparent))))
 `(homoglyph ((,class (:foreground ,brightYellow))))
 `(match ((,class (:background ,lightBlue))))

 ;;; whitespace-mode
 `(whitespace-space  ((,class (:foreground ,gray :background ,bg))))
 `(whitespace-hspace  ((,class (:foreground ,gray :background ,bg))))
 `(whitespace-tab  ((,class (:foreground ,gray :background ,bg))))
 `(whitespace-newline ((,class (:foreground ,gray :background ,bg))))
 `(whitespace-trailing  ((,class (:foreground ,pink :background ,bg))))
 `(whitespace-line   ((,class (:foreground ,pink :background ,bg))))
 `(whitespace-space-before-tab ((,class (:foreground ,gray :background ,bg))))
 `(whitespace-indentation  ((,class (:foreground ,gray :background ,bg))))
 `(whitespace-empty ((,class (:foreground ,gray :background ,bg))))
 `(whitespace-space-after-tab   ((,class (:foreground ,gray :background ,bg))))

 ;;; line numbers
 `(line-number  ((,class (:foreground ,gray :background ,bg))))
 `(line-number-current-line  ((,class (:foreground ,white :background ,bg))))
 `(linum ((,class (:foreground ,gray :background ,bg))))
 `(linum-highlight-face  ((,class (:foreground ,white :background ,bg))))
 `(linum-relative-current-face   ((,class (:foreground ,white :background ,bg))))

 ;;; borders
 `(border ((,class (:background ,bg))))
 `(border-glyph ((,class (nil))))

 ;; Mode line faces.
 `(mode-line ((,class :foreground ,gray :background ,bg :box nil)))
 `(mode-line-inactive ((,class :foreground ,darkerGray :background ,bg :box nil)))
 `(mode-line-buffer-id ((,class (:weight bold :foreground ,gray))))
 `(mode-line-emphasis ((,class (:weight bold :foreground ,brightMint))))
 `(mode-line-highlight ((,class (:foreground ,lightBlue))))

  ;; web-mode
  `(web-mode-doctype-face          (:foreground ,brightMint))
  `(web-mode-html-tag-bracket-face (:foreground ,white))
  `(web-mode-html-tag-face         (:foreground ,brightMint))
  `(web-mode-html-attr-name-face   (:foreground ,bluishGrayBrighter))
  `(web-mode-html-attr-equal-face  (:foreground ,bluishGrayBrighter))
  `(web-mode-html-attr-value-face  (:foreground ,brightMint))


 ;; doom-modeline
 `(doom-modeline-bar ((,class (:foreground ,brightMint i:box nil))))
 `(doom-modeline-info ((,class (:foreground ,brightMint))))
 `(doom-modeline-urgent ((,class (:foreground ,hotRed))))
 `(doom-modeline-warning ((,class (:foreground ,brightYellow))))
 `(doom-modeline-debug ((,class (:foreground ,focus))))
 `(doom-modeline-evil-normal-state ((,class (:background ,brightMint :foreground ,bg :weight bold))))
 `(doom-modeline-evil-insert-state ((,class (:background ,lightBlue :foreground ,bg :weight bold))))
 `(doom-modeline-evil-visual-state ((,class (:background ,brightYellow :foreground ,bg :weight bold))))
 `(doom-modeline-evil-replace-state ((,class (:background ,hotRed :foreground ,bg :weight bold))))
 `(doom-modeline-buffer-minor-mode ((,class (:background ,brightMint))))
 `(doom-modeline-project-dir ((,class (:foreground ,gray))))
 `(doom-modeline-project-parent-dir ((,class (:foreground ,gray))))
 `(doom-modeline-persp-name ((,class (:foreground ,gray))))
 `(doom-modeline-buffer-file ((,class (:foreground ,gray))))
 `(doom-modeline-buffer-modified ((,class (:foreground ,gray))))
 `(doom-modeline-buffer-path ((,class (:foreground ,gray))))
 `(doom-modeline-buffer-project-root ((,class (:foreground ,gray))))

 ;; Magit (a patch is pending in magit to make these standard upstream)
 `(magit-branch ((,class (:foreground ,brightMint))))
 `(magit-diff-add ((,class (:inherit diff-added))))
 `(magit-diff-del ((,class (:inherit diff-removed))))
 `(magit-header ((,class (:inherit nil :weight bold))))
 `(magit-item-highlight ((,class (:inherit selection :background nil))))
 `(magit-log-author ((,class (:foreground ,lightBlue))))
 `(magit-log-graph ((,class (:foreground ,darkerGray))))
 `(magit-log-head-label-bisect-bad ((,class (:foreground ,hotRed))))
 `(magit-log-head-label-bisect-good ((,class (:foreground ,brightMint))))
 `(magit-log-head-label-default ((,class (:foreground ,brightYellow :box nil :weight bold))))
 `(magit-log-head-label-local ((,class (:foreground ,pink :box nil :weight bold))))
 `(magit-log-head-label-remote ((,class (:foreground ,pink :box nil :weight bold))))
 `(magit-log-head-label-tags ((,class (:foreground ,lightBlue :box nil :weight bold))))
 `(magit-log-sha1 ((,class (:foreground ,brightYellow))))
 `(magit-section-title ((,class (:foreground ,lowerBlue :weight bold))))

  ;; ansi-colors
  `(ansi-color-black   ((,class (:foreground ,bg :background ,bg))))
  `(ansi-color-red     ((,class (:foreground ,hotRed :background ,hotRed))))
  `(ansi-color-green   ((,class (:foreground ,brightMint :background ,brightMint))))
  `(ansi-color-yellow  ((,class (:foreground ,brightYellow :background ,brightYellow))))
  `(ansi-color-blue    ((,class (:foreground ,lowerBlue :background ,lowerBlue))))
  `(ansi-color-magenta ((,class (:foreground ,pink :background ,pink))))
  `(ansi-color-cyan    ((,class (:foreground ,lowerBlue :background ,lowerBlue))))
  `(ansi-color-white   ((,class (:foreground ,white :background ,white)))))
  `(ansi-color-brightBlack   ((,class (:foreground ,gray :background ,gray))))
  `(ansi-color-brightRed     ((,class (:foreground ,hotRed :background ,hotRed))))
  `(ansi-color-brightGreen   ((,class (:foreground ,brightMint :background ,brightMint))))
  `(ansi-color-brightYellow  ((,class (:foreground ,brightYellow :background ,brightYellow))))
  `(ansi-color-brightBlue    ((,class (:foreground ,lightBlue :background ,lightBlue))))
  `(ansi-color-brightMagenta ((,class (:foreground ,pink :background ,pink))))
  `(ansi-color-brightCyan    ((,class (:foreground ,lightBlue :background ,lightBlue))))
  `(ansi-color-brightWhite   ((,class (:foreground ,white :background ,white)))))

;; (custom-theme-set-variables
;;   'poimandres
;;   '(linum-format " %3i "))

;;;###autoload
(when (and (boundp 'custom-theme-load-path)
           load-file-name)
  ;; add theme folder to `custom-theme-load-path' when installing over MELPA
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'poimandres)

;;; poimandres-theme.el ends here
