;;; doom-catppuccin-theme.el --- Based on catppuccin color palletes -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;;; Code:
(require 'doom-themes)

;; Compiler pacifier
(defvar modeline-bg)

;;
(defgroup doom-snazzy-theme nil
  "Options for the `doom-catppccin' theme."
  :group 'doom-themes)

;;
(def-doom-theme doom-catppuccin
  "Soothing pastel theme for the high-spirited!"

  ;; name        default   256       16
  ((bg         '("#332e41" "#332e41" nil          )) ;; this is the background for the hl-line, modeline, and minibuffer
   (bg-alt     '("#1e1e29" "#1e1e29" nil          )) ;; this is the background for the line you arent currently on
   (base0      '("#282a36" "#282a36" "black"      ))
   (base1      '("#34353e" "#34353e" "brightblack"))
   (base2      '("#43454f" "#43454f" "brightblack"))
   (base3      '("#78787e" "#78787e" "brightblack"))
   (base4      '("#a5a5a9" "#a5a5a9" "brightblack"))
   (base5      '("#e2e4e5" "#e2e4e5" "brightblack"))
   (base6      '("#eff0eb" "#eff0eb" "brightblack"))
   (base7      '("#f1f1f0" "#f1f1f0" "brightblack"))
   (base8      '("#ff5c57" "#ff5c57" "white"      ))
   (fg         '("#f2cecf" "#f2cecf" "white"      ))
   (fg-alt     '("#c3bac6" "#c3bac6" "brightwhite"))

   (ui0 '("#6e6c7e" "#6e6c7e" "grey"))
   (ui1 '("#988ba2" "#988ba2" "grey"))
   (ui2 '("#c3bac6" "#c3bac6" "grey"))
   (ui3 '("#15121c" "#15121c" "black"))

   (grey       ui0)
   (red        '("#e38c8f" "#e38c8f" "red"          ))
   (green      '("#b1e3ad" "#b1e3ad" "brightred"    ))
   (yellow     '("#ebddaa" "#ebddaa" "green"        ))
   (blue       '("#a4b9ef" "#a4b9ef" "brightgreen"  ))
   (dark-blue  '("#459fcc" "#459fcc" "yellow"       ))
   (magenta    '("#c6aae8" "#c6aae8" "brightblue"   ))
   (cyan       '("#988c0d0" "#88c0d0" "blue"         ))
   (violet     '("#bd93f9" "#bd93f9" "magenta"      ))
   (orange     '("#f9c096" "#f9c096" "brightmagenta"))
   (teal       '("#bee4ed" "#bee4ed" "brightcyan"   ))
   (dark-cyan  '("#81a1c1" "#81a1c1" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      blue) ;; when searching with (/) ?
   (vertical-bar   (doom-darken base1 0.1)) ; the bar that separates modeline and minibuffer?
   (selection      dark-blue)  ; for like company autocomplete and stuff
   (builtin        magenta)    ; saw this in company autocomplete if i moved my mouse
   ;; over it
   (comments       ui1) ;; comments
   (doc-comments (doom-lighten yellow 0.25)) ;; easy to test with elisp
   ;; documentation or git commit
   ;; first line thing
   (constants      green)
   (functions      blue)
   (keywords       orange)
   (methods        blue) ;; wtf is the difference between this and function?
   (operators      magenta)
   (type           cyan)
   (strings        yellow)
   ;; (variables      (doom-lighten magenta 0.4))
   (variables      red)
   (numbers        yellow)
   (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base0) 0.35)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    yellow)
   (vc-added       green)
   (vc-deleted     red))


  ;;;; Base theme face overrides
  ((line-number :foreground ui2)
   (line-number-current-line :foreground fg)
   ;; i have no idea what im doing with the modeline
   (mode-line :background (doom-darken bg-alt 0.15))
   (mode-line-inactive :background (doom-darken bg-alt 0.1) :foreground base5)
   (tooltip :background (doom-darken bg-alt 0.2) :foreground fg)

   ;;;; doom-modeline
   (doom-modeline-bar :background highlight)
   ;;;; ivy-posframe
   (ivy-posframe-border :background ui3)
   ;;;; outline <built-in>
   ((outline-3 &override) :foreground dark-blue)
   ;;;; org <built-in>
   ((org-block &override) :background bg-alt)
   ((org-block-begin-line &override) :background bg-alt)
   ((org-block-end-line &override)   :background bg-alt)
   ;;;; rjsx-mode
   (rjsx-text :foreground fg)))

;;; doom-catppuccin-theme.el ends here
