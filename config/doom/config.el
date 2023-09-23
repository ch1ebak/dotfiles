;;     ____   ____   ____   __  ___   ______ __  ___ ___    ______ _____ ;;
;;    / __ \ / __ \ / __ \ /  |/  /  / ____//  |/  //   |  / ____// ___/ ;;
;;   / / / // / / // / / // /|_/ /  / __/  / /|_/ // /| | / /     \__ \  ;;
;;  / /_/ // /_/ // /_/ // /  / /  / /___ / /  / // ___ |/ /___  ___/ /  ;;
;; /_____/ \____/ \____//_/  /_/  /_____//_/  /_//_/  |_|\____/ /____/   ;;

;; Auth gpg
(setq auth-sources '("~/Dokumenty/tajne/.authinfo.gpg"))

;; Beacon mode
(beacon-mode 1)

;; Bookmarks
(setq bookmark-default-file "~/.config/doom/bookmarks")

;; Calendar
(setq calendar-week-start-day 1)

;; Conf unix mode
(setq conf-unix-mode t)

;; Rainbow mode
(require 'rainbow-mode)
(add-hook! org-mode 'rainbow-mode)
(add-hook! prog-mode 'rainbow-mode)

;; Shell
(setq shell-file-name "/usr/bin/bash")

(use-package all-the-icons
  :if (display-graphic-p))

(setq all-the-icons-ivy-rich-icon t)
(setq all-the-icons-ivy-rich-color-icon t)
(setq all-the-icons-ivy-rich-icon-size 1.0)
(setq all-the-icons-ivy-rich-project t)
(setq all-the-icons-ivy-rich-field-width 80)
(setq inhibit-compacting-font-caches t)

(all-the-icons-ivy-rich-mode 1)
(setq all-the-icons-ivy-rich-icon t)
(setq all-the-icons-ivy-rich-color-icon t)
(setq all-the-icons-ivy-rich-project t)

(setq deft-directory "~/Dokumenty/notatki")
(after! deft
  (setq deft-default-extension "org"))

(setq dired-open-extensions '(("gif" . "nsxiv")
                              ("jpg" . "nsxiv")
                              ("png" . "nsxiv")
                              ("pdf" . "firefox")
                              ("flac" . "mpv")
                              ("avi" . "mpv")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv")))
(setq delete-by-moving-to-trash t
      trash-directory "~/.local/share/Trash/files/")

(after! elfeed
  (setq elfeed-search-filter "@2-days-ago +unread"))

(defun elfeed-mark-all-as-read ()
  (interactive)
  (elfeed-untag elfeed-search-entries 'unread)
  (elfeed-search-update :force)) ; redraw

(defun elfeed-search-format-date (date)
  (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time date)))

(require 'elfeed-org)
(elfeed-org)
(setq rmh-elfeed-org-files (list "~/.config/doom/elfeed/elfeed.org"))

(setq elfeed-goodies/entry-pane-size 0.5)

(setq erc-prompt (lambda () (concat "[" (buffer-name) "]"))
      erc-server "irc.libera.chat"
      erc-nick "papaemeritusIV"
      erc-track-shorten-start 24
      erc-autojoin-channels-alist '(("irc.libera.chat" "#archlinux" "#linux" "#emacs"))
      erc-kill-buffer-on-part t
      erc-fill-column 100
      erc-fill-function 'erc-fill-static
      erc-fill-static-center 20
      )

(setq evil-undo-system 'undo-tree)
(setq evil-respect-visual-line-mode t)

(setq
 browse-url-browser-function 'eww-browse-url
 shr-use-fonts  nil
 ;; shr-use-colors nil
 shr-indentation 2
 shr-width 70
 eww-auto-rename-buffer 1
 eww-download-directory "~/Pobrane"
 eww-search-prefix "https://frogfind.com/?q="
 browse-url-secondary-browser-function 'browse-url-firefox)

(add-hook 'eww-after-render-hook 'eww-readable)

(defun eww-new ()
  (interactive)
  (let ((url (read-from-minibuffer "Enter URL or keywords: ")))
    (switch-to-buffer (generate-new-buffer "eww"))
    (eww-mode)
    (eww url)))

(defun toggle-maximize-buffer () "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

(after! org
  (setq org-directory "~/Dokumenty/notatki/"
        org-log-done 'time
        org-agenda-include-all-todo t
        org-todo-keywords
          '((sequence
             "TODO(t)"
             "WAIT(w)"
             "|"
             "DONE(d)"
             "CANCELLED(c)" ))
        org-agenda-files
          '("~/Dokumenty/notatki/todo.org"
          "~/Dokumenty/notatki/zaplanowane.org")
        org-fancy-priorities-list '("[A]" "[B]" "[C]")
))

(setq org-ellipsis " ▾")
(setq org-hide-emphasis-markers t)
(setq org-hide-leading-stars t)
(setq org-startup-with-inline-images t)

(require 'org-sticky-header)
(require 'org-tempo)

(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'tree))

(setq pocket-reader-open-url-default-function #'eww)
(setq pocket-reader-pop-to-url-default-function #'eww)
(add-hook 'pocket-reader-mode (lambda () (display-line-numbers-mode 0)))

(use-package counsel
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package ivy
  :diminish
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
(setq ivy-rich-path-style 'abbrev)

(defun insert-todays-date (arg)
  (interactive "U")
  (insert (if arg
          (format-time-string "%d-%m-%Y")
          (format-time-string "%Y-%m-%d"))))

(defun insert-current-time (arg)
  (interactive "U")
  (insert (if arg
          (format-time-string "%R")
          (format-time-string "%H:%M"))))

(visual-line-mode 1)
(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
(setq visual-fill-column-width 200)

(setq doom-modeline-buffer-name t)
(setq doom-modeline-height 25
      ;; doom-modeline-bar-width 5
      doom-modeline-persp-name t
      doom-modeline-persp-icon t)

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font " :size 12)
      doom-variable-pitch-font (font-spec :family "Cantarell" :size 13)
      doom-big-font (font-spec :family "JetBrainsMono Nerd Font " :size 20))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

;; (setq doom-theme 'catppuccin)
;; (setq catppuccin-flavor 'macchiato)
;; (catppuccin-reload)
;; (setq doom-theme 'doom-dracula)
;; (setq doom-theme 'doom-gruvbox)
;; (setq doom-theme 'doom-nord-aurora)
(setq doom-theme 'doom-spacegrey)
;; (setq doom-theme 'doom-opera)

(evil-define-key 'normal dired-mode-map
  (kbd "M-RET") 'dired-display-file
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-open-file ; use dired-find-file instead of dired-open.
  (kbd "m") 'dired-mark
  (kbd "t") 'dired-toggle-marks
  (kbd "u") 'dired-unmark
  (kbd "C") 'dired-do-copy
  (kbd "D") 'dired-do-delete
  (kbd "J") 'dired-goto-file
  (kbd "M") 'dired-do-chmod
  (kbd "O") 'dired-do-chown
  (kbd "P") 'dired-do-print
  (kbd "R") 'dired-do-rename
  (kbd "T") 'dired-do-touch
  (kbd "Y") 'dired-copy-filenamecopy-filename-as-kill ; copies filename to kill ring.
  (kbd "+") 'dired-create-directory
  (kbd "-") 'dired-up-directory
  (kbd "% l") 'dired-downcase
  (kbd "% u") 'dired-upcase
  (kbd "; d") 'epa-dired-do-decrypt
  (kbd "; e") 'epa-dired-do-encrypt)

(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

(evil-define-key 'normal evil-org-mode-map
                 (kbd ">") 'org-metaright
                 (kbd "<") 'org-metaleft)

(global-set-key (kbd "C-x w") 'delete-frame)

(global-set-key (kbd "M-x") 'counsel-M-x)

(map! :leader
      "TAB RET" #'evil-window-vnew
      "TAB q" #'evil-window-delete
      "TAB h" #'previous-buffer
      "TAB H" #'evil-window-left
      "TAB j" #'+workspace/switch-left
      "TAB J" #'evil-window-down
      "TAB k" #'+workspace/switch-right
      "TAB K" #'evil-window-up
      "TAB l" #'next-buffer
      "TAB L" #'evil-window-right)

(map! :leader
      ("SPC" #'counsel-M-x)
      ("," #'counsel-switch-buffer)
      ("<" #'kill-buffer)
      (">" #'counsel-cd)
      ("s r" #'counsel-rg)
      ("s f" #'counsel-fzf)
      ("s s" #'counsel-linux-app)
      ("s l" #'counsel-imenu))

(map! :leader
      ("n d" #'deft)
      ("n f" #'deft-find-file)
      ("n n" #'deft-new-file-named))

(map! :leader
      "v" #'eat)

(map! :leader
      ("e e" #'elfeed)
      ("e r" #'elfeed-update)
      ("e m" #'elfeed-mark-all-as-read)
      ("e p" #'pocket-reader-add-link))

(map! :leader
      ("o e" #'erc-tls))

(map! :leader
      ("b b" #'eww-new)
      ("b s" #'eww-search-words))

(map! :leader
      "=" #'toggle-maximize-buffer)

(map! :leader
      ("o p" #'pocket-reader))

(map! :leader
      "o s" #'scratch-buffer)
