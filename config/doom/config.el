;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; https://github.com/ch1ebak/ ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq frame-resize-pixelwise t)
(setq display-line-numbers-type t)
(global-set-key (kbd "C-x w") 'delete-frame)
(evil-define-key 'normal evil-org-mode-map
                 (kbd ">") 'org-meta-right
                 (kbd "<") 'org-meta-left)

;; Auth gpg
(setq auth-sources '("~/.authinfo.gpg"))

;; Beacon mode
(beacon-mode 1)

;; Bookmarks
(setq bookmark-default-file "~/.doom.d/bookmarks")

;; Browser
;; (setq browse-url-browser-function 'eww-browse-url)

;; Conf unix mode
(setq conf-unix-mode t)

;; Modeline
(setq doom-modeline-buffer-name t)

;; Shell
(setq shell-file-name "/usr/bin/bash")

;; Visual line mode
(visual-line-mode 1)

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
(setq deft-recursive t)
(setq deft-extensions '("org" "md" "txt"))
(setq deft-default-extension "org")
(setq deft-text-mode 'org-mode)
(setq deft-auto-save-interval 0)
(setq deft-use-filename-as-title t)
(setq deft-use-filter-string-for-filename t)
(setq deft-incremental-search t)

(map! :leader
      "n r f" #'deft-find-file)

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
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(defun elfeed-mark-all-as-read ()
  (interactive)
  (elfeed-untag elfeed-search-entries 'unread)
  (elfeed-search-update :force)) ; redraw

(after! elfeed
  (setq elfeed-search-filter "@1-week-ago +unread"))

(require 'elfeed-org)
(elfeed-org)
(setq rmh-elfeed-org-files (list "~/.doom.d/elfeed/elfeed.org"))

(setq elfeed-goodies/entry-pane-size 0.5)

(setq erc-prompt (lambda () (concat "[" (buffer-name) "]"))
      erc-server "irc.libera.chat"
      erc-nick ""
      erc-track-shorten-start 24
      erc-autojoin-channels-alist '(("irc.libera.chat" "#archlinux" "#linux" "#emacs"))
      erc-kill-buffer-on-part t
      erc-fill-column 100
      erc-fill-function 'erc-fill-static
      erc-fill-static-center 20
      )

(setq evil-undo-system 'undo-tree)
(setq evil-respect-visual-line-mode t)
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font " :size 12)
      doom-variable-pitch-font (font-spec :family "Cantarell" :size 13)
      doom-big-font (font-spec :family "JetBrainsMono Nerd Font " :size 20))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(use-package ledger-mode
  :mode ("\\.dat\\'"
         "\\.ledger\\'")
  :custom (ledger-clear-whole-transactions t))

(defun toggle-maximize-buffer () "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))
(global-set-key [(super control return)] 'toggle-maximize-buffer)

(use-package mu4e
  ;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :ensure t
  :defer 10
  :config

  (setq mu4e-get-mail-command "mbsync -c ~/.doom.d/mu4e/.mbsyncrc -a")
  (setq mu4e-root-maildir (expand-file-name "~/Dokumenty/Maildir"))

  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)

  ;; Updates
  (setq mu4e-update-interval 120)
  (setq mu4e-headers-auto-update t)

  ;; Configure the function to use for sending mail
  (setq message-send-mail-function 'smtpmail-send-it)

  ;; Only ask if a context hasn't been previously picked
  (setq mu4e-compose-context-policy 'ask-if-none)

  ;; Make sure plain text mails flow correctly for recipients
  (setq mu4e-compose-format-flowed t)

  ;; enable inline images
  ;; (setq mu4e-view-show-images t)
  ;; use imagemagick, if available
  ;; (when (fboundp 'imagemagick-register-types)
    ;; (imagemagick-register-types))

  ;; Use Ivy for mu4e completions (maildir folders, etc)
  (setq mu4e-completing-read-function #'ivy-completing-read)

  ;; setup some handy shortcuts
  (setq mu4e-maildir-shortcuts
        '(("/gmail/Sent"         . ?g)
          ("/outlook/Sent Items" . ?o)))

  ;; (add-to-list 'mu4e-bookmarks
          ;; (make-mu4e-bookmark
           ;; :name "All Inboxes"
           ;; :query "maildir:/gmail/Inbox OR maildir:/outlook/Inbox"
           ;; :key ?a))

  ;; Accounts
  (setq mu4e-contexts
        (list
       ;; Private account
       (make-mu4e-context
        :name "gmail" ;; for gmail
        :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))))
        :vars '((user-mail-address . "")
                (user-full-name    . "")
                (smtpmail-smtp-server  . "smtp.gmail.com")
                (smtpmail-smtp-service . 465)
                (smtpmail-stream-type  . ssl)
                (mu4e-drafts-folder  . "/gmail/Drafts")
                (mu4e-sent-folder  . "/gmail/Sent")
                (mu4e-refile-folder  . "/gmail/Inbox")
                (mu4e-trash-folder  . "/gmail/Trash")))

       ;; Shopping account
       (make-mu4e-context
        :name "outlook" ;; for outlook
        :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/outlook" (mu4e-message-field msg :maildir))))
        :vars '((user-mail-address . "")
                (user-full-name    . "")
                (smtpmail-smtp-server  . "smtp-mail.outlook.com")
                (smtpmail-smtp-service . 587)
                (smtpmail-stream-type  . ssl)
                (mu4e-drafts-folder  . "/outlook/Drafts")
                (mu4e-sent-folder  . "/outlook/Sent Items")
                (mu4e-refile-folder  . "/outlook/Inbox")
                (mu4e-trash-folder  . "/outlook/Deleted Items"))))))

(setq org-ellipsis " ▾")
(setq org-hide-emphasis-markers t)
(setq org-hide-leading-stars t)
(setq org-startup-with-inline-images t)

(require 'org-sticky-header)
(require 'org-tempo)

(after! org
  (setq org-directory "~/Dokumenty/org/"
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
          '("~/Dokumenty/notatki/gtd.org"
          "~/Dokumenty/notatki/gtd-archiwum.org")
        org-fancy-priorities-list '("[A]" "[B]" "[C]")
))

(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

(setq org-capture-templates
      '(
        ("t" "Todo" entry (file "~/Dokumenty/notatki/todo.org")
         "* TODO %?\n %i\n  %a"
         :unnarrowed t)))

(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'tree))

;; (setq pocket-reader-open-url-default-function #'eww)
;; (setq pocket-reader-pop-to-url-default-function #'eww)
(global-set-key (kbd "C-x y") 'pocket-reader-add-link)

(use-package spell-fu)
(global-spell-fu-mode)
(add-hook 'spell-fu-mode-hook
  (lambda ()
    (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "pl"))
    (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "en"))
    (spell-fu-dictionary-add (spell-fu-get-personal-dictionary "pl-personal" "~/.config/enchant/pl_PL.dic"))
    (spell-fu-dictionary-add (spell-fu-get-personal-dictionary "en-personal" "~/.config/enchant/en.dic"))))

 (use-package counsel
   :bind (
          ;; defaults
          ("M-x" . counsel-M-x)
          ("C-c l" . counsel-imenu)
          ;; ("C-<" . 'counsel-switch-buffer)
          ;; search
          ("C-c r" . 'counsel-rg)
          ("C-c f" . 'counsel-fzf)
          ;; apps
          ("C-c s" . 'counsel-linux-app)
          :map minibuffer-local-map
          ("C-r" . 'counsel-minibuffer-history))
   :custom
   (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
   :config
   (counsel-mode 1))

(map! :leader
      ">" #'counsel-switch-buffer)

(use-package ivy
  :diminish
  :bind (:map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
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

;; (setq doom-theme 'catppuccin)
;; (setq doom-theme 'doom-dracula)
;; (setq doom-theme 'doom-gruvbox)
;; (setq doom-theme 'doom-nord-aurora)
(setq doom-theme 'doom-spacegrey)
;; (setq doom-theme 'yabaki)

;; (setq modus-themes-mode-line '(borderless (padding . 4) (height . 0.9)))
;; (setq modus-themes-region '(bg-only))
;; (setq modus-themes-completions 'minimal)
;; (setq modus-themes-bold-constructs t)
;; (setq modus-themes-italic-constructs t)
;; (setq modus-themes-paren-match '(bold intense underline))
;; (setq modus-themes-syntax '(faint))
;; (setq modus-themes-syntax '(green-strings yellow-comments))
;; (setq modus-themes-headings
    ;; '((1 . (rainbow overline))
      ;; (2 . (rainbow))
      ;; (3 . (rainbow bold))
      ;; (t . (semilight))))
;; (setq modus-themes-scale-headings t)
;; (load-theme 'modus-vivendi t)

(use-package undo-tree
  :init
  (global-undo-tree-mode 1))

(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
(setq visual-fill-column-width 200)