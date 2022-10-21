;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; https://github.com/ch1ebak/ ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq frame-resize-pixelwise t)
(setq display-line-numbers-type t)
(setq org-hide-emphasis-markers t)
(setq org-startup-with-inline-images t)
(global-set-key (kbd "C-x w") 'delete-frame)

;; Auth gpg
(setq auth-sources '("~/Dokumenty/tajne/.authinfo.gpg"))

;; Beacon mode
(beacon-mode 1)

;; Browser
;; (setq browse-url-browser-function 'eww-browse-url)

;; Conf unix mode
(setq conf-unix-mode t)

;; Modeline
(setq doom-modeline-buffer-name t)

;; Nov mode
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; Org tempo
(require 'org-tempo)

;; Pocket
;; (require 'pocket-reader)

;; Shell
(setq shell-file-name "/usr/bin/fish")

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

(defun my-open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-file-source "scheduled" "~/Dokumenty/org/org-roam/20220726170250-scheduled.org" "Green")
    (cfw:org-create-file-source "archive" "~/Dokumenty/org/org-roam/20220726170420-archive.org" "Cyan")
    (cfw:org-create-file-source "important" "~/Dokumenty/org/org-roam/20220515174922-important_dates.org" "Blue")
    ;; (cfw:ical-create-source "gcal" "https://..../basic.ics" "IndianRed") ; google calendar ICS
   )))

 (use-package counsel
   :bind (("C-M-j" . 'counsel-switch-buffer)
          :map minibuffer-local-map
          ("C-r" . 'counsel-minibuffer-history))
   :custom
   (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
   :config
   (counsel-mode 1))
(global-set-key (kbd "C-c k") 'counsel-rg)

(require 'dimmer)
(dimmer-configure-which-key)
(dimmer-configure-helm)
(dimmer-mode t)

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
                              ("pdf" . "zathura")
                              ("flac" . "mpv")
                              ("avi" . "mpv")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv")))
(setq delete-by-moving-to-trash t
      trash-directory "~/.local/share/Trash/files/")
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(require 'elfeed-goodies)
(elfeed-goodies/setup)
(setq elfeed-goodies/entry-pane-size 0.5)
;; (add-hook 'elfeed-show-mode-hook 'visual-line-mode)
(add-hook 'elfeed-search-mode-hook #'elfeed-update)
(defun elfeed-mark-all-as-read ()
      (interactive)
      (mark-whole-buffer)
      (elfeed-search-untag-all-unread))
(setq rmh-elfeed-org-files (list "~/.doom.d/elfeed.org"))

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

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         ("C-S-o" . counsel-rhythmbox)
         ("C-{" . counsel-rhythmbox-playpause-current-song)
         :map ivy-minibuffer-map
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
        :vars '((user-mail-address . "k.derwich96@gmail.com")
                (user-full-name    . "Karolina Derwich")
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
        :vars '((user-mail-address . "k.derwich@outlook.com")
                (user-full-name    . "Karolina Derwich")
                (smtpmail-smtp-server  . "smtp-mail.outlook.com")
                (smtpmail-smtp-service . 587)
                (smtpmail-stream-type  . ssl)
                (mu4e-drafts-folder  . "/outlook/Drafts")
                (mu4e-sent-folder  . "/outlook/Sent Items")
                (mu4e-refile-folder  . "/outlook/Inbox")
                (mu4e-trash-folder  . "/outlook/Deleted Items"))))))

(mu4e-alert-set-default-style 'libnotify)
(add-hook 'after-init-hook #'mu4e-alert-enable-notifications)

(after! org
  (setq org-directory "~/Dokumenty/org/"
        org-log-done 'time
        org-agenda-include-all-todo t
        org-todo-keywords        ; This overwrites the default Doom org-todo-keywords
          '((sequence
             "TODO(t)"           ; A task that is ready to be tackled
             "WAIT(w)"           ; Something is holding up this task
             "|"                 ; The pipe necessary to separate "active" states and "inactive" states
             "DONE(d)"           ; Task has been completed
             "CANCELLED(c)" )))) ; Task has been cancelled
  (setq org-agenda-files
        '("~/Dokumenty/org/org-roam/20220726170250-scheduled.org"
          "~/Dokumenty/org/org-roam/20220726170420-archive.org"
          "~/Dokumenty/org/org-roam/20220726155331-todo.org"
          "~/Dokumenty/org/org-roam/20220515174922-important_dates.org"))
  (let ((org-super-agenda-groups
       '((:auto-category t))))
        (org-agenda-list))

(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Dokumenty/org/org-roam/20220914141051-todo.org" "TODOs")
         "* TODO %?\n  %i\n  %a")
        ("s" "Scratchpad" entry (file+datetree "~/Dokumenty/org/org-roam/20220914141105-scratchpad.org")
         "* %?\nEntered on %U\n  %i\n  %a")))

(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'tree))

(setq org-modules '(org-habit
                    org-habit-plus))
(require 'org-habit)

(org-notifications-start)

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Dokumenty/org/org-roam")
  (setq org-roam-dailies-directory "~/Dokumenty/org/org-roam/daily")
  (custom-set-faces
    '((org-roam-link org-roam-link-current)
     :foreground "#e24888" :underline t))
  (org-roam-completion-everywhere t)
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   :map org-mode-map
   ("C-M-i" . completion-at-point)
   :map org-roam-dailies-map
   ("Y" . org-roam-dailies-capture-yesterday)
   ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode)
  (org-roam-setup))

(require 'org-sticky-header)

(use-package spell-fu)
(global-spell-fu-mode)
(add-hook 'spell-fu-mode-hook
  (lambda ()
    (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "pl"))
    (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "en"))
    (spell-fu-dictionary-add
      (spell-fu-get-personal-dictionary "pl-personal" "~/.config/enchant/pl_PL.dic"))
    (spell-fu-dictionary-add
      (spell-fu-get-personal-dictionary "en-personal" "~/.config/enchant/en.dic"))))

(defun insert-todays-date (arg)
  (interactive "U")
  (insert (if arg
          (format-time-string "%d-%m-%Y")
          (format-time-string "%Y-%m-%d"))))

;; (setq doom-theme 'doom-catppuccin)
(setq doom-theme 'doom-dracula)
;; (setq doom-theme 'everforest-hard-dark)
;; (setq doom-theme 'doom-gruvbox)
;; (setq doom-theme 'doom-nord)
;; (setq doom-theme 'doom-one)
;; (setq doom-theme 'doom-solarized-dark)
;; (setq doom-theme 'doom-tokyo-night)

;; (setq doom-theme 'modus-vivendi)

;; (setq modus-themes-mode-line '(borderless))
;; (setq modus-themes-region '(bg-only))
;; (setq modus-themes-completions 'minimal)
;; (setq modus-themes-bold-constructs t)
;; (setq modus-themes-italic-constructs t)
;; (setq modus-themes-paren-match '(bold intense))
;; (setq modus-themes-syntax '(alt-syntax faint))
;; (setq modus-themes-headings
   ;; '((1 . (rainbow 1.1))
     ;; (2 . (rainbow 1.1))
     ;; (3 . (rainbow 1.1))
     ;; (t . (semilight 1.0))))
;; (setq modus-themes-scale-headings t)
;; (setq modus-themes-org-blocks 'gray-background)
;; (load-theme 'modus-vivendi t)

(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
(defun td/visual-fill-setup ()
  (setq-local visual-fill-column-width 150
              visual-fill-column-center-text nil)
  (visual-fill-column-mode 1))
