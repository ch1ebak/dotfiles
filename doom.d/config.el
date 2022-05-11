(when (display-graphic-p)
  (require 'all-the-icons))
;; or
(use-package all-the-icons
  :if (display-graphic-p))

(setq browse-url-browser-function 'eww-browse-url)

;; https://stackoverflow.com/questions/9547912/emacs-calendar-show-more-than-3-months
(defun kd/year-calendar (&optional year)
  (interactive)
  (require 'calendar)
  (let* (
      (current-year (number-to-string (nth 5 (decode-time (current-time)))))
      (month 0)
      (year (if year year (string-to-number (format-time-string "%Y" (current-time))))))
    (switch-to-buffer (get-buffer-create calendar-buffer))
    (when (not (eq major-mode 'calendar-mode))
      (calendar-mode))
    (setq displayed-month month)
    (setq displayed-year year)
    (setq buffer-read-only nil)
    (erase-buffer)
    ;; horizontal rows
    (dotimes (j 4)
      ;; vertical columns
      (dotimes (i 3)
        (calendar-generate-month
          (setq month (+ month 1))
          year
          ;; indentation / spacing between months
          (+ 5 (* 25 i))))
      (goto-char (point-max))
      (insert (make-string (- 10 (count-lines (point-min) (point-max))) ?\n))
      (widen)
      (goto-char (point-max))
      (narrow-to-region (point-max) (point-max)))
    (widen)
    (goto-char (point-min))
    (setq buffer-read-only t)))

(defun kd/scroll-year-calendar-forward (&optional arg event)
  "Scroll the yearly calendar by year in a forward direction."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (unless arg (setq arg 0))
  (save-selected-window
    (if (setq event (event-start event)) (select-window (posn-window event)))
    (unless (zerop arg)
      (let* (
              (year (+ displayed-year arg)))
        (kd/year-calendar year)))
    (goto-char (point-min))
    (run-hooks 'calendar-move-hook)))

(defun kd/scroll-year-calendar-backward (&optional arg event)
  "Scroll the yearly calendar by year in a backward direction."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (kd/scroll-year-calendar-forward (- (or arg 1)) event))

(map! :leader
      :desc "Scroll year calendar backward" "<left>" #'kd/scroll-year-calendar-backward
      :desc "Scroll year calendar forward" "<right>" #'kd/scroll-year-calendar-forward)

(defalias 'year-calendar 'kd/year-calendar)

(require 'calibredb)
(setq calibredb-root-dir "~/Calibre Library")
(setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
(setq calibredb-library-alist '(("~/Calibre Library")))
(setq sql-sqlite-program "/usr/bin/sqlite3")
(setq calibredb-program "/usr/bin/calibredb")
(setq calibredb-id-width 4)
(setq calibredb-size-show t)
(setq calibredb-format-all-the-icons t)
(setq calibredb-fetch-metadata-source-list '("Goodreads" "Amazon.com"))

(use-package dashboard
  :init      ;; tweak dashboard config before loading it
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "\nKEYBINDINGS:\
\nFind file               (SPC .)     \
Open buffer list    (SPC b i)\
\nFind recent files       (SPC f r)   \
Open the eshell     (SPC e s)\
\nOpen dired file manager (SPC d d)   \
List of keybindings (SPC h b b)")
  (setq dashboard-startup-banner "~/.doom.d/themes/doomEmacs.svg")  ;; use custom image as banner
  (setq dashboard-center-content nil) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 5)
                          (agenda . 5 )))
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book"))))

(setq doom-fallback-buffer "*dashboard*")

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
(setq dired-open-extensions '(("gif" . "sxiv")
                              ("jpg" . "sxiv")
                              ("png" . "sxiv")
                              ("mkv" . "vlc")
                              ("mp4" . "vlc")))
(setq delete-by-moving-to-trash t
      trash-directory "~/.local/share/Trash/files/")

(require 'elfeed-goodies)
(elfeed-goodies/setup)
(setq elfeed-goodies/entry-pane-size 0.5)
(add-hook 'elfeed-show-mode-hook 'visual-line-mode)
(setq elfeed-feeds (quote
                    (("https://postep.org.pl/feed" based)
                     ("https://hyperreal.info/rss.xml" based)
                     ("http://feeds.soundcloud.com/users/soundcloud:users:284471201/sounds.rss" based)
                     ("http://strajk.eu/feed/" based)
                     ("http://goodereader.com/blog/feed/" ereader)
                     ("http://feeds.the-ebook-reader.com/feedburner/cmWU" ereader)
                     ("https://swiatczytnikow.pl/" ereader)
                     ("https://climateandeconomy.com/feed/" news)
                     ("http://codziennikfeministyczny.pl/feed/" news)
                     ("http://queer.pl/rss/" news)
                     ("https://odrodzenie.fr/feed/" news)
                     ("http://feeds.feedburner.com/niebezpiecznik/" security)
                     ("https://feeds.feedburner.com/TheHackersNews" security)
                     ("http://feeds.feedburner.com/Torrentfreak" security)
                     ("https://zaufanatrzeciastrona.pl/feed/" security)
                     ("https://www.androidpolice.com/feed/" tech)
                     ("https://opensource.com/rss.xml" tech)
                     ("http://www.antipsychiatry.org/" psychiatry)
                     ("https://antipsychiatry.net/" psychiatry)
                     ("https://distrowatch.com/news/dw.xml" linux)
                     ("https://feeds.feedburner.com/ItsFoss" linux)
                     ("https://www.linuxjournal.com/" linux)
                     ("https://stare.pro/" tech)
                     ("https://antyweb.pl" tech)
                     ("https://www.masteringemacs.org/" emacs)
                     ("https://jembendell.com/" nature)
                     ("https://consensus911.org/" conspiracy)
                     ("https://gamingonlinux.com/" linux)
                     ("https://9to5linux.com/" linux)
                     ("https://lwn.net/" linux)
                     ("https://omgubuntu.co.uk/" linux)
                     ("https://existentialcomics.com/rss.xml" comics)
                     ("https://www.g-central.com/feed/" watch))))

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(map! :leader
      (:prefix ("e". "evaluate/ERC/EWW")
       :desc "Launch ERC with TLS connection" "E" #'erc-tls))

(setq erc-server "irc.libera.chat"
      erc-nick "anilorak"
      erc-track-shorten-start 24
      erc-autojoin-channels-alist '(("irc.libera.chat" "#emacs" "#linux"))
      erc-kill-buffer-on-part t
      erc-fill-column 100
      erc-fill-function 'erc-fill-static
      erc-fill-static-center 20
      ;; erc-auto-query 'bury
      )

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(setq doom-font (font-spec :family "mononoki Nerd Font" :size 12)
      doom-variable-pitch-font (font-spec :family "Cantarell" :size 12)
      doom-big-font (font-spec :family "mononoki Nerd Font" :size 20))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))
;; (def-package! highlight-indent-guides
  ;; :commands highlight-indent-guides-mode
  ;; :hook (prog-mode . highlight-indent-guides-mode)
  ;; :config
  ;; (setq highlight-indent-guides-method 'character
        ;; highlight-indent-guides-character ?/->
        ;; highlight-indent-guides-delay 0.01
        ;; highlight-indent-guides-responsive 'top
        ;; highlight-indent-guides-auto-enabled nil
        ;; ))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
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

(use-package mastodon
  :ensure t)
(setq mastodon-instance-url "https://mastodon.social"
      mastodon-active-user "2137")

(defun toggle-maximize-buffer () "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))
;; Bind it to a key.
(global-set-key [(super shift return)] 'toggle-maximize-buffer)

(require 'org-mime)
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e/")
(require 'mu4e)
(setq mu4e-maildir (expand-file-name "~/Maildir"))
; get mail
(setq mu4e-get-mail-command "mbsync -c ~/.emacs.d/mu4e/.mbsyncrc -a"
  ;; mu4e-html2text-command "w3m -T text/html" ;;using the default mu4e-shr2text
  mu4e-view-prefer-html t
  mu4e-update-interval 180
  mu4e-headers-auto-update t
  mu4e-compose-signature-auto-include nil
  mu4e-compose-format-flowed t)
;; to view selected message in the browser, no signin, just html mail
(add-to-list 'mu4e-view-actions
  '("ViewInBrowser" . mu4e-action-view-in-browser) t)
;; enable inline images
(setq mu4e-view-show-images t)
;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))
;; every new email composition gets its own frame!
(setq mu4e-compose-in-new-frame t)
;; don't save message to Sent Messages, IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)
(add-hook 'mu4e-view-mode-hook #'visual-line-mode)
;; <tab> to navigate to links, <RET> to open them in browser
(add-hook 'mu4e-view-mode-hook
  (lambda()
;; try to emulate some of the eww key-bindings
(local-set-key (kbd "<RET>") 'mu4e~view-browse-url-from-binding)
(local-set-key (kbd "<tab>") 'shr-next-link)
(local-set-key (kbd "<backtab>") 'shr-previous-link)))
;; from https://www.reddit.com/r/emacs/comments/bfsck6/mu4e_for_dummies/elgoumx
(add-hook 'mu4e-headers-mode-hook
      (defun my/mu4e-change-headers ()
        (interactive)
        (setq mu4e-headers-fields
              `((:human-date . 25) ;; alternatively, use :date
                (:flags . 6)
                (:from . 22)
                (:thread-subject . ,(- (window-body-width) 70)) ;; alternatively, use :subject
                (:size . 7)))))
;; if you use date instead of human-date in the above, use this setting
;; give me ISO(ish) format date-time stamps in the header list
;(setq mu4e-headers-date-format "%Y-%m-%d %H:%M")
;; spell check
(add-hook 'mu4e-compose-mode-hook
    (defun my-do-compose-stuff ()
       "My settings for message composition."
       (visual-line-mode)
       (org-mu4e-compose-org-mode)
           (use-hard-newlines -1)
       (flyspell-mode)))
(require 'smtpmail)
;;rename files when moving
;;NEEDED FOR MBSYNC
(setq mu4e-change-filenames-when-moving t)
;;set up queue for offline email
;;use mu mkdir  ~/Maildir/acc/queue to set up first
(setq smtpmail-queue-mail nil)  ;; start in normal mode
;;from the info manual
(setq mu4e-attachment-dir  "~/Pobrane")
(setq message-kill-buffer-on-exit t)
(setq mu4e-compose-dont-reply-to-self t)
(require 'org-mu4e)
;; convert org mode to HTML automatically
(setq org-mu4e-convert-to-html t)
;;from vxlabs config
;; show full addresses in view message (instead of just names)
;; toggle per name with M-RET
(setq mu4e-view-show-addresses 't)
;; don't ask when quitting
(setq mu4e-confirm-quit nil)
;; mu4e-context
(setq mu4e-context-policy 'pick-first)
(setq mu4e-compose-context-policy 'always-ask)
(setq mu4e-contexts
  (list
   (make-mu4e-context
    :name "work" ;;for acc1-gmail
    :enter-func (lambda () (mu4e-message "Entering context work"))
    :leave-func (lambda () (mu4e-message "Leaving context work"))
    :match-func (lambda (msg)
                  (when msg
                (mu4e-message-contact-field-matches
                 msg '(:from :to :cc :bcc) "yellowparenti@disroot.org")))
    :vars '((user-mail-address . "yellowparenti@disroot.org")
            (user-full-name . "User Account1")
            (mu4e-sent-folder . "/acc1-gmail/[acc1].Sent Mail")
            (mu4e-drafts-folder . "/acc1-gmail/[acc1].drafts")
            (mu4e-trash-folder . "/acc1-gmail/[acc1].Bin")
            (mu4e-compose-signature . (concat "Formal Signature\n" "Emacs 25, org-mode 9, mu4e 1.0\n"))
            (mu4e-compose-format-flowed . t)
            (smtpmail-queue-dir . "~/Maildir/acc1-gmail/queue/cur")
            (message-send-mail-function . smtpmail-send-it)
            (smtpmail-smtp-user . "acc1")
            (smtpmail-starttls-credentials . (("smtp.disroot.org" 587 nil nil)))
            (smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg"))
            (smtpmail-default-smtp-server . "smtp.disroot.org")
            (smtpmail-smtp-server . "smtp.disroot.org")
            (smtpmail-smtp-service . 587)
            (smtpmail-debug-info . t)
            (smtpmail-debug-verbose . t)
            (mu4e-maildir-shortcuts . ( ("/acc1-gmail/INBOX"            . ?i)
                                        ("/acc1-gmail/[acc1].Sent Mail" . ?s)
                                        ("/acc1-gmail/[acc1].Bin"       . ?t)
                                        ("/acc1-gmail/[acc1].All Mail"  . ?a)
                                        ("/acc1-gmail/[acc1].Starred"   . ?r)
                                        ("/acc1-gmail/[acc1].drafts"    . ?d)
                                        ))))))

(setq org-books-file "~/Dokumenty/org/my-list.org")

(after! org
  (setq org-directory "~/Dokumenty/org/"
        org-log-done 'time
        org-todo-keywords        ; This overwrites the default Doom org-todo-keywords
          '((sequence
             "TODO(t)"           ; A task that is ready to be tackled
             "WAIT(w)"           ; Something is holding up this task
             "|"                 ; The pipe necessary to separate "active" states and "inactive" states
             "DONE(d)"           ; Task has been completed
             "CANCELLED(c)" )))) ; Task has been cancelled

(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'tree))

(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

 (use-package org
   :ensure org-plus-contrib)
 (use-package org-notify
   :ensure nil
   :after org
   :config
   (org-notify-start))

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
  (org-roam-dailies-capture-templates
    '(("d" "default" entry "* %<%I:%M %p>: %?"
       :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  (org-roam-capture-templates
  '(("d" "default" plain
     "%?"
     :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
     :unnarrowed t))
    ("b" "book notes" plain (file "~/Dokumenty/org/org-roam/templates/BookNoteTemplate.org")
     :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
     :unnarrowed t)
    ("p" "project" plain "~/Dokumenty/org/org-roam/templates/ProjectTemplate.org"
     :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Project")
     :unnarrowed t))
   :bind (("C-c n l" . org-roam-buffer-toggle)
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

(add-hook 'org-mode-hook 'pandoc-mode)

(setq shell-file-name "/bin/fish")

;; (setq doom-theme 'catppuccin)
;; (setq doom-theme 'doom-dracula)
;; (setq doom-theme 'doom-nord)
(setq doom-theme 'graphite)
(setq fancy-splash-image "~/.doom.d/themes/doomEmacs.svg")

(require 'twittering-mode)
      (setq twittering-use-master-password t)
      (setq twittering-cert-file "/etc/ssl/certs/ca-bundle.crt")
      (setq twittering-allow-insecure-server-cert t)
      (setq twittering-icon-mode t)
      (setq twittering-use-icon-storage t)
      (setq twittering-display-remaining t)
(defalias 'epa--decode-coding-string 'decode-coding-string)

(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)

(setq frame-resize-pixelwise t)
(setq display-line-numbers-type t)
(setq org-hide-emphasis-markers t)
