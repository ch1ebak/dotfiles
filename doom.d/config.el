(when (display-graphic-p)
  (require 'all-the-icons))
;; or
(use-package all-the-icons
  :if (display-graphic-p))

;; Whether display the icons
(setq all-the-icons-ivy-rich-icon t)

;; Whether display the colorful icons.
;; It respects `all-the-icons-color-icons'.
(setq all-the-icons-ivy-rich-color-icon t)

;; The icon size
(setq all-the-icons-ivy-rich-icon-size 1.0)

;; Whether support project root
(setq all-the-icons-ivy-rich-project t)

;; Maximum truncation width of annotation fields.
;; This value is adjusted depending on the `window-width'.
(setq all-the-icons-ivy-rich-field-width 80)

;; Slow Rendering
;; If you experience a slow down in performance when rendering multiple icons simultaneously,
;; you can try setting the following variable
(setq inhibit-compacting-font-caches t)

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
                              ("pdf" . "zathura")
                              ("flac" . "vlc")
                              ("avi" . "vlc")
                              ("mkv" . "vlc")
                              ("mp4" . "vlc")))
(setq delete-by-moving-to-trash t
      trash-directory "~/.local/share/Trash/files/")
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(require 'elfeed-goodies)
(elfeed-goodies/setup)
(setq elfeed-goodies/entry-pane-size 0.5)
(add-hook 'elfeed-show-mode-hook 'visual-line-mode)
(setq elfeed-feeds (quote
                    (
                     ("https://www.androidpolice.com/feed/" android)
                     ("https://hackaday.com/blog/feed/" bezpieczeństwo)
                     ("https://feeds.feedburner.com/TheHackersNews" bezpieczeństwo)
                     ("http://feeds.feedburner.com/niebezpiecznik/" bezpieczeństwo)
                     ("http://feeds.feedburner.com/Torrentfreak" bezpieczeństwo)
                     ("https://restoreprivacy.com/feed/" bezpieczeństwo)
                     ("https://zaufanatrzeciastrona.pl/feed/" bezpieczeństwo)
                     ("https://hyperreal.info/rss.xml" drzewa)
                     ("https://climateandeconomy.com/feed/" ekologia)
                     ("https://insideclimatenews.org/feed/" ekologia)
                     ("https://jembendell.com/feed" ekologia)
                     ("https://planet.emacslife.com/atom.xml" emacs)
                     ("http://feeds.the-ebook-reader.com/feedburner/cmWU" ereader)
                     ("http://goodereader.com/blog/feed/" ereader)
                     ("http://rss.swiatczytnikow.pl/SwiatCzytnikow" ereader)
                     ("https://www.gamingonlinux.com/article_rss.php" gaming)
                     ("http://pcgamer.com/feed" gaming)
                     ("https://existentialcomics.com/rss.xml" komiksy)
                     ("http://queer.pl/rss/" lgbt)
                     ("https://9to5linux.com/feed/atom" linux)
                     ("https://artixlinux.org/feed.php" linux)
                     ("https://distrowatch.com/news/dw.xml" linux)
                     ("https://www.linuxjournal.com/rss_feeds" linux)
                     ("https://linuxman.co/feed/" linux)
                     ("https://lwn.net/headlines/newrss" linux)
                     ("https://omgubuntu.co.uk/feed" linux)
                     ("https://static.fsf.org/fsforg/rss/news.xml" opensource)
                     ("https://www.eff.org/rss/updates.xml" opensource)
                     ("https://feeds.feedburner.com/ItsFoss" opensource)
                     ("https://opensource.com/rss.xml" opensource)
                     ("http://codziennikfeministyczny.pl/feed/" płeć)
                     ("https://antipsychiatry.net/" psychatria)
                     ("http://www.antipsychiatry.org/" psychatria)
                     ("https://odrodzenie.fr/feed/" socjalizm)
                     ("https://postep.org.pl/feed" socjalizm)
                     ("http://strajk.eu/feed/" socjalizm)
                     ("http://feeds.soundcloud.com/users/soundcloud:users:284471201/sounds.rss" socjalizm)
                     ("https://antyweb.pl/feed" tech)
                     ("https://news.ycombinator.com/item?id=16908241" tech)
                     ("https://stare.pro/" tech)
                     ("https://consensus911.org/" teorie spiskowe)
                     ("https://www.g-central.com/feed/" zegarki)
                     ("https://digdeeper.neocities.org/atom.xml" niewiem)
                     )))

(setq emms-source-file-default-directory "~/Muzyka/"
      emms-playlist-buffer-name "*Music*"
      emms-info-asynchronously t
      emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)
(map! :leader
      (:prefix ("a" . "EMMS audio player")
       :desc "Go to emms playlist" "a" #'emms-playlist-mode-go
       :desc "Emms pause track" "x" #'emms-pause
       :desc "Emms stop track" "s" #'emms-stop
       :desc "Emms play previous track" "p" #'emms-previous
       :desc "Emms play next track" "n" #'emms-next))

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(setq erc-server "irc.libera.chat"
      erc-nick "anilorak"    ; Change this!
      erc-user-full-name "anilorak"  ; And this!
      erc-track-shorten-start 8
      erc-autojoin-channels-alist '(("irc.libera.chat" "#systemcrafters" "#distrotube" "#artix" "#emacs"))
      erc-kill-buffer-on-part t
            erc-auto-query 'bury)

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


(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(require 'ivy-posframe)
;; display at `ivy-posframe-style'
(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
(ivy-posframe-mode 1)

(setq ivy-youtube-key "AIzaSyBIoWmx9EONMNEYkSSpXzuyPHjgTdWpGfc")
;;start ivy-youtube.el
(autoload 'ivy-youtube "ivy-youtube" nil t)
(global-set-key (kbd "C-c y") 'ivy-youtube) ;; bind hotkey

;;set default browser for you will use to play videos/default generic
;; (setq browse-url-browser-function 'browse-url-generic)
;; (setq browse-url-generic-program "firefox-open-url")
(setq ivy-youtube-play-at "/usr/bin/vlc")

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

(setq doom-modeline-counsel-rhythmbox t)
(setq doom-modeline-buffer-name t)

(use-package mu4e
  :ensure nil
  :config

  (setq mu4e-get-mail-command "mbsync -c ~/.emacs.d/mu4e/.mbsyncrc -a")
  (setq mu4e-root-maildir (expand-file-name "~/Dokumenty/Maildir"))

  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)

  ;; Updates
  (setq mu4e-update-interval 180)
  (setq mu4e-headers-auto-update t)

  ;; Configure the function to use for sending mail
  (setq message-send-mail-function 'smtpmail-send-it)

  ;; Only ask if a context hasn't been previously picked
  (setq mu4e-compose-context-policy 'ask-if-none)

  ;; Make sure plain text mails flow correctly for recipients
  (setq mu4e-compose-format-flowed t)

  ;; enable inline images
  (setq mu4e-view-show-images t)
  ;; use imagemagick, if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  ;; Accounts
  (setq mu4e-contexts
        (list
       ;; Private account
       (make-mu4e-context
        :name "Gmail" ;; for gmail
        :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/Gmail" (mu4e-message-field msg :maildir))))
        :vars '((user-mail-address . "k.derwich96@gmail.com")
                (user-full-name    . "Karolina Derwich")
                (smtpmail-smtp-server  . "smtp.gmail.com")
                (smtpmail-smtp-service . 465)
                (smtpmail-stream-type  . ssl)
                (mu4e-drafts-folder  . "/Gmail/Drafts")
                (mu4e-sent-folder  . "/Gmail/Sent")
                (mu4e-refile-folder  . "/Gmail/Inbox")
                (mu4e-trash-folder  . "/Gmail/Trash")))

       ;; Shopping account
       (make-mu4e-context
        :name "Outlook" ;; for outlook
        :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/Outlook" (mu4e-message-field msg :maildir))))
        :vars '((user-mail-address . "k.derwich@outlook.com")
                (user-full-name    . "Karolina Derwich")
                (smtpmail-smtp-server  . "smtp-mail.outlook.com")
                (smtpmail-smtp-service . 587)
                (smtpmail-stream-type  . ssl)
                (mu4e-drafts-folder  . "/Outlook/Drafts")
                (mu4e-sent-folder  . "/Outlook/Sent Items")
                (mu4e-refile-folder  . "/Outlook/Inbox")
                (mu4e-trash-folder  . "/Outlook/Deleted Items"))))))

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
  (setq org-agenda-files
        '("~/Dokumenty/org/org-roam/20211206160944-org_agenda.org"
          "~/Dokumenty/org/org-roam/20220515174754-reccuring.org"
          "~/Dokumenty/org/org-roam/20220515174922-important_dates.org"))
  (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("@errand" . ?E)
       ("@home" . ?H)
       ("@uni" . ?U)
       ("@doc" . ?D)))

(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'tree))

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

(setq doom-theme 'tron-legacy)
;; (setq doom-theme 'catppuccin)
;; (setq doom-theme 'doom-dracula)
;; (setq doom-theme 'doom-nord)

;; (setq fancy-splash-image "~/.doom.d/splash/doomDracula.svg")
;; (setq fancy-splash-image "~/.doom.d/splash/doomOne.svg")

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
(setq auth-sources '("~/.authinfo.gpg"))
