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

(all-the-icons-ivy-rich-mode 1)
(setq all-the-icons-ivy-rich-icon t)
(setq all-the-icons-ivy-rich-color-icon t)
(setq all-the-icons-ivy-rich-project t)

;; (setq browse-url-browser-function 'eww-browse-url)

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
(defun elfeed-mark-all-as-read ()
      (interactive)
      (mark-whole-buffer)
      (elfeed-search-untag-all-unread))
(setq elfeed-feeds (quote
                    (
                     ;; Android
                     ("https://www.androidpolice.com/feed/" android)
                     ;; Bezpieczeństwo
                     ("https://hackaday.com/blog/feed/" bezpieczeństwo)
                     ("https://feeds.feedburner.com/TheHackersNews" bezpieczeństwo)
                     ("http://feeds.feedburner.com/niebezpiecznik/" bezpieczeństwo)
                     ("https://torrentfreak.com/feed/" bezpieczeństwo)
                     ("https://restoreprivacy.com/feed/" bezpieczeństwo)
                     ("https://zaufanatrzeciastrona.pl/feed/" bezpieczeństwo)
                     ;; Drzewa
                     ("https://hyperreal.info/rss.xml" drzewa)
                     ;; Ekologia
                     ("https://climateandeconomy.com/feed/" ekologia)
                     ("https://insideclimatenews.org/feed/" ekologia)
                     ("https://jembendell.com/feed" ekologia)
                     ;; Emacs
                     ("https://planet.emacslife.com/atom.xml" emacs)
                     ("http://pragmaticemacs.com/feed/" emacs)
                     ("https://sachachua.com/blog/category/emacs-news/feed" emacs)
                     ("https://blog.tecosaur.com/tmio/rss.xml" emacs)
                     ;; Ereader
                     ("http://feeds.the-ebook-reader.com/feedburner/cmWU" ereader)
                     ("http://goodereader.com/blog/feed/" ereader)
                     ("http://rss.swiatczytnikow.pl/SwiatCzytnikow" ereader)
                     ;; Gaming
                     ("blog.bioware.com/feed/" gaming)
                     ("https://www.gamingonlinux.com/article_rss.php" gaming)
                     ("http://pcgamer.com/feed" gaming)
                     ;; Komiksy
                     ("https://existentialcomics.com/rss.xml" komiksy)
                     ("https://xkcd.com/atom.xml" komiksy)
                     ;; LGBT
                     ("https://www.autostraddle.com/feed" lgbt)
                     ("https://www.afterellen.com/feed" lgbt)
                     ("https://lesbrary.com/feed" lgbt)
                     ("http://queer.pl/rss/" lgbt)
                     ;; Linux
                     ("https://9to5linux.com/feed/atom" linux)
                     ("https://artixlinux.org/feed.php" linux)
                     ("https://distrowatch.com/news/dw.xml" linux)
                     ("http://feeds.feedburner.com/linuxpl-news" linux)
                     ("https://www.linuxjournal.com/node/feed" linux)
                     ("https://linuxman.co/feed/" linux)
                     ("https://lwn.net/headlines/newrss" linux)
                     ("https://omgubuntu.co.uk/feed" linux)
                     ("https://sysdfree.wordpress.com/feed" linux)
                     ("https://unixsheikh.com/feed.rss" linux)
                     ;; Newsy
                     ("http://www.gazetaprawna.pl/rss.xml" newsy)
                     ("https://allthatsinteresting.com/tag/news/feed" newsy)
                     ;; Open source
                     ("https://fossforce.com/feed/" opensource)
                     ("https://static.fsf.org/fsforg/rss/news.xml" opensource)
                     ("https://www.eff.org/rss/updates.xml" opensource)
                     ("https://feeds.feedburner.com/ItsFoss" opensource)
                     ("https://opensource.com/rss.xml" opensource)
                     ;; Płeć
                     ("http://codziennikfeministyczny.pl/feed/" płeć)
                     ;; Podcasty
                     ("https://feeds.buzzsprout.com/1890340.rss" podcast)
                     ("https://revolutionaryleftradio.libsyn.com/rss" podcast)
                     ("https://feeds.soundcloud.com/users/soundcloud:users:407338935/sounds.rss" podcast)
                     ("https://feeds.soundcloud.com/users/soundcloud:users:672423809/sounds.rss" podcast)
                     ;; Psychatria
                     ("https://antipsychiatry.net/" psychatria)
                     ("http://www.antipsychiatry.org/" psychatria)
                     ("https://www.psypost.org/feed" psychatria/psychologia)
                     ("http://rss.sciam.com/ScientificAmerican-Global" psychatria/psychologia)
                     ;; Socjalizm
                     ("https://instytut-marksa.org/feed/" socjalizm)
                     ("https://marxistsociology.org/feed/" socjalizm)
                     ("https://odrodzenie.fr/feed/" socjalizm)
                     ("https://postep.org.pl/feed" socjalizm)
                     ("http://strajk.eu/feed/" socjalizm)
                     ("http://feeds.soundcloud.com/users/soundcloud:users:284471201/sounds.rss" socjalizm)
                     ("https://krytykapolityczna.pl/feed/" socjalizm)
                     ("https://jacobin.com/feed/" socjalizm)
                     ;; Socjologia
                     ("https://feeds.feedburner.com/EverydaySociologyBlog" socjologia)
                     ("http://www.sociologylens.net/feed" socjologia)
                     ;; Tech
                     ("https://antyweb.pl/feed" tech)
                     ("https://kernal.eu/feed" tech)
                     ("https://hnrss.org/frontpage" tech)
                     ("https://sadgrl.online/feed.xml" tech)
                     ("https://stare.pro/" tech)
                     ("https://www.wired.com/feed/rss" tech)
                     ;; Teorie spiskowe
                     ("https://consensus911.org/" teorie spiskowe)
                     ;; Zegarki
                     ("https://www.g-central.com/feed/" zegarki)
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

(require 'ivy-posframe)
;; display at `ivy-posframe-style'
(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
(ivy-posframe-mode 1)

(use-package ivy-todo :ensure t
  :bind ("C-c t" . ivy-todo)
  :commands ivy-todo
  :config
  (setq ivy-todo-file "~/Dokumenty/org/org-roam/20220527183456-inbox.org")
  (setq ivy-todo-default-tags '("TODO")))

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
  :ensure t
  :defer 10
  :config

  (setq mu4e-get-mail-command "mbsync -c ~/.emacs.d/mu4e/.mbsyncrc -a")
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
  (setq mu4e-view-show-images t)
  ;; use imagemagick, if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  ;; Use Ivy for mu4e completions (maildir folders, etc)
  (setq mu4e-completing-read-function #'ivy-completing-read)

  ;; setup some handy shortcuts
  (setq mu4e-maildir-shortcuts
        '(("/gmail/Sent"         . ?g)
          ("/outlook/Sent Items" . ?o)))

  (add-to-list 'mu4e-bookmarks
          (make-mu4e-bookmark
           :name "All Inboxes"
           :query "maildir:/gmail/Inbox OR maildir:/outlook/Inbox"
           :key ?a))

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

(require 'org-sticky-header)

(require 'rainbow-mode)
(rainbow-mode t)

(setq shell-file-name "/usr/bin/fish")

;; from http://emacswiki.org/emacs/InsertingTodaysDate
(defun insert-todays-date (arg)
  (interactive "U")
  (insert (if arg
              (format-time-string "%d-%m-%Y")
            (format-time-string "%Y-%m-%d"))))

;; (setq doom-theme 'catppuccin)
;; (setq doom-theme 'doom-dracula)
;; (setq doom-theme 'doom-gruvbox)
;; (setq doom-theme 'doom-nord)
(setq doom-theme 'doom-one)

(require 'twittering-mode)
      (setq twittering-use-master-password t)
      (setq twittering-private-info-file "~/.emacs.d/twitter/.twittering-mode.gpg")
      (setq twittering-cert-file "/etc/ssl/certs/ca-bundle.crt")
      (setq twittering-allow-insecure-server-cert t)
      (setq twittering-icon-mode t)
      (setq twittering-use-icon-storage t)
      (setq twittering-icon-storage-file "~/.emacs.d/twitter/.twittering-mode-icons.gz")
      (setq twittering-display-remaining t)
(defalias 'epa--decode-coding-string 'decode-coding-string)

(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
(defun td/visual-fill-setup ()
  "Center the column 100 characters wide."
  (setq-local visual-fill-column-width 150
              visual-fill-column-center-text nil)
  (visual-fill-column-mode 1))

(use-package wallabag
  :defer t
  :config
  (setq wallabag-host "https://app.wallabag.it")
  (setq wallabag-username "")
  (setq wallabag-password "")
  (setq wallabag-clientid "")
  (setq wallabag-secret "")
  (add-hook 'wallabag-after-render-hook 'wallabag-search-update-and-clear-filter)
  )

(setq frame-resize-pixelwise t)
(setq display-line-numbers-type t)
(setq org-hide-emphasis-markers t)
(setq auth-sources '("~/.authinfo.gpg"))
(global-set-key (kbd "C-x w") 'delete-frame)
