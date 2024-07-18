;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

    (defvar elpaca-installer-version 0.7)
    (defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
    (defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
    (defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
    (defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
				  :ref nil :depth 1
				  :files (:defaults "elpaca-test.el" (:exclude "extensions"))
				  :build (:not elpaca--activate-package)))
    (let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
	   (build (expand-file-name "elpaca/" elpaca-builds-directory))
	   (order (cdr elpaca-order))
	   (default-directory repo))
      (add-to-list 'load-path (if (file-exists-p build) build repo))
      (unless (file-exists-p repo)
	(make-directory repo t)
	(when (< emacs-major-version 28) (require 'subr-x))
	(condition-case-unless-debug err
	    (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
		     ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
						     ,@(when-let ((depth (plist-get order :depth)))
							 (list (format "--depth=%d" depth) "--no-single-branch"))
						     ,(plist-get order :repo) ,repo))))
		     ((zerop (call-process "git" nil buffer t "checkout"
					   (or (plist-get order :ref) "--"))))
		     (emacs (concat invocation-directory invocation-name))
		     ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
					   "--eval" "(byte-recompile-directory \".\" 0 'force)")))
		     ((require 'elpaca))
		     ((elpaca-generate-autoloads "elpaca" repo)))
		(progn (message "%s" (buffer-string)) (kill-buffer buffer))
	      (error "%s" (with-current-buffer buffer (buffer-string))))
	  ((error) (warn "%s" err) (delete-directory repo 'recursive))))
      (unless (require 'elpaca-autoloads nil t)
	(require 'elpaca)
	(elpaca-generate-autoloads "elpaca" repo)
	(load "./elpaca-autoloads")))
    (add-hook 'after-init-hook #'elpaca-process-queues)
  (elpaca `(,@elpaca-order))

    ;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

;;When installing a package which modifies a form used at the top-level
;;(e.g. a package which adds a use-package key word),
;;use `elpaca-wait' to block until that package has been installed/configured.
;;For example:
;;(use-package general :demand t)
;;(elpaca-wait)

(global-set-key [escape] 'keyboard-escape-quit)

(use-package evil
    :init
    (setq evil-want-integration t)
    (setq evil-want-keybinding nil)
    (setq evil-vsplit-window-right t)
    (setq evil-split-window-below t)
    (setq evil-undo-system 'undo-redo)
    (evil-mode))

(use-package evil-collection
  :after evil
  :config
  (add-to-list 'evil-collection-mode-list 'help) ;; evilify help mode
  (evil-collection-init))

(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil))
(setq org-return-follows-link  t)

(use-package general
  :config
  (general-evil-setup)

  ;; set up 'SPC' as the global leader key
  (general-create-definer me/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode

  (me/leader-keys
    "SPC" '(counsel-M-x :wk "M-x")
    "." '(counsel-find-file :wk "Find file")
    ">" '(dired-jump :wk "Dired")
    "," '(counsel-ibuffer :wk "Ibuffer")
    "<" '(kill-buffer :wk "Kill buffer"))
  
  (me/leader-keys
    "b m" '(bookmark-set :wk "Add to bookmarks")
    "b s" '(bookmark-save :wk "Save bookmarks")
    "RET" '(counsel-bookmark :wk "List bookmarks"))

  (me/leader-keys
    "s b" '(swiper-isearch :wk "Swiper")
    "s l" '(counsel-imenu :wk "Imenu")
    "s r" '(counsel-rg :wk "Grep")
    "s f" '(counsel-fzf :wk "Fuzzy finding"))

  (me/leader-keys
    "c c" '(comment-line :wk "Comment Line")
    "c r" '(comment-or-uncomment-region :wk "Comment Region"))

  (me/leader-keys
    "f p" '((lambda () (interactive) (find-file "~/.config/emacs/config.org")) :wk "Emacs config.org")
    "f P" '((lambda () (interactive) (dired "~/.config/emacs/")) :wk "Emacs directory")
    "f r" '(counsel-recentf :wk "Recent files"))

  (me/leader-keys
    "f u" '(sudo-edit-find-file :wk "Sudo find file")
    "f U" '(sudo-edit :wk "Sudo edit file"))

  (me/leader-keys
    "h t" '(counsel-load-theme :wk "Change theme")
    "h U" '(elpaca-update-all :wk "Update packages")
    "h r r" '((lambda () (interactive) (load-file "~/.config/emacs/init.el") (ignore (elpaca-process-queues))) :wk "Reload emacs config"))

  (me/leader-keys
    "TAB q" '(evil-window-delete :wk "Close window")
    "TAB RET" '(evil-window-vnew :wk "New window")
    "TAB h" '(evil-window-left :wk "Window left")
    "TAB j" '(evil-window-down :wk "Window down")
    "TAB k" '(evil-window-up :wk "Window up")
    "TAB l" '(evil-window-right :wk "Window right")
    "TAB n" '(tab-new :wk "New tab")
    "TAB J" '(tab-next :wk "Next tab")
    "TAB K" '(tab-previous :wk "Previous tab")
    "TAB r" '(tab-rename :wk "Rename tab")
    "TAB H" '(previous-buffer :wk "Buffer previous")
    "TAB L" '(next-buffer :wk "Buffer next"))

  (me/leader-keys
    "A" '(org-agenda :wk "Org Agenda")
    "a a" '((lambda () (interactive) (find-file "~/Dokumenty/notatki/agenda/agenda-agenda.org")) :wk "Notes index")
    "X" '(org-capture :wk "Org Capture"))

  (me/leader-keys
    "N" '((lambda () (interactive) (find-file "~/Dokumenty/notatki/index-index.org")) :wk "Notes index")
    "n n" '((lambda () (interactive) (counsel-find-file "~/Dokumenty/notatki/")) :wk "Notes folder")
    "E" '(elfeed :wk "Elfeed")
    "P" '(pocket-reader :wk "Pocket")
    "W" '(eww :wk "EWW"))

  (general-nmap
    :keymaps 'org-mode-map
    "m a" 'org-insert-link
    "m A" 'link-hint-copy-link-at-point
    "m t" 'org-todo
    "m d" 'org-deadline
    "m s" 'org-schedule
    "m r" 'org-refile
    "m p" 'org-priority
    "m H" 'org-metaleft
    "m L" 'org-metaright
    "m J" 'org-metadown
    "m K" 'org-metaup
    "M" 'org-sidebar-tree-toggle)
  
  (general-nmap
    :keymaps 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-open-file)
  
  (general-nmap
    :keymaps 'elfeed-search-mode-map
    "W" 'elfeed-search-browse-url
    "M" 'elfeed-mark-all-as-read
    "P" 'pocket-reader-elfeed-search-add-link
    "O" 'elfeed-update)
)

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package beacon
  :init
  (beacon-mode 1))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :custom ((dired-listing-switches "-agho --group-directories-first")))

(setq delete-by-moving-to-trash t
      trash-directory "~/.local/share/Trash/files/")

(add-hook 'dired-mode-hook 'auto-revert-mode)

(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(use-package dired-open
  :config
  (setq dired-open-extensions '(("gif" . "nsxiv")
                                ("jpg" . "nsxiv")
                                ("png" . "nsxiv")
                                ("mkv" . "mpv")
                                ("mp4" . "mpv"))))

(use-package dired-preview
  :config
  (setq dired-preview-delay 0.7)
  (setq dired-preview-max-size (expt 2 20))
  (setq dired-preview-ignored-extensions-regexp
        (concat "\\."
                "\\(mkv\\|webm\\|mp4\\|mp3\\|ogg\\|m4a"
                "\\|gz\\|zst\\|tar\\|xz\\|rar\\|zip"
                "\\|iso\\|epub\\|pdf\\)"))
  (dired-preview-global-mode 1))

(use-package elfeed
  :config
  (setq elfeed-search-feed-face ":foreground #b3b8c3 :weight bold")
  (setq elfeed-db-directory "~/.config/emacs/files/elfeed/database"))

(defun elfeed-mark-all-as-read ()
  (interactive)
  (elfeed-untag elfeed-search-entries 'unread)
  (elfeed-search-update :force)) ; redraw

(use-package elfeed-goodies
  :init
  (elfeed-goodies/setup)
  :config
  (setq elfeed-goodies/entry-pane-size 0.5))

(use-package elfeed-org
  :ensure t
  :config
  (setq rmh-elfeed-org-files (list "~/.config/emacs/files/elfeed/elfeed.org"))
  (elfeed-org))

(use-package emojify
  :hook (after-init . global-emojify-mode))

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

(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-mode))
  ;; (evil-goggles-use-diff-faces)

(setq
 browse-url-browser-function 'eww-browse-url
 shr-use-fonts  nil
 ;; shr-use-colors nil
 ;; shr-indentation 2
 ;; shr-width 70
 shr-indentation 70 
 shr-width 170
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

(use-package flycheck
  :ensure t
  :defer t
  :diminish
  :init (global-flycheck-mode))

(use-package hl-todo
  :hook ((org-mode . hl-todo-mode)
         (prog-mode . hl-todo-mode))
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"      error bold)
	      ("WAIT"      warning bold)
          ("FIXME"     font-lock-constant-face bold)
          ("CANCELED"  font-lock-keyword-face bold)
          ("DONE"      success bold))))

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

(use-package counsel
  :after ivy
  :diminish
  :config 
    (counsel-mode)
    (setq ivy-initial-inputs-alist nil)) ;; removes starting ^ regex in M-x

(use-package ivy
  :bind
     (:map ivy-minibuffer-map
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
  :diminish
  :custom
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "(%d/%d) ")
    (setq enable-recursive-minibuffers t)
  :config
    (ivy-mode))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-prescient
  :after counsel
  :custom
    (ivy-prescient-enable-filtering nil)
  :config
    (prescient-persist-mode 1)
    (ivy-prescient-mode 1))

(use-package ivy-rich
  :after ivy
  :ensure t
  :init (ivy-rich-mode 1) ;; this gets us descriptions in M-x.
  :custom
  (ivy-virtual-abbreviate 'full
   ivy-rich-switch-buffer-align-virtual-buffer t
   ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer))

(setq org-ellipsis " ▾")
(setq org-src-preserve-indentation t)
(setq calendar-week-start-day 1)
(setq org-log-done 'time)
(setq org-log-into-drawer t)

(setq org-todo-keywords
  '((sequence "TODO(t)" "WAIT(w)" "FIXME(f)" "|" "CANCELED(c)" "DONE(d)")))

(setq org-agenda-start-with-log-mode t)

;; (setq org-agenda-files '("~/Dokumenty/notatki/agenda/"))
(setq org-agenda-files
  '("~/Dokumenty/notatki/agenda/agenda-agenda.org"
    "~/Dokumenty/notatki/agenda/agenda-habits.org"
    "~/Dokumenty/notatki/agenda/agenda-important.org"))

(setq org-agenda-span 10
      org-agenda-start-on-weekday nil
      org-agenda-start-day "-2d")

(setq org-agenda-prefix-format
      (quote
       ((agenda . "%-20c%?-12t% s")
        (timeline . "% s")
        (todo . "%-12c")
        (tags . "%-12c")
        (search . "%-12c"))))
(setq org-agenda-deadline-leaders (quote (":" "D%2d: " "")))
(setq org-agenda-scheduled-leaders (quote ("" "S%3d: ")))

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Dokumenty/notatki/agenda/agenda-agenda.org" "ZADANIA")
         "* TODO %?\n  %i\n  %a")))

(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 60)

(use-package org-fancy-priorities
  :ensure t
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq
     org-fancy-priorities-list '(" " " " "!")
     org-priority-faces
     '((?A :foreground "#b04b57")
       (?B :foreground "#e5c179")
       (?C :foreground "#87b379"))))

(setq org-refile-targets
  '(("archive.org" :maxlevel . 1)
    ("agenda-agenda.org" :maxlevel . 1)))
(advice-add 'org-refile :after 'org-save-all-org-buffers)

(use-package org-sidebar)

(require 'org-tempo)

(use-package ox-epub)

(use-package pocket-reader)
(setq pocket-reader-open-url-default-function #'eww)
(setq pocket-reader-pop-to-url-default-function #'eww)
(add-hook 'pocket-reader-mode (lambda () (display-line-numbers-mode 0)))

(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (clojure-mode . rainbow-delimiters-mode)))

(use-package rainbow-mode
  :diminish
  :hook org-mode prog-mode)

(use-package solaire-mode
  :init
  (solaire-global-mode +1))

(use-package sudo-edit)

(setq tab-bar-new-tab-choice "*scratch*"
      tab-bar-close-button-show nil
      tab-bar-new-button-show nil
      tab-bar-show 1)

(use-package which-key
  :init
    (which-key-mode 1)
  :diminish
  :config
  (setq which-key-side-window-location 'bottom
	  which-key-sort-order #'which-key-key-order-alpha
	  which-key-allow-imprecise-window-fit nil
	  which-key-sort-uppercase-first nil
	  which-key-add-column-padding 1
	  which-key-max-display-columns nil
	  which-key-min-display-lines 6
	  which-key-side-window-slot -10
	  which-key-side-window-max-height 0.25
	  which-key-idle-delay 0.8
	  which-key-max-description-length 25
	  which-key-allow-imprecise-window-fit nil
	  which-key-separator " → " ))

(delete-selection-mode 1)
(electric-indent-mode -1)
(electric-pair-mode 1)
(add-hook 'org-mode-hook (lambda ()
           (setq-local electric-pair-inhibit-predicate
                   `(lambda (c)
                  (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
(global-auto-revert-mode t)
(global-visual-line-mode t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq org-edit-src-content-indentation 0)
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq pop-up-windows nil)
(setq inhibit-startup-screen nil)

(global-display-line-numbers-mode 1)
(setq display-line-numbers 'relative)

(setq conf-unix-mode t)
(add-to-list 'auto-mode-alist '("\\.*rc$" . conf-unix-mode))
(setq shell-file-name "/usr/bin/bash")

(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

(setq backup-directory-alist '((".*" . "~/.local/share/Trash/files")))
(setq user-emacs-directory "~/.config/emacs")
;; (setq bookmark-default-file "~/.config/emacs/files/bookmarks")
(setq auth-sources '("~/Dokumenty/tajne/.authinfo.gpg"))

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

(set-face-attribute 'default nil
  :font "JetBrainsMono NF"
  :height 90
  :weight 'medium)
(set-face-attribute 'variable-pitch nil
  :font "Cantarell"
  :height 90
  :weight 'medium)
(set-face-attribute 'fixed-pitch nil
  :font "JetBrainsMono NF"
  :height 90
  :weight 'medium)
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)
(add-to-list 'default-frame-alist '(font . "JetBrainsMono NF-9"))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 30
        doom-modeline-bar-width 5
        doom-modeline-persp-name t
        doom-modeline-persp-icon t))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-spacegrey t)
  (doom-themes-org-config))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
