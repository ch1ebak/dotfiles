;; -*- lexical-binding: t; -*-

;; ============================
;; ▗▄▄▄▖▗▖  ▗▖ ▗▄▖  ▗▄▄▖ ▗▄▄▖
;; ▐▌   ▐▛▚▞▜▌▐▌ ▐▌▐▌   ▐▌   
;; ▐▛▀▀▘▐▌  ▐▌▐▛▀▜▌▐▌    ▝▀▚▖
;; ▐▙▄▄▖▐▌  ▐▌▐▌ ▐▌▝▚▄▄▖▗▄▄▞▘
;;
;; github.com/ch1ebak/dotfiles
;; ============================


(setq gc-cons-threshold #x40000000)

(setq read-process-output-max (* 1024 1024 4))

;; Use Package
(require 'use-package-ensure)
(setq use-package-always-ensure t
      package-enable-at-startup nil
      package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(setq package-quickstart t)

;; Emacs Config
(use-package emacs
  :ensure nil

  :custom
	(global-visual-line-mode t)
  (delete-selection-mode 1)
  (electric-indent-mode -1)
  (electric-pair-mode 1)
  (global-auto-revert-mode t)
  (set-fringe-mode 5)
  (blink-cursor-mode 0)
  (display-line-numbers-type 'relative) 
  (global-display-line-numbers-mode 1)
  (tab-width 2)
  (inhibit-startup-message t)
  (initial-scratch-message "")
  (vc-follow-symlinks t)
  (use-short-answers t)
  (use-dialog-box nil)
  (conf-unix-mode t)

  :config
  (set-face-attribute 'default nil :family "JetBrainsMono Nerd Font"  :height 105)
  (set-face-attribute 'fixed-pitch nil :family "JetBrainsMono Nerd Font" :height 105)
  (set-face-attribute 'variable-pitch nil :family "JetBrainsMono Nerd Font" :height 105)

  (setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory)))
        auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
        auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t))
        backup-directory-alist '((".*" . "~/.local/share/Trash/files"))
        user-emacs-directory "~/.config/emacs"
        bookmark-default-file "~/.config/emacs/files/bookmarks"
        auth-sources '("~/Dokumenty/tajne/.authinfo.gpg")
        custom-file (locate-user-emacs-file "files/custom-vars.el"))

  :init
  (indent-tabs-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1))

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-max-menu-items 25
				recentf-max-saved-items 25
				recentf-save-file "~/.config/emacs/files/recentf"))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :config
  (setq savehist-file "~/.config/emacs/files/savehist")
  (setq history-length 100)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (add-to-list 'savehist-additional-variables 'kill-ring))

;; Keymaps
(use-package evil
    :init
    (setq evil-want-integration t
          evil-want-keybinding nil
          evil-want-Y-yank-to-eol t
          evil-vsplit-window-right t
          evil-split-window-below t
          evil-shift-width 2
          evil-undo-system 'undo-tree)
    :config
    (evil-mode 1)
    (define-key evil-motion-state-map (kbd "SPC") nil)
    (define-key evil-motion-state-map (kbd "RET") nil)
    (define-key evil-motion-state-map (kbd "TAB") nil)
    ;; line movement
    (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
    (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
    (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
    (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
    ;; other
    (define-key evil-visual-state-map (kbd "J") 'evil-collection-unimpaired-move-text-down)
    (define-key evil-visual-state-map (kbd "K") 'evil-collection-unimpaired-move-text-up)
    (define-key evil-normal-state-map (kbd "gh") 'evil-beginning-of-line)
    (define-key evil-normal-state-map (kbd "gl") 'evil-end-of-line)
    (define-key evil-normal-state-map (kbd "gra") 'eglot-code-actions)
    ;; frames/tabs/windows/buffers
    (define-key evil-normal-state-map (kbd "C-n") 'split-window-right)
    (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
    (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
    (define-key evil-normal-state-map (kbd "C-t") 'tab-new)
    (define-key evil-normal-state-map (kbd "C-k") 'tab-next)
    (define-key evil-normal-state-map (kbd "C-j") 'tab-previous)
    (define-key evil-normal-state-map (kbd "C-w") 'evil-window-delete)
    (define-key evil-normal-state-map (kbd "C-o") 'my-min-max-window)
    (define-key evil-insert-state-map (kbd "C-n") 'evil-window-vnew)
    (define-key evil-insert-state-map (kbd "C-h") 'evil-window-left)
    (define-key evil-insert-state-map (kbd "C-l") 'evil-window-right)
    (define-key evil-insert-state-map (kbd "C-t") 'tab-new)
    (define-key evil-insert-state-map (kbd "C-k") 'tab-next)
    (define-key evil-insert-state-map (kbd "C-j") 'tab-previous)
    (define-key evil-insert-state-map (kbd "C-w") 'evil-window-delete)
    (define-key evil-insert-state-map (kbd "C-o") 'my-min-max-window)
    (define-key evil-normal-state-map (kbd "C-S-J") 'evil-window-move-far-left)
    (define-key evil-normal-state-map (kbd "C-S-K") 'evil-window-move-far-right)
    (define-key evil-normal-state-map (kbd "C-S-H") 'previous-buffer)
    (define-key evil-normal-state-map (kbd "C-S-L") 'next-buffer))

(setq-default evil-cross-lines t)
(setq org-return-follows-link t)

(use-package evil-collection
  :after evil
  :config
  (add-to-list 'evil-collection-mode-list 'help)
  (evil-collection-init))

(use-package evil-commentary
  :config
  (evil-commentary-mode))

(use-package evil-org
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package general
  :config
  (general-evil-setup)

  (general-create-definer me/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC")

  (me/leader-keys
    "SPC" '(execute-extended-command :wk "M-x")
    "RET" '(consult-bookmark :wk "Bookmarks")
    "." '(consult-find :wk "Find file")
    ">" '(dired-jump :wk "Dired")
    "," '(consult-buffer :wk "Buffers")
    "<" '(kill-buffer :wk "Killing Buffers")
    "?" '(consult-ripgrep :wk "Grep")
    "/" '(consult-line :wk "Search line"))

  (me/leader-keys
    "s" '(:ignore t :wk "Sessions")
    "s s" '(wg-create-workgroup :wk "Create new session from open frames")
    "s l" '(wg-open-workgroup :wk "Load session"))

  (me/leader-keys
    "b" '(:ignore t :wk "Bookmarks")
    "b m" '(bookmark-set :wk "Add to bookmarks")
    "b s" '(bookmark-save :wk "Save bookmarks"))

  (me/leader-keys
    "f" '(:ignore t :wk "Files")
    "f p" '((lambda () (interactive) (find-file "~/.config/emacs/init.el")) :wk "Emacs Config")
    "f n" '((lambda () (interactive) (consult-fd "~/Dokumenty/notatki/")) :wk "Notes")
    "f N" '((lambda () (interactive) (dired "~/Dokumenty/notatki/")) :wk "Notes")
    "f r" '(consult-recent-file :wk "Recent files")
    "f u" '(sudo-edit :wk "Sudo edit file")
    "f U" '(sudo-edit-find-file :wk "Sudo find file"))

  (me/leader-keys
    "h" '(:ignore t :wk "Emacs")
    "h d" '(redraw-display :wk "Redraw display (some issues on wayland)")
    "h t" '(consult-theme :wk "Change theme")
    "h l" '(package-upgrade-all :wk "Update packages")
    "h r" '((lambda () (interactive) (load-file "~/.dotfiles/.config/emacs/init.el")) :wk "Reload emacs config")
    "h R" '(restart-emacs :wk "Reload emacs config"))

  (me/leader-keys
    "p" '(:ignore t :wk "Packages")
    "p a" '(org-agenda :wk "Org Agenda")
    "p x" '(org-capture :wk "Org Capture")
    "p e" '(elfeed :wk "Elfeed")
    "p m" '(magit-status :wk "Magit")
    "p w" '(eww :wk "EWW"))

  (me/leader-keys
    "t" '(:ignore t :wk "Toggles")
    "t u" '(undo-tree-visualize :wk "Undo Tree")
    "t l" '(toggle-truncate-lines :wk "Line wrapping")
    "t x" '(executable-set-magic :wk "Set interpreter")
    "t v" '(visual-fill-column-mode :wk "Visual fill column")
    "t r" '(rainbow-mode :wk "Rainbow mode"))

  (me/leader-keys
    "e" '(:ignore t :wk "Eglot")
    "e h" '(eglot :wk "Launch Eglot")
    "e l" '(eglot-shutdown :wk "Shutdown Eglot")
    "e w" '(eglot-code-actions :wk "Eglot Code Actions"))

  (me/leader-keys
    "z" '(:ignore t :wk "Langtool")
    "z h" '(langtool-check :wk "Launch Langtool")
    "z l" '(langtool-check-done :wk "Shutdown Langtool")
    "z w" '(langtool-correct-buffer :wk "Langtool Correct"))

  (general-nmap
    :keymaps 'org-mode-map
    "C-RET" 'org-meta-return
    "C-SPC" 'org-toggle-checkbox
    "C-k" 'tab-next
    "C-j" 'tab-previous
    ">>" 'org-metaleft
    "<<" 'org-metaright
    "] d" 'org-next-link
    "[ d" 'org-previous-link
    "g j" 'org-next-visible-heading
    "g k" 'org-previous-visible-heading
    "m" '(:ignore t :wk "Org")
    "m a" 'org-insert-link
    "m A" 'link-hint-copy-link-at-point
    "m t" 'org-todo
    "m d" 'org-deadline
    "m s" 'org-schedule
    "m r" 'org-refile
    "m p" 'org-priority
    "m n" 'org-add-note
    "m l" 'org-cycle-list-bullet
    "m J" 'org-metadown
    "m K" 'org-metaup
    "t" '(:ignore t :wk "Tabela")
    "t s" 'org-table-sort-lines
    "t a" 'org-table-sum
    "t n" 'org-table-insert-column
    "t h" 'org-table-move-column-left
    "t l" 'org-table-move-column-right
    "t k" 'org-table-move-row-up
    "t j" 'org-table-move-row-down)
  
  (general-nmap
    :keymaps 'markdown-mode-map
    ">>" 'markdown-promote
    "<<" 'markdown-demote
    "] d" 'markdown-next-link
    "[ d" 'markdown-previous-link
    "] h" 'markdown-next-visible-heading
    "[ h" 'markdown-previous-visible-heading
    "m w" 'count-words-region
    "m J" 'markdown-move-down
    "m K" 'markdown-move-up
    "m l" 'markdown-insert-link
    "m i" 'markdown-insert-image
    "m p" 'markdown-preview
    "m c" 'markdown-toggle-markup-hiding
    "m h" 'markdown-toggle-url-hiding
    "m i" 'markdown-toggle-inline-images)
  
  (general-nmap
    :keymaps 'dired-mode-map
    "y d" 'dired-copy-dirname-as-kill
    "y c" 'dired-copy-path-at-point
    "a" 'find-file
    "A" 'mkdir
    "M" 'dired-unmark
    "p" 'image-dired
		"s" 'dired-sort-toggle-or-edit
    "c" 'dired-do-copy
    "C" 'dired-do-copy
    "r" 'dired-do-rename
    "R" 'dired-do-rename
    "h" 'dired-up-directory
    "l" 'dired-open-file)
  
  (general-nmap
    :keymaps 'elfeed-search-mode-map
    "W" 'elfeed-search-browse-url
    "A" 'elfeed-mark-all-as-read
    "R" 'elfeed-update))

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

;; Completion
(use-package vertico
  :hook
  (after-init . vertico-mode)
  :custom
  (vertico-count 10)
  (vertico-resize nil)
  (vertico-cycle t)
  :config
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "» " 'face '(:foreground "#80adf0" :weight bold))
                   "  ")
                 cand))))

(use-package orderless
  :defer t
  :after vertico
  :bind (:map vertico-map
         ("C-j" . vertico-next)
         ("C-k" . vertico-previous)
         ("C-f" . vertico-exit)
         :map minibuffer-local-map
         ("M-h" . backward-kill-word))
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :custom
  (marginalia-align 'right)
  :hook
  (after-init . marginalia-mode))

(use-package consult
  :defer t
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package embark
  :defer t)

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; UI
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes")

(use-package catppuccin-theme)

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-org-config))

(use-package kanagawa-themes)

(use-package real-mono-themes)

(load-theme 'doom-tokyo-night :no-confirm)

(add-to-list 'default-frame-alist '(alpha-background . 90))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 30
        doom-modeline-bar-width 5
        doom-modeline-major-mode-icon nil
        doom-modeline-window-width-limit 85
        doom-modeline-spc-face-overrides nil
        doom-modeline-buffer-file-name-style 'truncate-all
        doom-modeline-buffer-state-icon nil
        doom-modeline-buffer-modification-icon nil
        doom-modeline-highlight-modified-buffer-name t
        doom-modeline-minor-modes nil
        doom-modeline-enable-word-count t
        doom-modeline-buffer-encoding nil
        doom-modeline-modal-modern-icon nil
        doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode)))

;; Icons
(use-package nerd-icons)

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :after (:all nerd-icons marginalia)
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; Packages
(use-package beacon
  :init
  (beacon-mode 1))

(use-package dired
  :ensure nil
  :defer
  :hook
  (dired-mode . hl-line-mode)
  (dired-mode . auto-revert-mode)
  :config
  (setq dired-listing-switches
      "-AGFhlv --group-directories-first")
  :custom
  (dired-do-revert-buffer t)
  (dired-auto-revert-buffer t)
  (delete-by-moving-to-trash t)
  (dired-dwim-target t))

(defun dired-copy-path-at-point ()
    (interactive)
    (dired-copy-filename-as-kill 0))

(defun dired-copy-dirname-as-kill ()
  "Copy the current directory into the kill ring."
  (interactive)
  (kill-new default-directory))

(use-package dired-open
  :config
  (setq dired-open-extensions '(("gif" . "feh")
                                ("jpg" . "feh")
                                ("png" . "feh")
                                ("mkv" . "mpv")
                                ("mp4" . "mpv")
                                ("flac" . "mpv")
                                ("mp3" . "mpv")
                                ("pdf" . "zen-browser"))))

(use-package image-dired
  :ensure nil
  :commands (image-dired)
  :bind
  ( :map image-dired-thumbnail-mode-map
    ("<return>" . image-dired-thumbnail-display-external))
  :config
  (setq image-dired-thumbnail-storage 'standard)
  (setq image-dired-external-viewer "xdg-open")
  (setq image-dired-thumb-size 80)
  (setq image-dired-thumb-margin 2)
  (setq image-dired-thumb-relief 0)
  (setq image-dired-thumbs-per-row 4))

(use-package eglot
  :ensure nil
  :custom
  (eglot-events-buffer-size 0)
  (eglot-autoshutdown t)
  (eglot-report-progress nil)
	:config
  (add-to-list 'eglot-server-programs '(markdown-mode . ("harper-ls" "--stdio"))))

(setq-default eglot-workspace-configuration
              '(:harper-ls (:userDictPath ""
                            :fileDictPath ""
                            :linters (:SpellCheck t
                                      :SpelledNumbers :json-false
                                      :AnA t
                                      :SentenceCapitalization t
                                      :UnclosedQuotes t
                                      :WrongQuotes :json-false
                                      :LongSentences t
                                      :RepeatedWords t
                                      :Spaces t
                                      :Matcher t
                                      :CorrectNumberSuffix t)
                            :codeActions (:ForceStable :json-false)
                            :markdown (:IgnoreLinkTitle :json-false)
                            :diagnosticSeverity "hint"
                            :isolateEnglish :json-false
                            :dialect "American"
                            :maxFileLength 120000)))

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
  :config
  (setq rmh-elfeed-org-files (list "~/.config/emacs/files/elfeed/elfeed.org"))
  (elfeed-org))

(use-package eww
  :ensure nil
  :config
  (setq
  browse-url-browser-function 'eww-browse-url
  shr-use-fonts  nil
  ;; shr-use-colors nil
  shr-indentation 2
  ;; shr-indentation 70 
  shr-width 100
  eww-auto-rename-buffer 'title
  eww-download-directory "~/Pobrane"
  eww-bookmarks-directory "~/.config/emacs/files/"
  eww-search-prefix "https://frogfind.com/?q="
  browse-url-secondary-browser-function 'browse-url-xdg-open)
  :hook
  (eww-after-render-hook . eww-readable))

(defun eww-new ()
  (interactive)
  (let ((url (read-from-minibuffer "Enter URL or keywords: ")))
    (switch-to-buffer (generate-new-buffer "eww"))
    (eww-mode)
    (eww url)))

(use-package indent-guide
  :hook
  (prog-mode . indent-guide-mode)
  :config
  (setq indent-guide-char "│"))

(use-package ispell
  :ensure nil
  :config
  (setq ispell-program-name "hunspell"
        ispell-dictionary "pl_PL,en_US"
        ispell-personal-dictionary "~/.config/emacs/files/hunspell_personal"
        ispell-silently-savep t)
  :custom
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "pl_PL,en_US"))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(use-package langtool
  :ensure t
  :config
  (setq langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*")
  (setq langtool-default-language "en-US")
  (setq langtool-mother-tongue "pl"))

(use-package magit)

(use-package markdown-mode
  :init
  (setq markdown-unordered-list-item-prefix "  -")
  (setq markdown-hide-urls t)
  (setq markdown-command
      (concat
      "pandoc"
      " --from=markdown --to=html"
      " --standalone --mathjax --highlight-style=pygments")))

;; https://www.reddit.com/r/emacs/comments/yzjmmf/comment/ix1y211
(defvar my-min-max-window nil)
(defun my-min-max-window()
  (interactive)
  (if (and (one-window-p) my-min-max-window)
      (window-state-put my-min-max-window)
    (setq my-min-max-window (window-state-get))
    (delete-other-windows)))

(use-package org
  :ensure nil
  :config
  (setq org-ellipsis " ▾")
  (setq org-src-preserve-indentation t)
  (setq calendar-week-start-day 1)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-hide-emphasis-markers t)
  (customize-set-variable 'org-blank-before-new-entry
                          '((heading . nil)
                          (plain-list-item . nil)))
  (setq org-agenda-start-with-log-mode t)
  (setq org-agenda-files
    '("~/Dokumenty/notatki/02-agenda/Taski.org"
      "~/Dokumenty/notatki/02-agenda/Powtarzalne.org"
      "~/Dokumenty/notatki/02-agenda/Daty.org"
      "~/Dokumenty/notatki/02-agenda/Wydarzenia.org"))
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
  (setq org-agenda-current-time-string "← now")
  (setq org-agenda-time-grid '((daily today require-timed) (800 1000 1200 1400 1600 1800 2000)
                              " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (add-hook 'org-mode-hook 'org-indent-mode)
  (setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Dokumenty/notatki/02-agenda/Taski.org" "Inbox")
          "** TODO %?\n  %i\n ")))
  (setq org-refile-targets
  '(("Archiwum.org" :maxlevel . 1)))
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (setq org-todo-keywords
  '((sequence "TODO(t)" "WAIT(w)" "FIXME(f)" "|" "CANCELED(c)" "DONE(d)")))
  (setq org-todo-keyword-faces
      '(("TODO" . (:foreground "#b04b57" :weight bold))
        ("WAIT" . (:foreground "#e5c179" :weight bold))
        ("FIXME" . (:foreground "#a47996" :weight bold))
        ("CANCELED" . (:foreground "#85a7a5" :weight bold))
        ("DONE" . (:foreground "#87b379" :weight bold)))))

(use-package org-tempo
  :ensure nil)

(use-package org-habit
  :ensure nil
  :config
  (setq org-habit-graph-column 60))

(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (clojure-mode . rainbow-delimiters-mode)))

(use-package rainbow-mode
  :ensure t
  :hook (prog-mode org-mode markdown-mode))

(use-package sudo-edit)

(use-package tab-bar
  :ensure nil
  :defer t
  :config
  (setq tab-bar-new-tab-choice "*scratch*"
        tab-bar-close-button-show nil
        tab-bar-new-button-show nil
        tab-bar-close-last-tab-choice 'tab-bar-mode-disable
        tab-bar-close-tab-select 'recent
        tab-bar-new-tab-to 'right
        tab-bar-tab-hints nil
        tab-bar-separator " "
        tab-bar-show 1))

(use-package treesit
  :ensure nil
  :defer
	:config
	(setq treesit-language-source-alist
		'((bash "https://github.com/tree-sitter/tree-sitter-bash")
			(elisp "https://github.com/Wilfred/tree-sitter-elisp")
			(html "https://github.com/tree-sitter/tree-sitter-html")
      (hyprlang "https://github.com/tree-sitter-grammars/tree-sitter-hyprlang")
			(markdown "https://github.com/ikatyang/tree-sitter-markdown")))
  :custom
  (treesit-font-lock-level 2))

(use-package treesit-auto
  :ensure t
  :hook (emacs-startup . global-treesit-auto-mode)
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all))

(use-package undo-tree
  :defer t
  :hook
  (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        undo-limit 800000
        undo-strong-limit 12000000
        undo-outer-limit 120000000)
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.config/emacs/tmp/undo"))))

(use-package visual-fill-column
  :custom
  (visual-fill-column-width 120)
  (visual-fill-column-center-text t))

(use-package wdired
  :ensure nil
  :commands (wdired-change-to-wdired-mode)
  :config
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))

(use-package writegood-mode)

(use-package workgroups2
	:config
	(setq wg-session-file "~/.config/emacs/files/emacs_workgroups")
	:init
	(workgroups-mode 1))

(setq gc-cons-threshold (* 2 1000 1000))

(provide 'init)
