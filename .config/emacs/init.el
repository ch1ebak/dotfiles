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
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents) (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)


(setq package-quickstart t)

;; Emacs Config
(use-package emacs
  :ensure nil
  :custom
  (delete-selection-mode 1)
  (electric-indent-mode -1)
  (electric-pair-mode 1)
  (global-auto-revert-mode t)
  (global-visual-line-mode t)
  (set-fringe-mode 5)
  (blink-cursor-mode 0)
  (winner-mode 1)
  :config
  (setq-default tab-width 2)
  (setq org-edit-src-content-indentation 0)
  (setq vc-follow-symlinks nil)
  (setq use-file-dialog nil)
  (setq use-dialog-box nil)
  (setq pop-up-windows nil)
  (setq inhibit-startup-screen nil)
  (setq shell-file-name "/usr/bin/bash")
  (setq-default indent-tabs-mode nil)
  (setq use-short-answers t)
  (setq electric-pair-pairs
      '((?\( . ?\)) (?\[ . ?\]) (?\{ . ?\}) (?\< . ?\>)
          (?\« . ?\») (?\" . ?\")))
  (setq electric-pair-skip-whitespace 'chomp)
  (add-hook 'org-mode-hook (lambda ()
          (setq-local electric-pair-inhibit-predicate
                  `(lambda (c)
                  (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
  (setq display-line-numbers-type 'relative) 
  (global-display-line-numbers-mode)
  (setq conf-unix-mode t)
  (add-to-list 'auto-mode-alist '("\\.*rc$" . conf-unix-mode))
  (defun crm-indicator (args)
      (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                  "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                  crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  (setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
  (setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))
  (setq backup-directory-alist '((".*" . "~/.local/share/Trash/files")))
  (setq user-emacs-directory "~/.config/emacs")
  (setq bookmark-default-file "~/.config/emacs/files/bookmarks")
  (setq auth-sources '("~/Dokumenty/tajne/.authinfo.gpg"))
  (setq custom-file (locate-user-emacs-file "custom-vars.el"))
  (setq recentf-mode t)
  (setq recentf-save-file "~/.config/emacs/files/recentf")
  (load custom-file 'noerror 'nomessage)
  (set-face-attribute 'default nil :family "JetBrainsMono Nerd Font"  :height 100)
  (set-face-attribute 'fixed-pitch     nil :family "JetBrainsMono Nerd Font" :height 100)
  (set-face-attribute 'variable-pitch  nil :family "JetBrainsMono Nerd Font" :height 100)

  :init
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1))

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
    (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
    (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
    (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
    (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
    (define-key evil-normal-state-map (kbd "C-n") 'evil-window-vnew)
    (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
    (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
    (define-key evil-normal-state-map (kbd "C-t") 'tab-new)
    (define-key evil-normal-state-map (kbd "C-k") 'tab-next)
    (define-key evil-normal-state-map (kbd "C-j") 'tab-previous)
    (define-key evil-normal-state-map (kbd "C-w") 'evil-window-delete)
    (define-key evil-normal-state-map (kbd "C-S-J") 'evil-window-move-far-left)
    (define-key evil-normal-state-map (kbd "C-S-K") 'evil-window-move-far-right)
    (define-key evil-normal-state-map (kbd "C-S-H") 'previous-buffer)
    (define-key evil-normal-state-map (kbd "C-S-L") 'next-buffer)
    (define-key evil-normal-state-map (kbd "gh") 'evil-beginning-of-line)
    (define-key evil-normal-state-map (kbd "gl") 'evil-end-of-line)
    (define-key evil-normal-state-map (kbd "gra") 'eglot-code-actions))

(setq-default evil-cross-lines t)
(setq org-return-follows-link t)

(use-package evil-collection
  :after evil
  ;; :custom (evil-collection-setup-minibuffer t)
  :config
  (add-to-list 'evil-collection-mode-list 'help)
  (evil-collection-init))

(use-package evil-commentary
  :config
  (evil-commentary-mode))

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
    "." '(consult-find :wk "Fuzzy finding")
    ">" '(dired-jump :wk "Dired")
    "," '(consult-buffer :wk "Buffers")
    "?" '(consult-ripgrep :wk "Grep")
    "/" '(consult-line :wk "Search line"))

  (me/leader-keys
    "s" '(:ignore t :wk "Sessions")
    "s s" '(burly-bookmark-frames :wk "Create new session from open frames")
    "s S" '(burly-bookmark-windows :wk "Create new session from open windows")
    "s l" '(burly-open-bookmark :wk "Load session"))

  (me/leader-keys
    "b" '(:ignore t :wk "Bookmarks")
    "b m" '(bookmark-set :wk "Add to bookmarks")
    "b s" '(bookmark-save :wk "Save bookmarks"))

  (me/leader-keys
    "f" '(:ignore t :wk "Files")
    "f p" '((lambda () (interactive) (find-file "~/.config/emacs/init.el")) :wk "Emacs Config")
    "f n" '((lambda () (interactive) (consult-fd "~/Dokumenty/notatki/")) :wk "Notes")
    "f r" '(consult-recent-file :wk "Recent files")
    "f u" '(sudo-edit :wk "Sudo edit file")
    "f U" '(sudo-edit-find-file :wk "Sudo find file"))

  (me/leader-keys
    "h" '(:ignore t :wk "Emacs")
    "h t" '(consult-theme :wk "Change theme")
    "h l" '(package-upgrade-all :wk "Update packages")
    "h r" '((lambda () (interactive) (load-file "~/.dotfiles/.config/emacs/init.el")) :wk "Reload emacs config")
    "h R" '(restart-emacs :wk "Reload emacs config"))

  (me/leader-keys
    "p" '(:ignore t :wk "Packages")
    "p x" '(org-capture :wk "Org Capture")
    "p a" '(org-agenda :wk "Org Agenda")
    "p e" '(elfeed :wk "Elfeed")
    "p w" '(eww :wk "EWW")
    "p W" '(eww-list-bookmarks :wk "EWW Bookmarks"))

  (me/leader-keys
    "t" '(:ignore t :wk "Toggles")
    "t l" '(toggle-word-wrap :wk "Line wrapping")
    "t x" '(executable-set-magic :wk "Set interpreter")
    "t v" '(visual-fill-column-mode :wk "Visual fill column")
    "t r" '(rainbow-mode :wk "Rainbow mode"))

  (me/leader-keys
    "e" '(:ignore t :wk "Eglot")
    "e h" '(eglot :wk "Launch Eglot")
    "e d" '(eglot-shutdown :wk "Shutdown Eglot")
    "e w" '(eglot-code-actions :wk "Eglot Code Actions"))

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
    "g j" 'markdown-next-visible-heading
    "g k" 'markdown-previous-visible-heading
    "m J" 'markdown-move-down
    "m K" 'markdown-move-up
    "m l" 'markdown-insert-link
    "m i" 'markdown-insert-image
    "m p" 'markdown-preview
    "m h" 'markdown-toggle-url-hiding
    "m i" 'markdown-toggle-inline-images)
  
  (general-nmap
    :keymaps 'dired-mode-map
    "y d" 'dired-copy-dirname-as-kill
    "y c" 'dired-copy-path-at-point
    "a" 'counsel-find-file
    "." 'zoxide-travel
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
    "O" 'elfeed-update))

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
(use-package catppuccin-theme)

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-ayu-dark t)
  (doom-themes-org-config))

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
(use-package burly)

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

(use-package eglot
  :ensure nil
  :custom
  (eglot-events-buffer-size 0)
  (eglot-autoshutdown t)
  (eglot-report-progress nil))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(markdown-mode . ("harper-ls" "--stdio"))))

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
  eww-auto-rename-buffer 1
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

(with-eval-after-load "ispell"
  (setenv "LANG" "pl_PL.UTF-8")
  (setq ispell-program-name "hunspell")
  (setq ispell-dictionary "pl_PL,en_US")
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "pl_PL,en_US")
  (setq ispell-personal-dictionary "~/.config/emacs/files/hunspell_personal"))

(setq ispell-silently-savep t)
(setq flyspell-issue-message-flag nil)

(use-package markdown-mode
  :init
  (setq markdown-unordered-list-item-prefix "  -")
  (setq markdown-hide-urls t)
  (setq markdown-command
      (concat
      "pandoc"
      " --from=markdown --to=html"
      " --standalone --mathjax --highlight-style=pygments")))

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
      ;; ("agenda-agenda.org" :maxlevel . 1)
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

(use-package pulsar
  :hook
  (after-init . pulsar-global-mode)
  :config
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.025)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'evil-ex-lazy-highlight)

  (add-to-list 'pulsar-pulse-functions 'evil-scroll-down)
  (add-to-list 'pulsar-pulse-functions 'flymake-goto-next-error)
  (add-to-list 'pulsar-pulse-functions 'flymake-goto-prev-error)
  (add-to-list 'pulsar-pulse-functions 'evil-yank)
  (add-to-list 'pulsar-pulse-functions 'evil-yank-line)
  (add-to-list 'pulsar-pulse-functions 'evil-delete)
  (add-to-list 'pulsar-pulse-functions 'evil-delete-line)
  (add-to-list 'pulsar-pulse-functions 'evil-jump-item)
  (add-to-list 'pulsar-pulse-functions 'diff-hl-next-hunk)
  (add-to-list 'pulsar-pulse-functions 'diff-hl-previous-hunk))

(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (clojure-mode . rainbow-delimiters-mode)))

(use-package rainbow-mode
  :defer
  :hook (prog-mode org-mode markdown-mode))

(use-package sudo-edit)

(setq tab-bar-new-tab-choice "*scratch*"
      tab-bar-close-button-show nil
      tab-bar-new-button-show nil
      tab-bar-close-last-tab-choice 'tab-bar-mode-disable
      tab-bar-close-tab-select 'recent
      tab-bar-new-tab-to 'right
      tab-bar-tab-hints nil
      tab-bar-separator " "
      tab-bar-show 1)

(use-package treesit-auto
  :after emacs
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode t))

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

(use-package zoxide)

(setq gc-cons-threshold (* 2 1000 1000))
