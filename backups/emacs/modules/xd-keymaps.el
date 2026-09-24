;; -*- lexical-binding: t; -*-


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
  (define-key evil-normal-state-map (kbd "C-.") 'zoxide-travel)
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
  :ensure t
  :config
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
  	"f P" '((lambda () (interactive) (find-file "~/.config/emacs/")) :wk "Notes")
  	"f n" '((lambda () (interactive) (find-file "~/Dokumenty/notatki/")) :wk "Notes")
  	"f N" '((lambda () (interactive) (dired "~/Dokumenty/notatki/")) :wk "Notes")
  	"f r" '(counsel-recentf :wk "Recent files")
  	"f u" '(sudo-edit :wk "Sudo edit file")
  	"f U" '(sudo-edit-find-file :wk "Sudo find file"))
  
  (me/leader-keys
  	"h" '(:ignore t :wk "Emacs")
  	"h d" '(redraw-display :wk "Redraw display (some issues on wayland)")
  	"h l" '(package-upgrade-all :wk "Update packages")
  	"h r" '((lambda () (interactive) (load-file "~/.dotfiles/.config/emacs/init.el")) :wk "Reload emacs config")
  	"h R" '(TJ/emacs-restart-daemon :wk "Reload emacs config"))
  
  (me/leader-keys
  	"p" '(:ignore t :wk "Packages")
  	"p a" '(org-agenda :wk "Org Agenda")
  	"p x" '(org-capture :wk "Org Capture")
  	"p t" '(ghostel :wk "Terminal")
  	"p e" '(elfeed :wk "Elfeed")
  	"p m" '(magit-status :wk "Magit")
  	"p w" '(eww :wk "EWW"))
  
  (me/leader-keys
  	"t" '(:ignore t :wk "Toggles")
  	"t d" '(magit-diff :wk "Diff")
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
  	"w" '(:ignore t :wk "Writing")
  	"w c" '(org-timer-set-timer :wk "Timer")
  	"w s" '(ispell :wk "iSpell")
  	"w t" '(typopunct-mode :wk "Typopunct"))
  
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
  	"] h" 'markdown-move-down
  	"[ h" 'markdown-move-up
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
  	"s" 'dired-sort-toggle-or-edit
  	"c" 'dired-clipboard-copy
  	"x" 'dired-clipboard-cut
  	"r" 'dired-do-rename
  	"p" 'dired-clipboard-paste
  	"h" 'dired-up-directory
  	"l" 'dired-open-file)
  
  (general-nmap
  	:keymaps 'elfeed-search-mode-map
  	"W" 'elfeed-search-browse-url
  	"A" 'elfeed-mark-all-as-read
  	"R" 'elfeed-update))

(use-package which-key
	:ensure nil
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
		which-key-idle-delay 0
		which-key-max-description-length 25
		which-key-allow-imprecise-window-fit nil
		which-key-separator " → " ))


(provide 'xd-keymaps)
