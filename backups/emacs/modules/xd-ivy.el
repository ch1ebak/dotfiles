;; -*- lexical-binding: t; -*-


(me/leader-keys
	"SPC" '(execute-extended-command :wk "M-x")
	"RET" '(counsel-bookmark :wk "Bookmarks")
	"." '(counsel-fzf :wk "FZF")
	">" '(dired-jump :wk "Dired")
	"," '(counsel-ibuffer :wk "Buffers")
	"<" '(kill-buffer :wk "Kill Buffers")
	"?" '(counsel-rg :wk "Grep")
	"/" '(swiper :wk "Search line"))

(use-package nerd-icons-completion
	:config
	(nerd-icons-completion-mode))

(use-package ivy
	:diminish
	:bind (("C-s" . swiper)
				 :map ivy-minibuffer-map
				 ("y" . self-insert-command)
				 ("TAB" . ivy-alt-done)
				 ("C-l" . ivy-alt-done)
				 ("C-j" . ivy-next-line)
				 ("C-k" . ivy-previous-line)
				 :map ivy-switch-buffer-map
				 ("C-k" . ivy-previous-line)
				 ("C-j" . ivy-next-line)
				 ("C-l" . ivy-done)
				 ("C-d" . ivy-switch-buffer-kill)
				 :map ivy-reverse-i-search-map
				 ("C-k" . ivy-previous-line)
				 ("C-j" . ivy-next-line)
				 ("C-d" . ivy-reverse-i-search-kill))
	:custom
	(setq ivy-use-virtual-buffers t)
	(setq ivy-count-format "(%d/%d) ")
	(setq enable-recursive-minibuffers t)
	:config
	(ivy-mode 1))

(use-package counsel
	:bind (("C-M-j" . 'counsel-switch-buffer)
				 :map minibuffer-local-map
				 ("C-r" . 'counsel-minibuffer-history))
	:custom
	(counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
	:config
	(counsel-mode 1)
	(setq ivy-initial-inputs-alist nil))

(use-package ivy-rich
	:after ivy
	:init
	(ivy-rich-mode 1)
	:custom
	(ivy-virtual-abbreviate 'full
	 ivy-rich-switch-buffer-align-virtual-buffer t
	 ivy-rich-path-style 'abbrev)
	:config
	(ivy-set-display-transformer 'ivy-switch-buffer
															 'ivy-rich-switch-buffer-transformer))

(use-package ivy-prescient
	:after counsel
	:custom
	(ivy-prescient-enable-filtering nil)
	:config
	(prescient-persist-mode 1)
	(ivy-prescient-mode 1))


(provide 'xd-ivy)
