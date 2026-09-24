;; -*- lexical-binding: t; -*-


(add-to-list 'custom-theme-load-path "~/.config/emacs/lisp/everforest-theme/")

(use-package beacon
	:init
	(beacon-mode 1))

(use-package catppuccin-theme)

(use-package doom-themes
	:config
	(setq doom-themes-enable-bold t
				doom-themes-enable-italic t)
	(doom-themes-org-config))

(load-theme 'modus-vivendi :no-confirm)

;; Transparency (On NixOS, it only works with the "emacs-gtk" package.)
(add-to-list 'default-frame-alist '(alpha-background . 80))

(use-package doom-modeline
	:init (doom-modeline-mode 1)
	:config
	(setq doom-modeline-height 25
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

(use-package hl-todo
	:hook ((org-mode . hl-todo-mode)
				 (markdown-mode . hl-todo-mode)
				 (prog-mode . hl-todo-mode))
	:config
	(setq hl-todo-highlight-punctuation ":"
				hl-todo-keyword-faces
				'(("TODO" . (:foreground "#1a1b26" :background "#f7768e" :weight bold))
					("WAIT" . (:foreground "#1a1b26" :background "#e0af68" :weight bold))
					("FIXME" . (:foreground "#1a1b26" :background "#7aa2f7" :weight bold))
					("CANCELED" . (:foreground "#1a1b26" :background "#73daca" :weight bold))
					("DONE" . (:foreground "#1a1b26" :background "#9ece6a" :weight bold)))))

(use-package indent-guide
	:hook
	(prog-mode . indent-guide-mode)
	:config
	(setq indent-guide-char "│"))

(use-package nerd-icons)

(use-package nerd-icons-dired
	:hook
	(dired-mode . nerd-icons-dired-mode))

(use-package rainbow-delimiters
	:hook ((emacs-lisp-mode . rainbow-delimiters-mode)
				 (clojure-mode . rainbow-delimiters-mode)))

(use-package rainbow-mode
	:ensure t
	:hook prog-mode org-mode markdown-mode)

(use-package tab-bar
	:ensure nil
	:defer t
	:custom
	(tab-bar-new-tab-choice "*scratch*")
	(tab-bar-close-button-show nil)
	(tab-bar-new-button-show nil)
	(tab-bar-tab-hints t)
	(tab-bar-auto-width nil)
	(tab-bar-separator "  ")
	(tab-bar-format '(tab-bar-format-tabs-groups
										tab-bar-separator))
	:init
	(defun tab-bar-tab-name-format-hints (name _tab i)
		(if tab-bar-tab-hints (concat (format "»%d«" i) "") name))
	(defun tab-bar-tab-group-format-default (tab _i &optional current-p)
		(propertize
		 (concat (funcall tab-bar-tab-group-function tab))
		 'face (if current-p 'tab-bar-tab-group-current 'tab-bar-tab-group-inactive)))
	(defun emacs-solo/tab-group-from-project ()
		"Call `tab-group` with the current project name as the group."
		(interactive)
		(when-let* ((proj (project-current))
								(name (file-name-nondirectory
											 (directory-file-name (project-root proj)))))
			(tab-group (format "[%s]" name))))
	(defun emacs-solo/tab-switch-to-group ()
		(interactive)
		(let* ((tabs (funcall tab-bar-tabs-function)))
			(let* ((groups (delete-dups (mapcar (lambda (tab)
																						(funcall tab-bar-tab-group-function tab))
																					tabs)))
						 (group (completing-read "Switch to group: " groups nil t)))
				(let ((i 1) (found nil))
					(dolist (tab tabs)
						(let ((tab-group (funcall tab-bar-tab-group-function tab)))
							(when (and (not found)
												 (string= tab-group group))
								(setq found t)
								(tab-bar-select-tab i)))
						(setq i (1+ i)))))))
	(tab-bar-mode 1)
	(tab-bar-history-mode 1))

(use-package visual-fill-column
	:custom
	(visual-fill-column-width 120)
	(visual-fill-column-center-text t))


(provide 'xd-interface)
