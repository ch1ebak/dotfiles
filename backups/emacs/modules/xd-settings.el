;; -*- lexical-binding: t; -*-


(use-package emacs
  :ensure nil

  :custom
  (global-visual-line-mode 1)
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
  (bidi-display-reordering 'left-to-right)
  (bidi-paragraph-direction 'left-to-right)
  (bidi-inhibit-bpa t)
  (cursor-in-non-selected-windows nil)
  (highlight-nonselected-windows nil)
  
  :config
  (set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 105)
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


(provide 'xd-settings)
