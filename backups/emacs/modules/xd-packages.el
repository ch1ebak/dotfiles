;; -*- lexical-binding: t; -*-


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

(use-package dired-clipboard
  :load-path "~/.config/emacs/lisp/dired-clipboard"
  :hook (dired-mode . dired-clipboard-mode))

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


(use-package drag-stuff
  :init
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))


(use-package elfeed
	:config
	(setq elfeed-search-feed-face ":foreground #b3b8c3 :weight bold")
	(setq elfeed-db-directory "~/.config/emacs/files/elfeed/database"))

(defun elfeed-mark-all-as-read ()
	(interactive)
	(elfeed-untag elfeed-search-entries 'unread)
	(elfeed-search-update :force))

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
	browse-url-secondary-browser-function 'browse-url-xdg-open
	shr-use-fonts nil
	shr-indentation 2
	shr-width 100
	eww-auto-rename-buffer 'title
	eww-download-directory "~/Pobrane"
	eww-bookmarks-directory "~/.config/emacs/files/"
	eww-search-prefix "https://frogfind.com/?q=")
	:hook
	(eww-after-render-hook . eww-readable))

(defun eww-new ()
	(interactive)
	(let ((url (read-from-minibuffer "Enter URL or keywords: ")))
		(switch-to-buffer (generate-new-buffer "eww"))
		(eww-mode)
		(eww url)))


(defun kill-other-buffers ()
	"Kill all other buffers."
	(interactive)
	(mapc 'kill-buffer (delq (current-buffer) (buffer-list))))


;; https://www.reddit.com/r/emacs/comments/yzjmmf/comment/ix1y211
(defvar my-min-max-window nil)
(defun my-min-max-window()
	(interactive)
	(if (and (one-window-p) my-min-max-window)
			(window-state-put my-min-max-window)
		(setq my-min-max-window (window-state-get))
		(visual-fill-column-mode)
		(global-visual-line-mode)
		(delete-other-windows)))


(use-package popper
	:init
	(setq popper-reference-buffers
				'("\\*Messages\\*"
					"Output\\*$"
					"\\*Async Shell Command\\*"
					ghostel-mode
					help-mode
					compilation-mode))
	(popper-mode +1)
	(popper-echo-mode +1))                ; For echo area hints


;; https://timothyjohnsonsci.com/writing/2025-12-30-emacs-daemon-restart/
(defun TJ/emacs-restart-daemon ()
  "Restart the Emacs daemon cleanly."
  (interactive)
  (save-some-buffers t)
  (let ((cmd (concat invocation-directory invocation-name)))
    (call-process-shell-command
     (format "%s --daemon &" cmd))
    (kill-emacs)))


(use-package sudo-edit)


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


(use-package wdired
	:ensure nil
	:commands (wdired-change-to-wdired-mode)
	:config
	(setq wdired-allow-to-change-permissions t)
	(setq wdired-create-parent-directories t))


(use-package workgroups2
	:config
	(setq wg-session-file "~/.config/emacs/files/emacs_workgroups")
	:init
	(workgroups-mode 1))


(use-package zoxide)


(provide 'xd-packages)
