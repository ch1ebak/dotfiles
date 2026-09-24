;; -*- lexical-binding: t; -*-


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

(use-package flyspell
	:ensure nil
	:config
	(setq ispell-program-name "hunspell"
				ispell-dictionary "pl_PL,en_US"
				ispell-personal-dictionary "~/.config/emacs/files/hunspell_personal"
				ispell-silently-savep t)
	:custom
	(ispell-set-spellchecker-params)
	(ispell-hunspell-add-multi-dic "pl_PL,en_US"))


(use-package markdown-mode
	:commands gfm-mode markdown-mode
	:mode
	("README\\.md\\'" . gfm-mode)
	("\\.md\\'" . markdown-mode)
	("\\.markdown\\'" . markdown-mode)
	:init
	(setq markdown-unordered-list-item-prefix "  -"
				markdown-hide-urls t
				markdown-list-indent-width 2
				markdown-enable-highlighting-syntax t
				markdown-max-image-size '( 600 . 600)
				markdown-command (concat "pandoc" " --from=markdown --to=html")))


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
	'((sequence "TODO(t)" "WAIT(w)" "FIXME(f)" "|" "CANCELED(c)" "DONE(d)"))))

(use-package org-tempo
	:ensure nil)

(use-package org-habit
	:ensure nil
	:config
	(setq org-habit-graph-column 60))


(use-package typopunct
	:load-path "~/.config/emacs/lisp/typopunct"
	:custom
	(typopunct-buffer-language 'english))


(provide 'xd-writing)
