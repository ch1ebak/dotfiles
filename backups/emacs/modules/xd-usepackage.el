;; -*- lexical-binding: t; -*-


(require 'use-package-ensure)

(setq use-package-always-ensure t
			package-enable-at-startup nil
			package-archives '(("melpa" . "https://melpa.org/packages/")
												 ("org" . "https://orgmode.org/elpa/")
												 ("elpa" . "https://elpa.gnu.org/packages/")
												 ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(setq package-quickstart t)


(provide 'xd-usepackage)
