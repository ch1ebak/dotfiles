;; -*- lexical-binding: t; -*-


(use-package eglot
	:ensure nil
	:custom
	(eglot-events-buffer-size 0)
	(eglot-autoshutdown t)
	(eglot-report-progress nil)
  :hook ((sh-mode . eglot-ensure)
         (nix-mode . eglot-ensure)
         (lua-mode . eglot-ensure)
         (web-mode . eglot-ensure))
  :config
	(add-to-list 'eglot-server-programs '(markdown-mode . ("harper-ls" "--stdio")))
  (add-to-list 'eglot-server-programs '((sh-mode) . ("bash-language-server" "start")))
  (add-to-list 'eglot-server-programs '(lua-mode . ("lua-language-server")))

  (setq eglot-autoshutdown t
        eglot-confirm-server-initiated-edits nil)

  (defun my/eglot-format-on-save ()
    "Format buffer with eglot before saving."
    (when (and (bound-and-true-p eglot--managed-mode)
               (member major-mode '(rust-mode python-mode typescript-mode tsx-ts-mode js-mode)))
      (eglot-format-buffer)))

  (add-hook 'before-save-hook #'my/eglot-format-on-save))

(use-package ghostel
	:ensure t)

(use-package evil-ghostel
	:after (ghostel evil)
	:hook (ghostel-mode . evil-ghostel-mode))

(use-package lua-mode
  :mode "\\.lua\\'")

(use-package magit)

(use-package nix-mode
  :mode "\\.nix\\'"
  :config
  (setq nix-indent-function 'nix-indent-line))

(use-package sh-script
  :ensure nil
  :mode (("\\.sh\\'" . sh-mode)
         ("\\.bashrc\\'" . sh-mode))
  :config
  (setq sh-basic-offset 2
        sh-indentation 2))

(use-package text-mode
  :ensure nil
  :hook (text-mode . dw/display-column-indicator-in-text-mode))

(provide 'xd-dev)
