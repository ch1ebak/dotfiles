;; -*- lexical-binding: t; -*-


(me/leader-keys
	"SPC" '(execute-extended-command :wk "M-x")
	"RET" '(consult-bookmark :wk "Bookmarks")
	"." '(consult-find :wk "Find File")
	">" '(dired-jump :wk "Dired")
	"," '(consult-buffer :wk "Buffers")
	"<" '(kill-buffer :wk "Kill Buffers")
	"?" '(consult-ripgrep :wk "Grep")
	"/" '(consult-line :wk "Search line"))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

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


(provide 'xd-vertico)
