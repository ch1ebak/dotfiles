;; -*- lexical-binding: t; -*-

;; ░██████████ ░███     ░███    ░███      ░██████    ░██████   
;; ░██         ░████   ░████   ░██░██    ░██   ░██  ░██   ░██  
;; ░██         ░██░██ ░██░██  ░██  ░██  ░██        ░██         
;; ░█████████  ░██ ░████ ░██ ░█████████ ░██         ░████████  
;; ░██         ░██  ░██  ░██ ░██    ░██ ░██                ░██ 
;; ░██         ░██       ░██ ░██    ░██  ░██   ░██  ░██   ░██  
;; ░██████████ ░██       ░██ ░██    ░██   ░██████    ░██████   
;;
;; github.com/ch1ebak
                                                                   

(setq gc-cons-threshold #x40000000)

(setq read-process-output-max (* 1024 1024 4))

;; Modules
(add-to-list 'load-path '"~/.config/emacs/modules")

(require 'xd-usepackage)
(require 'xd-settings)
(require 'xd-keymaps)
(require 'xd-interface)
(require 'xd-ivy)
;; (require 'xd-vertico)
(require 'xd-packages)
(require 'xd-dev)
(require 'xd-writing)


(setq gc-cons-threshold (* 2 1000 1000))
