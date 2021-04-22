;;; init.el --- Initialization file for Emacs
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;;
;;;  ███████╗███╗   ███╗ █████╗  ██████╗███████╗
;;;  ██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝
;;;  █████╗  ██╔████╔██║███████║██║     ███████╗
;;;  ██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║
;;;  ███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║
;;;  ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝
;;;
;;;
;;; Code:

;;; Speed up Emacs startup time increasing
;;; the garbage collector number of bytes
(setq gc-cons-threshold 100000000)

(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-deactivate-region t)
 '(anzu-mode-lighter "")
 '(anzu-replace-to-string-separator " => ")
 '(blink-cursor-mode nil)
 '(evil-undo-system 'undo-tree)
 '(fci-rule-color "#dedede")
 '(global-auto-revert-mode t)
 '(helm-minibuffer-history-key "M-p")
 '(line-spacing 0.2)
 '(standard-indent 4))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :foreground "#c5c8c6" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "ADBO" :family "Source Code Pro"))))
 '(org-agenda-date-today ((t (:foreground "lime green" :weight ultra-bold))))
 '(org-scheduled ((t (:foreground "SlateBlue2"))))
 '(org-scheduled-previously ((t (:foreground "medium turquoise"))))
 '(org-scheduled-today ((t (:foreground "deep sky blue"))))
 '(quote (mode-line-inactive nil)))

;;; old background "#1d1f21"

(load "~/.emacs.d/main.el")

;;; init.el ends here
