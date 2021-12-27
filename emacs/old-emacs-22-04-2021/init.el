;;
;;  ███████╗███╗   ███╗ █████╗  ██████╗███████╗
;;  ██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝
;;  █████╗  ██╔████╔██║███████║██║     ███████╗
;;  ██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║
;;  ███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║
;;  ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝
;;
;;
;;; init.el --- Initialization file for Emacs
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs

;;; Code:

(require 'package)
(setq package-enable-at-startup nil)
(setq package-check-signature nil)

(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(custom-safe-themes
   (quote
    ("fee4e306d9070a55dce4d8e9d92d28bd9efe92625d2ba9d4d654fc9cd8113b7f" "274fa62b00d732d093fc3f120aca1b31a6bb484492f31081c1814a858e25c72e" "97965ccdac20cae22c5658c282544892959dc541af3e9ef8857dbf22eb70e82b" "34ed3e2fa4a1cb2ce7400c7f1a6c8f12931d8021435bad841fdc1192bd1cc7da" "5a39d2a29906ab273f7900a2ae843e9aa29ed5d205873e1199af4c9ec921aaab" "26d49386a2036df7ccbe802a06a759031e4455f07bda559dcf221f53e8850e69" "1263771faf6967879c3ab8b577c6c31020222ac6d3bac31f331a74275385a452" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "fede08d0f23fc0612a8354e0cf800c9ecae47ec8f32c5f29da841fe090dfc450" "cea3ec09c821b7eaf235882e6555c3ffa2fd23de92459751e18f26ad035d2142" default)))
 '(fci-rule-color "#dedede")
 '(evil-undo-system 'undo-tree);fix C-r undo on evil mode
 '(global-auto-revert-mode t)
 '(line-spacing 0.2)
 '(package-selected-packages
   (quote
    (page-break-lines dashboard dash-functional company-lsp lsp-ui lsp-mode go-mode evil-surround docker treemacs-icons-dired treemacs-magit treemacs-evil treemacs-projectile treemacs ranger hydra tldr flycheck-mypy multi-term git-gutter counsel org-bullets smex evil-matchit projectile ggtags flycheck-irony company-irony python-docstring irony gitignore-mode dumb-jump ag mmm-mode popwin color-identifiers-mode highlight-indent-guides telephone-line dockerfile-mode pyenv-mode all-the-icons gh-md evil-magit async magit flycheck ## python-doctring markdown-mode evil py-autopep8 goto-chg undo-tree elpy which-key use-package)))
 '(send-mail-function (quote mailclient-send-it))
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
 '(org-scheduled-today ((t (:foreground "deep sky blue")))))

; :background "#0b0719"
;old background "#1d1f21"

(load "~/.emacs.d/main.el")

;;; init.el ends here
