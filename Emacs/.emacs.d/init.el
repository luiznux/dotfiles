;;; init.el --- Initialization file for Emacs
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs

;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-term-color-vector
   [unspecified "#1d1f21" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb" "#81a2be" "#c5c8c6"] t)
 '(custom-enabled-themes (quote (base16-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("26d49386a2036df7ccbe802a06a759031e4455f07bda559dcf221f53e8850e69" "1263771faf6967879c3ab8b577c6c31020222ac6d3bac31f331a74275385a452" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "fede08d0f23fc0612a8354e0cf800c9ecae47ec8f32c5f29da841fe090dfc450" "cea3ec09c821b7eaf235882e6555c3ffa2fd23de92459751e18f26ad035d2142" default)))
 '(package-selected-packages
   (quote
    (poet poet-theme sly darkroom origami tldr moe-theme flycheck-mypy green-is-the-newblack-theme green-is-the-new-black-theme dracula-theme multi-term writeroom-mode rainbow-mode cider clojure-mode dashboard git-gutter elfeed alchemist nov counsel kotlin-mode groovy-mode org-bullets smex fancy-battery evil-matchit symon projectile ggtags fireplace flycheck-irony company-irony python-docstring irony gitignore-mode dumb-jump ag apache-mode elixir-mode mmm-mode popwin color-identifiers-mode highlight-indent-guides telephone-line yaml-mode dockerfile-mode pyenv-mode all-the-icons neotree gh-md arduino-mode evil-magit async magit flycheck latex-preview-pane ## python-doctring web-mode markdown-mode php-mode js2-mode evil py-autopep8 goto-chg undo-tree elpy base16-theme which-key use-package)))
 '(send-mail-function (quote mailclient-send-it))
 '(standard-indent 4))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#1d1f21" :foreground "#c5c8c6" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "ADBO" :family "Source Code Variable" )))))

(global-hl-line-mode 1)

(when window-system 
   (set-frame-position (selected-frame) 0 0) 
   (set-frame-size (selected-frame) 77 29)) 

(load "~/.emacs.d/main.el")


;;; init.el ends here
