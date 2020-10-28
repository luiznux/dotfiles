;;; packages.el --- Package configuration file
;;; Commentary:
;;; Emacs Packages configuration --- Package configuration for Emacs
;;;
;;;
;;; ██████╗  █████╗  ██████╗██╗  ██╗ █████╗  ██████╗ ███████╗███████╗
;;; ██╔══██╗██╔══██╗██╔════╝██║ ██╔╝██╔══██╗██╔════╝ ██╔════╝██╔════╝
;;; ██████╔╝███████║██║     █████╔╝ ███████║██║  ███╗█████╗  ███████╗
;;; ██╔═══╝ ██╔══██║██║     ██╔═██╗ ██╔══██║██║   ██║██╔══╝  ╚════██║
;;; ██║     ██║  ██║╚██████╗██║  ██╗██║  ██║╚██████╔╝███████╗███████║
;;; ╚═╝     ╚═╝  ╚═╝ ╚═════╝╚═╝  ╚═╝╚═╝  ╚═╝ ╚═════╝ ╚══════╝╚══════╝
;;;
;;; Code:

(require 'package)
(setq package-enable-at-startup nil)
(setq packages-check-signature nil)


(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))


(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  :hook (go-mode . yas-minor-mode))

(use-package yasnippet-snippets
  :ensure t)

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred) (sh-mode . lsp))

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t)
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (define-key company-active-map (kbd "M-j") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "M-k") 'company-select-previous-or-abort))

(use-package company-lsp
  :ensure t
  :config
  (company-lsp-enable-snippet t)
  (company-lsp-cache-candidates 'auto)
  :commands company-lsp)

(use-package company-quickhelp
  :hook 'after-init-hook company-quickhelp-mode)

(use-package ccls
  :ensure t
  :config (setq ccls-executable "/usr/bin/ccls")
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))

(use-package lsp-java
 :hook 'java-mode-hook #'lsp)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package latex-preview-pane
  :ensure t
  :init
  (latex-preview-pane-enable))

(use-package telephone-line
  :ensure t
  :init
  (telephone-line-mode 1))

(require 'iso-transl)

(use-package highlight-indent-guides
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-responsive 'top))

(use-package smex
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'smex))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook
            (lambda () (org-bullets-mode 1))))

(use-package ranger
  :ensure t
  :config
  (ranger-override-dired-mode t))

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package diff-hl
  :ensure t
  :config
  (global-hl-line-mode)
  (global-diff-hl-mode))

(use-package nlinum
  :ensure t
  :config
  (global-nlinum-mode))
;  (set-face-background 'linum "#303030")
;  (set-face-foreground 'linum "#8b8bcd")
;  (set-face-foreground 'highlight nil))

(use-package all-the-icons
  :ensure t)

(use-package rainbow-mode
  :ensure t)

(use-package evil-mode
  :hook (org-mode . evil-org-mode))

(use-package dashboard
  :ensure t
  :init
  (progn
    (setq recentf-exclude '("/org/*")) ;prevent  show recent org-agenda files
    (setq dashboard-items '((bookmarks . 4)
                            (recents   . 7)
                            (projects  . 6))))
  :config
  (dashboard-setup-startup-hook)
  (add-hook 'dashboard-mode-hook (lambda () (org-agenda t "x")) (lambda () (ace-window)))

  (setq dashboard-set-heading-icons  t
        dashboard-set-file-icons     t
        dashboard-set-navigator      t
        dashboard-startup-banner     'logo)

  (setq dashboard-navigator-buttons
        `(;;line1
          ((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
            "Homepage"
            "Browse homepage"
            (lambda (&rest _) (browse-url "https://github.com/luiznux")))
           (" " "Refresh" "Refresh" (lambda (&rest _) (dashboard-refresh-buffer)) nil)))))

(use-package minimap
  :ensure t
  :custom
  (minimap-major-modes '(prog-mode))
  :config
  (setq minimap-window-location 'right
        minimap-update-delay 0.2
        minimap-highlight-line  t
        minimap-hide-scroll-bar nil
        minimap-highlight-line t
        minimap-display-semantic-overlays t)
  (custom-set-faces
   '(minimap-font-face ((t (:height 32 :family "DejaVu Sans Mono"))))
   '(minimap-active-region-background ((t (:extend t :background "#232526"))))
   '(minimap-current-line-face ((t (:background "#344256"))))))

;;--------------------JAVASCRIPTU
(use-package rjsx-mode
  :ensure t
  :mode "\\.js\\'")

(defun setup-tide-mode()
  "Setup funcion for tide."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package tide
  :ensure t
  :after (rjsx-mode company flycheck)
  :hook (rjsx-mode . setup-tide-mode))

(use-package prettier-js
  :ensure t
  :after (rjsx-mode)
  :hook (rjsx-mode . prettier-js-mode))


;Local packages(github)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/awesome-tab"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/page-break-lines"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/origami.el"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/nerd-fonts.el"))

(defun setup-awesome-tab()
  (require 'awesome-tab)
  (setq awesome-tab-display-icon t)
  (awesome-tab-mode t))

(defun setup-page-break-lines()
  (require 'page-break-lines)
  (turn-on-page-break-lines-mode))

(defun setup-origami-mode ()
  (require 'origami)
  (global-origami-mode))

(defun setup-nerd-fonts-el()
  (require 'nerd-fonts))


(load "~/.emacs.d/lisp/custom-modes-config.el")
(load "~/.emacs.d/lisp/evil-config.el")
(load "~/.emacs.d/lisp/python-config.el")
(load "~/.emacs.d/lisp/irony-config.el")
(load "~/.emacs.d/lisp/git-config.el")
(load "~/.emacs.d/lisp/project-config.el")
(load"~/.emacs.d/lisp/agenda-config.el")


(setup-evil-packages)
(setup-project-packages)
(setup-custom-modes-packages)
(setup-python-packages)
;(setup-irony-packages)
(setup-git-packages)
(setup-awesome-tab)
(setup-page-break-lines)
(setup-origami-mode)
;(setup-nerd-fonts-el)

;;; packages.el ends here
