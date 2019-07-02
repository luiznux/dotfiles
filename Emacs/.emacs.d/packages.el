;;; packages.el --- Package configuration file
;;; Commentary:
;;; Emacs Packages configuration --- Package configuration for Emacs
;;;
;;;                   _
;;;  _ __   __ _  ___| | ____ _  __ _  ___  ___
;;; | '_ \ / _` |/ __| |/ / _` |/ _` |/ _ \/ __|
;;; | |_) | (_| | (__|   < (_| | (_| |  __/\__ \
;;; | .__/ \__,_|\___|_|\_\__,_|\__, |\___||___/
;;; |_|                         |___/
;;;
;;; Code:

(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck-mypy
  :ensure t)

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0)
  (define-key company-active-map (kbd "M-j") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "M-k") 'company-select-previous-or-abort))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package fancy-battery
  :ensure t
  :config
  (fancy-battery-mode))

(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-tomorrow-night t))

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
  :defer t
  :init
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'column)
  (setq highlight-indent-guides-character ?\|))

(use-package smex
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'smex))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook
            (lambda () (org-bullets-mode 1))))

(use-package nov
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package elfeed
  :ensure t
  :defer t
  :config
  (global-set-key (kbd "C-x w") 'elfeed)
  (elfeed-feeds
   '("https://undeadly.org/cgi?action=rss")))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5))))

(use-package sly
  :ensure t
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl"))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

(load "~/.emacs.d/lisp/python-config.el")
(load "~/.emacs.d/lisp/evil-config.el")
(load "~/.emacs.d/lisp/irony-config.el")
(load "~/.emacs.d/lisp/custom-modes-config.el")
(load "~/.emacs.d/lisp/git-config.el")
(load "~/.emacs.d/lisp/elixir-config.el")
(load "~/.emacs.d/lisp/project-config.el")

(setup-custom-modes-packages)
(setup-evil-packages)
(setup-python-packages)
(setup-irony-packages)
(setup-elixir-packages)
(setup-git-packages)
(setup-project-packages)

;;; packages.el ends here
