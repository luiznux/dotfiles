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
  (global-diff-hl-mode))

(use-package nlinum
  :ensure t
  :config
  (global-nlinum-mode)
  (set-face-background 'linum "#303030")
  (set-face-foreground 'linum "#8b8bcd")
  (set-face-foreground 'highlight nil))

(use-package treemacs
  :ensure t
  :defer t
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-follow-mode                   t
          treemacs-file-follow-delay             0.1
          treemacs-filewatch-mode                nil
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   1
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-fringe-indicator-mode         t
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        1
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    1
          treemacs-recenter-after-tag-follow     1
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   t
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              0.1
          treemacs-user-mode-line-format         nil
          treemacs-width                         15)
    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)
    (treemacs-get-icon-value 'root nil "Default")
    (treemacs-get-icon-value "org" t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package treemacs-persp
  :after treemacs persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package all-the-icons
  :ensure t)

(use-package rainbow-mode
  :ensure t)

(use-package evil-leader
  :ensure t)

(use-package evil-org
  :ensure t)

(use-package evil-mode
  :hook (org-mode . evil-org-mode))

(use-package undo-tree ;dependency for evil-undo-system
  :ensure t
  :config
  (global-undo-tree-mode))

(use-package org-super-agenda
  :ensure t
  :config (org-super-agenda-mode t))


;Local packages(github)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/awesome-tab"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/emacs-dashboard"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/page-break-lines"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/origami.el"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/nerd-fonts.el"))

(defun setup-awesome-tab()
  (require 'awesome-tab)
  (setq awesome-tab-display-icon t)
  (awesome-tab-mode t))

(defun setup-emacs-dashboard()
  (require 'dashboard)
  (dashboard-setup-startup-hook)
  (add-hook 'dashboard-mode-hook (lambda () (org-agenda t "x")) (lambda () (ace-window)))

  (setq dashboard-set-heading-icons  t
        dashboard-set-file-icons     t
        show-week-agenda-p           t
        dashboard-set-navigator      t
        dashboard-startup-banner     'logo)
  (setq dashboard-itens '((recents   .  7)
                          (agenda    .  7)
                          (bookmarks .  4)))

 ;; Format: "(icon title help action face prefix suffix)"
(setq dashboard-navigator-buttons
      `(;;line1
        ((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
           "Homepage"
           "Browse homepage"
           (lambda (&rest _) (browse-url "https://github.com/luiznux")))
         (" " "Refresh" "Refresh" (lambda (&rest _) (dashboard-refresh-buffer)) nil)))))

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


(setup-evil-packages)
(setup-project-packages)
(setup-custom-modes-packages)
(setup-python-packages)
;(setup-irony-packages)
(setup-git-packages)
(setup-awesome-tab)
(setup-emacs-dashboard)
(setup-page-break-lines)
(setup-origami-mode)
;(setup-nerd-fonts-el)

;;; packages.el ends here
