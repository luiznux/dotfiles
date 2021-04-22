;;; evil-config.el --- Package configuration for evil packages
;;; Commentary:
;;; Emacs Evil configuration --- Package configuration for Emacs evil
;;; Code:

(defun setup-evil-packages()
  "Call evil packages."

  (use-package evil
    :ensure t
    :init
    (setq evil-want-integration t
          evil-want-keybinding nil)
    :config
    (evil-mode 1)
    (evil-set-initial-state 'term-mode 'emacs)
    (with-eval-after-load 'evil
      (define-key evil-normal-state-map (kbd "M-.") nil)
      (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)))

  (use-package evil-collection
    :after evil
    :ensure t
    :config
    (evil-collection-init)
    (setq evil-collection-setup-minibuffer t
          evil-collection-company-use-tng  nil))

  (use-package evil-matchit
    :ensure t
    :config
    (global-evil-matchit-mode 1))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode 1))

  (use-package evil-leader
    :ensure t)

  (use-package evil-org
    :ensure t)

  (use-package undo-tree ;dependency for evil-undo-system
    :ensure t
    :config
    (global-undo-tree-mode))

  (use-package evil-mode
    :hook (org-mode . evil-org-mode)))

(setup-evil-packages)

(provide 'evil-config)
;;; evil-config.el ends here
