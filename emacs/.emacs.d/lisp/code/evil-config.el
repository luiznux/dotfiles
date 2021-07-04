;;; evil-config.el --- Package configuration for evil packages
;;; Commentary:
;;; Emacs Evil configuration --- Package configuration for Emacs evil
;;; Code:

(defun setup-evil-packages()
  "Call evil packages."

  (use-package evil
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
    :config
    (evil-collection-init)
    (setq evil-collection-setup-minibuffer t
          evil-collection-company-use-tng  nil))

  (use-package evil-matchit
    :config
    (global-evil-matchit-mode 1))

  (use-package evil-surround
    :config
    (global-evil-surround-mode 1))

  (use-package evil-leader
    :config
    (global-evil-leader-mode))

  (use-package evil-org
    :after evil org
    :hook (org-mode . (lambda () evil-org-mode))
    :config
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys))

  (use-package undo-tree ;; dependency for evil-undo-system
    :config
    (global-undo-tree-mode)))


(setup-evil-packages)

(provide 'evil-config)
;;; evil-config.el ends here
