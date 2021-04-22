;;; git-config.el --- Package configuration for git packages
;;; Commentary:
;;; Emacs git configuration --- Package configuration for git
;;; Code:

(defun setup-git-packages()
  "Call git packages."

  (use-package magit
    :ensure t
    :defer t)

  (use-package magit-todos
    :ensure t)

  (use-package diff-hl
    :ensure t
    :config
    (global-hl-line-mode)
    (global-diff-hl-mode)
    (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

  (use-package github-pullrequest
    :ensure t))

;;;;  (use-package git-gutter
;;;;    :ensure t
;;;;    :config
;;;;    (global-git-gutter-mode t)
;;;;    (git-gutter:linum-setup)
;;;;    (custom-set-variables
;;;;     '(git-gutter:update-interval 1)))

(setup-git-packages)

(provide 'git-config)
;;; git-config.el ends here
