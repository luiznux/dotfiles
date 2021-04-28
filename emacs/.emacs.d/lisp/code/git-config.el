;;; git-config.el --- Package configuration for git packages
;;; Commentary:
;;; Emacs git configuration --- Package configuration for git
;;; Code:

(defun setup-git-packages()
  "Call git packages."

  (use-package magit
    :defer t)

  (use-package magit-todos)

  (use-package diff-hl
    :config
    (global-hl-line-mode)
    (global-diff-hl-mode)
    (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

  (use-package github-pullrequest))


(setup-git-packages)

(provide 'git-config)
;;; git-config.el ends here
