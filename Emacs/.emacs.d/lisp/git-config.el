;;; git-config.el --- Package configuration for git packages
;;; Commentary:
;;; Emacs git configuration --- Package configuration for git
;;; Code:

(defun setup-git-packages()
  (use-package magit
    :ensure t
    :defer t)

  (use-package git-gutter
    :ensure t
    :config
    (global-git-gutter-mode t)
    (git-gutter:linum-setup)
    (custom-set-variables
     '(git-gutter:update-interval 1))))

(provide 'git-config)

;;; git-config.el ends here
