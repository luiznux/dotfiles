;;; custom-modes-config.el --- Package configuration for custom modes packages
;;; Commentary:
;;; Emacs custom modes configuration --- Package configuration for Emacs custom modes
;;; Code:

(defun setup-custom-modes-packages()
  (use-package dockerfile-mode
    :ensure t
    :defer t)

  (use-package gitignore-mode
    :ensure t
    :defer t)

  (use-package markdown-mode
    :ensure t
    :defer t)

  (use-package go-mode
    :ensure t
    :config
    (autoload 'go-mode "go-mode" nil t)
    (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))))

(provide 'custom-modes-config)

;;; custom-modes-config.el ends here
