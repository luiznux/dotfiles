;;; elixir-config.el --- Package configuration for elixir packages
;;; Commentary:
;;; Emacs elixir configuration --- Package configuration for elixir
;;; Code:

(defun setup-elixir-packages()
  (use-package elixir-mode
    :ensure t
    :defer t
    :config
    (add-hook 'elixir-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'elixir-format nil t))))

  (use-package alchemist
    :ensure t
    :defer t
    :config
    (setq alchemist-key-command-prefix (kbd "C-c /"))))


(provide 'elixir-config)

;;; elixir-config.el ends here
