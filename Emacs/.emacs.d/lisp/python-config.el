;;; python-config.el --- Package configuration for python packages
;;; Commentary:
;;; Emacs python configuration --- Package configuration for python
;;; Code:

(defun setup-python-packages()
  (use-package elpy
    :ensure t
    :init
    (elpy-enable)
    (setq elpy-rpc-python-command "python3")
    (setq python-shell-interpreter "python3"
          python-shell-interpreter-args "-i"))

  (use-package py-autopep8
    :ensure t
    :config
    (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
    (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))
  
  (use-package pyenv-mode
    :ensure t))

(provide 'python-config)

;;; python-config.el ends here
