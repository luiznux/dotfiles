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
  
  (use-package js2-mode
    :ensure t
    :defer t)
  
  (use-package apache-mode
    :ensure t
    :defer t)
  
  (use-package php-mode
    :ensure t
    :defer t
    :config
    (add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode)))
  
  (use-package web-mode
    :ensure t
    :defer t
    :config
    (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
    (setq web-mode-css-indent-offset 4))
  
  (use-package markdown-mode
    :ensure t
    :defer t)
  
  (use-package arduino-mode
    :ensure t
    :defer t)
  
  (use-package yaml-mode
    :ensure t
    :defer t
    :config
    (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))
  
  (use-package groovy-mode
    :ensure t
    :defer t)

  (use-package kotlin-mode
    :ensure t
    :defer t)

  (use-package clojure-mode
    :ensure t)

  (use-package cider
    :ensure t))


(provide 'custom-modes-config)

;;; custom-modes-config.el ends here
