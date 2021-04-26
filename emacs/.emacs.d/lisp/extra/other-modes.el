;;; other-modes.el --- Package configuration for other modes packages
;;; Commentary:
;;; Emacs other modes configuration.
;;; Code:

(defun setup-other-modes()
  "Call custom mode packages."

  (use-package dockerfile-mode
    :ensure t
    :defer t)

  (use-package gitignore-mode
    :ensure t
    :defer t)

  (use-package markdown-mode
    :ensure t
    :defer t)

  (use-package yaml-mode
    :ensure t
    :config
    (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

  (use-package clojure-snippets
    :ensure t)

  (use-package clojure-mode
    :ensure t)

  (use-package cider
    :ensure t)

  (use-package go-mode
    :ensure t
    :config
    (autoload 'go-mode "go-mode" nil t)
    (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode)))

  (use-package docker
    :ensure t
    :bind ("C-c d" . docker))

  (use-package esup
    :ensure t
    :pin melpa))


(setup-other-modes)

(provide 'custom-modes-config)
;;; other-modes.el ends here
