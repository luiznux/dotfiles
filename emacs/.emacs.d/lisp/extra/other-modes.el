;;; other-modes.el --- Package configuration for other modes packages
;;; Commentary:
;;; Emacs other modes configuration.
;;; Code:

(defun setup-other-modes()
  "Call custom mode packages."

  (use-package dockerfile-mode
    :defer t)

  (use-package gitignore-mode
    :defer t)

  (use-package markdown-mode
    :defer t)

  (use-package yaml-mode
    :config
    (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

  (use-package clojure-snippets)

  (use-package clojure-mode)

  (use-package cider)

  (use-package go-mode
    :config
    (autoload 'go-mode "go-mode" nil t)
    (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode)))

  (use-package docker
    :bind ("C-c d" . docker))

  (use-package esup
    :pin melpa))


(setup-other-modes)

(provide 'custom-modes-config)
;;; other-modes.el ends here
