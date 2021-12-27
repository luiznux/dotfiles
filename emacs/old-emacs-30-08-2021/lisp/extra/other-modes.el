;;; other-modes.el --- Package configuration for other modes packages.
;;; Commentary:
;;; Emacs other modes configuration.
;;;
;;; Code:

(use-package go-mode
  :config
  (autoload 'go-mode "go-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode)))


(use-package docker
  :bind ("C-c d" . docker))

(use-package dockerfile-mode
  :defer t)

(use-package olivetti
  :config
  (setq-default olivetti-body-width 100))

;; games and useless things
(use-package hackernews
  :commands (hackernews))
(use-package 2048-game
  :commands (2048-game))
(use-package speed-type
  :commands (speed-type-text))
(use-package snow)

(use-package vimrc-mode)
(use-package google-this)
(use-package google-translate
  :bind
  ("M-o t" . google-translate-at-point)
  ("M-o T" . google-translate-at-point-reverse)
  :custom
  (google-translate-default-source-language "en")
  (google-translate-default-target-language "ja"))


;; emacs stuffs
(use-package esup
  :config
  ;; Work around a bug where esup tries to step into the byte-compiled
  ;; version of `cl-lib', and fails horribly.
  (setq esup-depth 0)
  :pin melpa)

(use-package benchmark-init
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package memory-usage)
(use-package bug-hunter)
(use-package logview
  :defer t)


(provide 'other-modes)
;;; other-modes.el ends here
