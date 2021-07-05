;;; other-modes.el --- Package configuration for other modes packages ;;; Commentary:
;;; Emacs other modes configuration.
;;; Code:

(defun setup-other-modes()
  "Call custom mode packages."

  (use-package go-mode
    :config
    (autoload 'go-mode "go-mode" nil t)
    (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode)))

  (use-package yaml-mode
    :config
    (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

  (use-package docker
    :bind ("C-c d" . docker))

  (use-package dockerfile-mode
    :defer t)

  (use-package hackernews
    :commands (hackernews))

  ;; games and useless things
  (use-package 2048-game
    :commands (2048-game))
  (use-package speed-type
    :commands (speed-type-text))
  (use-package snow)

  (use-package esup
    :pin melpa)

  (use-package gitignore-mode
    :defer t)

  (use-package markdown-mode
    :defer t)

  (use-package htmlize
    :defer t)

  (use-package csv-mode)

  (use-package memory-usage)

  (use-package vimrc-mode)

  (use-package pyvenv
    :config
    (pyvenv-mode 1))

  (use-package python
    :ensure nil
    :hook (inferior-python-mode . (lambda ()
                                    (process-query-on-exit-flag
                                     (get-process "Python"))))
    :init
    ;; Disable readline based native completion
    (setq python-shell-completion-native-enable nil)
    :config
    ;; Default to Python 3. Prefer the versioned Python binaries since some
    ;; systems stupidly make the unversioned one point at Python 2.
    (when (and (executable-find "python3")
               (string= python-shell-interpreter "python"))
      (setq python-shell-interpreter "python3"))

    ;; Env vars
    (with-eval-after-load 'exec-path-from-shell
      (exec-path-from-shell-copy-env "PYTHONPATH"))

    ;; Live Coding in Python
    (use-package live-py-mode))


  ;;CLOJURE
  (use-package clojure-snippets)
  (use-package clojure-mode)
  (use-package cider
    :config
    (setq cider-repl-pop-to-buffer-on-connect 'display-only
          cider-prompt-for-symbol             t
          nrepl-hide-special-buffers          t
          cider-repl-use-content-types        t
          cider-repl-wrap-history             t
          cider-repl-use-clojure-font-lock    t
          cider-repl-buffer-size-limit        100000
          cider-repl-history-size             1000)
    (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
    (add-hook 'cider-test-report-mode 'jcf-soft-wrap))

  (use-package dizzee
    :commands (jcf-lein-datomic-start jcf-lein-headless-start)
    :config
    (dz-defservice jcf-lein-headless
                   "lein"
                   :cd "~/"
                   :args ("repl" ":headless"))

    (dz-defservice jcf-lein-datomic
                   "lein"
                   :args ("datomic")))

  ;; JavaScript
  (use-package js2-mode
    :defines flycheck-javascript-eslint-executable
    :mode (("\\.js\\'" . js2-mode)
           ("\\.jsx\\'" . js2-jsx-mode))
    :interpreter (("node" . js2-mode)
                  ("node" . js2-jsx-mode))
    :hook ((js2-mode . js2-imenu-extras-mode)
           (js2-mode . js2-highlight-unused-variables-mode))
    :init (setq js-indent-level 2)
    :config
    ;; Use default keybindings for lsp
    (unbind-key "M-." js2-mode-map)

    (with-eval-after-load 'flycheck
      (when (or (executable-find "eslint_d")
                (executable-find "eslint")
                (executable-find "jshint"))
        (setq js2-mode-show-strict-warnings nil))
      (when (executable-find "eslint_d")
        ;; https://github.com/mantoni/eslint_d.js
        ;; npm -i -g eslint_d
        (setq flycheck-javascript-eslint-executable "eslint_d")))

    (use-package js2-refactor
      :diminish
      :hook (js2-mode . js2-refactor-mode)
      :config (js2r-add-keybindings-with-prefix "C-c C-m")))

  (use-package skewer-mode
    :diminish
    :hook (((js-mode js2-mode). skewer-mode)
           (css-mode . skewer-css-mode)
           (web-mode . skewer-html-mode)
           (html-mode . skewer-html-mode))
    :init
    ;; diminish
    (with-eval-after-load 'skewer-css
      (diminish 'skewer-css-mode))
    (with-eval-after-load 'skewer-html
      (diminish 'skewer-html-mode)))

  (use-package typescript-mode
    :mode ("\\.ts[x]\\'" . typescript-mode))

  (use-package web-mode
    :mode "\\.\\(phtml\\|php|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
    :config
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2))

  (use-package impatient-mode))


(setup-other-modes)

(provide 'other-modes)
;;; other-modes.el ends here
