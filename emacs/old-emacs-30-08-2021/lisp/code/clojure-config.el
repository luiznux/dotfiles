
;;CLOJURE
(use-package clojure-snippets)
(use-package inf-clojure)
(use-package zprint-mode)
(use-package clojure-mode)
(use-package clj-refactor
  :config
  (setq cljr-hotload-dependencies t))

(use-package cider
  :functions 'cider-company-enable-fuzzy-completion
  :config
  (setq cider-repl-pop-to-buffer-on-connect      'nil ;;display-only
        cider-completion-annotations-include-ns  'always
        cider-prompt-for-symbol                  t
        nrepl-hide-special-buffers               t
        cider-repl-use-content-types             t
        cider-repl-wrap-history                  t
        cider-repl-use-clojure-font-lock         t
        cider-repl-buffer-size-limit             100000
        cider-repl-history-size                  1000)

  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  (add-hook 'cider-test-report-mode 'jcf-soft-wrap)
  (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion))

(use-package eros
  :config
  (eros-mode 1))

(use-package dizzee
  :functions jcf-lein-datomic-stop jcf-lein-headless-stop
  :commands (jcf-lein-datomic-start jcf-lein-headless-start)
  :config
  (dz-defservice jcf-lein-headless
                 "lein"
                 :cd "~/"
                 :args ("repl" ":headless"))

  (dz-defservice jcf-lein-datomic
                 "lein"
                 :args ("datomic")))

(provide 'clojure-config)
