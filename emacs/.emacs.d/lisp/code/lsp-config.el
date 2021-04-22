;;; lsp-config.el --- Package configuration file
;;; Commentary:
;;; Emacs Packages configuration --- Package configuration for Emacs
;;;
;;; ██╗     ███████╗██████╗
;;; ██║     ██╔════╝██╔══██╗
;;; ██║     ███████╗██████╔╝
;;; ██║     ╚════██║██╔═══╝
;;; ███████╗███████║██║
;;; ╚══════╝╚══════╝╚═╝
;;;
;;; Code:

(defun lsp-setup()
  "Call lsp-packages."

  (use-package lsp-mode
    :ensure t
    :commands (lsp lsp-deferred)
    :hook (go-mode . lsp-deferred) (sh-mode . lsp)
    (setq lsp-bash-highlight-parsing-errors t
          lsp-bash-explainshell-endpoint    t
          lsp-bash-glob-pattern             t))

  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t)
    (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))

  (use-package lsp-ui
    :ensure t
    :config
    (setq lsp-ui-doc-enable           t
          lsp-ui-doc-header           t
          lsp-ui-peek-enable          t
          lsp-ui-peek-show-directory  t
          lsp-ui-doc-delay            0.5))

  (use-package lsp-treemacs
    :ensure t
    :config
    (lsp-treemacs-sync-mode 1))

  (use-package lsp-origami
    :ensure t
    :config
    (add-hook 'lsp-after-open-hook #'lsp-origami-try-enable))

  (use-package lsp-dart
    :ensure t
    :hook (dart-mode . lsp))

  (use-package dap-mode
    :ensure t
    :config
;;; Enabling only some features
;;;(setq dap-auto-configure-features '(sessions locals controls tooltip))
    (dap-mode 1)
    ;; The modes below are optional
    (dap-ui-mode 1)
    ;; enables mouse hover support
    (dap-tooltip-mode 1)
    ;; use tooltips for mouse hover
    ;; if it is not enabled `dap-mode' will use the minibuffer.
    (tooltip-mode 1)
    ;; displays floating panel with debug buttons
    ;; requies emacs 26+
    (dap-ui-controls-mode 1))

  (use-package ccls
    :ensure t
    :config (setq ccls-executable "/usr/bin/ccls")
    :hook ((c-mode c++-mode objc-mode cuda-mode) .
           (lambda () (require 'ccls) (lsp))))

  (use-package lsp-java
    :hook 'java-mode-hook #'lsp))


(lsp-setup)
(provide 'lsp-config)
;;; lsp-config.el ends here
