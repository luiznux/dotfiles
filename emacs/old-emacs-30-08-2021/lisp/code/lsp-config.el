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


(use-package lsp-mode
  :defines (lsp-clients-python-library-directories
            lsp-rust-server
            lsp-bash-highlight-parsing-errors
            lsp-bash-explainshell-endpoint
            lsp-bash-glob-pattern)

  :commands (lsp
             lsp-deferred
             lsp-enable-which-key-integration
             lsp-format-buffer
             lsp-organize-imports
             lsp-install-server)
  :hook
  (go-mode . lsp-deferred)
  (sh-mode . lsp)
  (web-mode . lsp)
  ((clojure-mode . lsp)(clojurec-mode . lsp) (clojurescript-mode . lsp))
  (lsp-mode . (lambda ()
                ;; Integrate `which-key'
                (lsp-enable-which-key-integration)
                (add-hook 'before-save-hook #'lsp-format-buffer t t)
                (add-hook 'before-save-hook #'lsp-organize-imports t t)))

  :init
  ;; @see https://emacs-lsp.github.io/lsp-mode/page/performance
  (setq read-process-output-max (* 1024 1024)) ;; 1MB

  :config
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))

  (setq lsp-bash-highlight-parsing-errors t
        lsp-bash-explainshell-endpoint    t
        lsp-bash-glob-pattern             t)

  (with-no-warnings
    (defun my-lsp--init-if-visible (func &rest args)
      "Not enabling lsp in `git-timemachine-mode'."
      (unless (bound-and-true-p git-timemachine-mode)
        (apply func args)))
    (advice-add #'lsp--init-if-visible :around #'my-lsp--init-if-visible))

  (defun lsp-update-server ()
    "Update LSP server."
    (interactive)
    ;; Equals to `C-u M-x lsp-install-server'
    (lsp-install-server t)))

(defun lsp-go-install-save-hooks ()
  "Install save hooks."
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t)
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))

(use-package lsp-ui
  :bind ("C-c u" . lsp-ui-imenu)
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable           t
        lsp-ui-doc-header           t
        lsp-ui-peek-enable          t
        lsp-ui-peek-show-directory  t
        lsp-ui-doc-delay            0.5))

(use-package lsp-ivy
  :after lsp-mode
  :config
  (with-no-warnings
    (lsp-defun my-lsp-ivy--format-symbol-match
      ((sym &as &SymbolInformation :kind :location (&Location :uri))
       project-root)
      "Convert the match returned by `lsp-mode` into a candidate string."
      (let* ((sanitized-kind (if (< kind (length lsp-ivy-symbol-kind-icons)) kind 0))
             (type (elt lsp-ivy-symbol-kind-icons sanitized-kind))
             (typestr (if lsp-ivy-show-symbol-kind (format "%s " type) ""))
             (pathstr (if lsp-ivy-show-symbol-filename
                          (propertize (format " · %s" (file-relative-name (lsp--uri-to-path uri) project-root))
                                      'face font-lock-comment-face)
                        "")))
        (concat typestr (lsp-render-symbol-information sym ".") pathstr)))
    (advice-add #'lsp-ivy--format-symbol-match :override #'my-lsp-ivy--format-symbol-match)))


(use-package lsp-treemacs
  :config
  (lsp-treemacs-sync-mode 1))

(use-package lsp-origami
  :config
  (add-hook 'lsp-after-open-hook #'lsp-origami-try-enable))

(use-package lsp-dart
  :hook (dart-mode . lsp))

(use-package dap-mode
  :defines dap-python-executable
  :bind (:map lsp-mode-map
              ("<f5>" . dap-debug))
  :hook (after-init . dap-auto-configure-mode)
  :init (when (executable-find "python3")
          (setq dap-python-executable "python3"))
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1))

(use-package lsp-python-ms
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp)))) ; or lsp-deferred
;; Python: pyright
;;  (use-package lsp-pyright
;;    :preface
;;    ;; Use yapf to format
;;    (defun lsp-pyright-format-buffer ()
;;      (interactive)
;;      (when (and (executable-find "yapf") buffer-file-name)
;;        (call-process "yapf" nil nil nil "-i" buffer-file-name)))
;;    :hook (python-mode . (lambda ()
;;                           (require 'lsp-pyright)
;;                           (add-hook 'after-save-hook #'lsp-pyright-format-buffer t t)))
;;    :init (when (executable-find "python3")
;;            (setq lsp-pyright-python-executable-cmd "python3")))

(use-package lsp-java
  :hook 'java-mode-hook #'lsp)

(use-package ccls
  :config (setq ccls-executable "/usr/bin/ccls")
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))


(provide 'lsp-config)
;;; lsp-config.el ends here
