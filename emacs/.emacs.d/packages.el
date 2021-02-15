;;; packages.el --- Package configuration file
;;; Commentary:
;;; Emacs Packages configuration --- Package configuration for Emacs
;;;
;;;
;;; ██████╗  █████╗  ██████╗██╗  ██╗ █████╗  ██████╗ ███████╗███████╗
;;; ██╔══██╗██╔══██╗██╔════╝██║ ██╔╝██╔══██╗██╔════╝ ██╔════╝██╔════╝
;;; ██████╔╝███████║██║     █████╔╝ ███████║██║  ███╗█████╗  ███████╗
;;; ██╔═══╝ ██╔══██║██║     ██╔═██╗ ██╔══██║██║   ██║██╔══╝  ╚════██║
;;; ██║     ██║  ██║╚██████╗██║  ██╗██║  ██║╚██████╔╝███████╗███████║
;;; ╚═╝     ╚═╝  ╚═╝ ╚═════╝╚═╝  ╚═╝╚═╝  ╚═╝ ╚═════╝ ╚══════╝╚══════╝
;;;
;;; Code:

(require 'package)
(setq package-enable-at-startup nil)
(setq packages-check-signature nil)


(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  :hook (go-mode . yas-minor-mode))

(use-package yasnippet-snippets
  :ensure t)

(use-package origami
  :ensure t
  :config
  (global-origami-mode))

(use-package flycheck
  :ensure t
  :init (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (add-to-list 'display-buffer-alist ;; custom flycheck buffer display(smaller and at the bottom)
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-below-selected)
                 (reusable-frames . visible)
                 (side            . bottom)
                 (window-height   . 0.2))))

(use-package flyspell
  :diminish
  :if (executable-find "aspell")
  :hook
  ((org-mode yaml-mode markdown-mode git-commit-mode) . flyspell-mode)
  (prog-mode . flyspell-prog-mode)
  (before-save-hook . flyspell-buffer)
  (flyspell-mode . (lambda ()
                     (dolist (key '("C-;" "C-," "C-."))
                       (unbind-key key flyspell-mode-map))))
  :custom
  (flyspell-issue-message-flag nil)
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together"))
  :custom-face
  (flyspell-incorrect ((t (:underline (:color "#f1fa8c" :style wave)))))
  (flyspell-duplicate ((t (:underline (:color "#50fa7b" :style wave)))))
  :preface
  (defun message-off-advice (oldfun &rest args)
    "Quiet down messages in adviced OLDFUN."
    (let ((message-off (make-symbol "message-off")))
      (unwind-protect
          (progn
            (advice-add #'message :around #'ignore (list 'name message-off))
            (apply oldfun args))
        (advice-remove #'message message-off))))
  :config
  (advice-add #'ispell-init-process :around #'message-off-advice)
  (use-package flyspell-correct-ivy
    :bind ("C-M-:" . flyspell-correct-at-point)
    :config
    (when (eq system-type 'darwin)
      (progn
        (global-set-key (kbd "C-M-;") 'flyspell-correct-at-point)))
    (setq flyspell-correct-interface #'flyspell-correct-ivy)))

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

(use-package lsp-java
  :hook 'java-mode-hook #'lsp)

(use-package lsp-dart
  :ensure t
  :hook (dart-mode . lsp))

(use-package dap-mode
  :ensure t
  :config
;;; Enabling only some features
;(setq dap-auto-configure-features '(sessions locals controls tooltip))
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

(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package latex-preview-pane
  :ensure t
  :init
  (latex-preview-pane-enable))

(require 'iso-transl)
(use-package highlight-indent-guides
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-responsive 'top))

(use-package highlight-symbol
  :bind
  (:map prog-mode-map
  ("M-o h" . highlight-symbol)
  ("M-p" . highlight-symbol-prev)
  ("M-n" . highlight-symbol-next)))

(use-package hide-mode-line
  :ensure t)

(use-package smex
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'smex))

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package sudo-edit
  :ensure t)

(use-package logview
  :ensure t
  :defer t)

(use-package anzu
  :ensure t
  :diminish
  :bind
  ("C-r"   . anzu-query-replace-regexp)
  ("C-M-r" . anzu-query-replace-at-cursor-thing)
  :hook
  (after-init . global-anzu-mode)
  :config
  (custom-set-variables
   '(anzu-mode-lighter "")
   '(anzu-deactivate-region t)
   '(anzu-replace-to-string-separator " => ")))

(use-package all-the-icons
  :ensure t)

(use-package plantuml-mode
  :ensure t)

(use-package emojify
  :ensure t
  :config
  (setq emojify-company-tooltips-p t
        emojify-composed-text-p    nil)
  :hook (after-init . global-emojify-mode))

(use-package rainbow-mode
  :ensure t
  :hook (emacs-lisp-mode . rainbow-mode))

(use-package ranger
  :ensure t
  :config
  (ranger-override-dired-mode t))

(use-package google-translate
  :ensure t
  :bind
  ("M-o t" . google-translate-at-point)
  ("M-o T" . google-translate-at-point-reverse)
  :custom
  (google-translate-default-source-language "en")
  (google-translate-default-target-language "ja"))

(use-package math-preview
  :ensure t)

(use-package switch-window
  :ensure t)

(use-package google-this
  :ensure t)

(use-package dashboard
  :ensure t
  :init
  (progn
    (setq recentf-exclude '("/org/*")) ;prevent  show recent org-agenda files
    (setq dashboard-items '((recents   . 8)
                            (projects  .  6))))
  :config
  (dashboard-setup-startup-hook)

  (setq dashboard-set-heading-icons  t
        dashboard-set-file-icons     t
        dashboard-set-navigator      t
        dashboard-startup-banner     'logo)

  (setq dashboard-navigator-buttons
        `(;;line1
          ((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
            "Homepage"
            "Browse homepage"
            (lambda (&rest _) (browse-url "https://github.com/luiznux")))
           (" " "Refresh" "Refresh" (lambda (&rest _) (dashboard-refresh-buffer)) nil))))

  (add-hook 'dashboard-mode-hook (lambda () (org-agenda t "x")) (lambda () (ace-window)))
  (add-hook 'dashboard-mode-hook (lambda () (goto-char (point-min)))))


;;; ------ Local packages(from github)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/awesome-tab"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/parrot"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/page-break-lines"))

(defun setup-awesome-tab()
  (require 'awesome-tab)
  (setq awesome-tab-display-icon   t
        awesome-tab-height         108)
  (awesome-tab-mode t))

(defun setup-parrot-mode()
  (require 'parrot)
  ;; To see the party parrot in the modeline, turn on parrot mode:
  (parrot-mode)
  (parrot-set-parrot-type 'emacs)
  (setq parrot-num-rotations 6)
  (add-hook 'evil-insert-state-entry-hook #'parrot-start-animation)
  (add-hook 'evil-visual-state-entry-hook #'parrot-start-animation)
  (add-hook 'evil-emacs-state-entry-hook #'parrot-start-animation))

(defun setup-page-break-lines()
  (require 'page-break-lines)
  (turn-on-page-break-lines-mode))


;;; ------ load the others files from /lisp dir
(load "~/.emacs.d/lisp/custom-modes-config.el")
(load "~/.emacs.d/lisp/evil-config.el")
(load "~/.emacs.d/lisp/python-config.el")
(load "~/.emacs.d/lisp/git-config.el")
(load "~/.emacs.d/lisp/project-config.el")
(load "~/.emacs.d/lisp/company-config.el")
(load "~/.emacs.d/lisp/org-config.el")
(load "~/.emacs.d/lisp/agenda-config.el")
(load "~/.emacs.d/line-mode.el")
;(load "~/.emacs.d/lisp/irony-config.el") not used anymore(use lsp instead)


;;; ------  call the functions from /lisp dir
(setup-evil-packages)
(setup-project-packages)
(setup-company-config)
(setup-org-packages)
(setup-custom-modes-packages)
(setup-python-packages)
(setup-git-packages)
(setup-awesome-tab)
(setup-page-break-lines)
(setup-parrot-mode)

;;; packages.el ends here
