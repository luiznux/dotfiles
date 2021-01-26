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

(use-package lsp-origami
  :ensure t
  :config
  (add-hook 'lsp-after-open-hook #'lsp-origami-try-enable))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  ;;; custom flycheck buffer display(smaller and at the bottom)
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-below-selected)
                 (reusable-frames . visible)
                 (side            . bottom)
                 (window-height   . 0.2))))

(use-package plantuml-mode
  :ensure t)

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred) (sh-mode . lsp))

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t)
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package company
  :ensure t
  :bind
  (:map company-active-map
        ("M-j" . company-select-next-or-abort)
        ("M-k" . company-select-previous-or-abort)
        ("<tab>" . company-complete-common-or-cycle))
  :hook
  (after-init . global-company-mode)
    (plantuml-mode . (lambda () (set (make-local-variable 'company-backends)
                              '((company-yasnippet
                                 ;; company-dabbrev
                                 )))))

  :config
  (setq company-idle-delay 0
        company-echo-delay 0
        company-minimum-prefix-length 1)

  (use-package company-posframe
    :ensure t
    :hook (company-mode . company-posframe-mode))

  ;; Show pretty icons
  (use-package company-box
    :ensure t
    :hook (company-mode . company-box-mode)
    :init (setq company-box-icons-alist 'company-box-icons-all-the-icons)
    :config
    (setq company-box-backends-colors nil)
    (setq company-box-show-single-candidate t)
    (setq company-box-max-candidates 50))

  (defun company-box-icons--elisp (candidate)
    (when (derived-mode-p 'emacs-lisp-mode)
      (let ((sym (intern candidate)))
        (cond ((fboundp sym) 'Function)
              ((featurep sym) 'Module)
              ((facep sym) 'Color)
              ((boundp sym) 'Variable)
              ((symbolp sym) 'Text)
              (t . nil)))))

  (with-eval-after-load 'all-the-icons
    (declare-function all-the-icons-faicon 'all-the-icons)
    (declare-function all-the-icons-fileicon 'all-the-icons)
    (declare-function all-the-icons-material 'all-the-icons)
    (declare-function all-the-icons-octicon 'all-the-icons)
    (setq company-box-icons-all-the-icons
          `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.7 :v-adjust -0.15))
            (Text . ,(all-the-icons-faicon "book" :height 0.68 :v-adjust -0.15))
            (Method . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
            (Function . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
            (Constructor . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
            (Field . ,(all-the-icons-faicon "tags" :height 0.65 :v-adjust -0.15 :face 'font-lock-warning-face))
            (Variable . ,(all-the-icons-faicon "tag" :height 0.7 :v-adjust -0.05 :face 'font-lock-warning-face))
            (Class . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01 :face 'font-lock-constant-face))
            (Interface . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01))
            (Module . ,(all-the-icons-octicon "package" :height 0.7 :v-adjust -0.15))
            (Property . ,(all-the-icons-octicon "package" :height 0.7 :v-adjust -0.05 :face 'font-lock-warning-face)) ;; Golang module
            (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.7 :v-adjust -0.15))
            (Value . ,(all-the-icons-material "format_align_right" :height 0.7 :v-adjust -0.15 :face 'font-lock-constant-face))
            (Enum . ,(all-the-icons-material "storage" :height 0.7 :v-adjust -0.15 :face 'all-the-icons-orange))
            (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.7 :v-adjust -0.15))
            (Snippet . ,(all-the-icons-faicon "code" :height 0.7 :v-adjust 0.02 :face 'font-lock-variable-name-face))
            (Color . ,(all-the-icons-material "palette" :height 0.7 :v-adjust -0.15))
            (File . ,(all-the-icons-faicon "file-o" :height 0.7 :v-adjust -0.05))
            (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.7 :v-adjust -0.15))
            (Folder . ,(all-the-icons-octicon "file-directory" :height 0.7 :v-adjust -0.05))
            (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.7 :v-adjust -0.15 :face 'all-the-icons-blueb))
            (Constant . ,(all-the-icons-faicon "tag" :height 0.7 :v-adjust -0.05))
            (Struct . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01 :face 'font-lock-constant-face))
            (Event . ,(all-the-icons-faicon "bolt" :height 0.7 :v-adjust -0.05 :face 'all-the-icons-orange))
            (Operator . ,(all-the-icons-fileicon "typedoc" :height 0.65 :v-adjust 0.05))
            (TypeParameter . ,(all-the-icons-faicon "hashtag" :height 0.65 :v-adjust 0.07 :face 'font-lock-const-face))
            (Template . ,(all-the-icons-faicon "code" :height 0.7 :v-adjust 0.02 :face 'font-lock-variable-name-face)))))

  ;; Show quick tooltip
  (use-package company-quickhelp
    :ensure t
    :defines company-quickhelp-delay
    :bind (:map company-active-map
                ("M-h" . company-quickhelp-manual-begin))
    :hook (global-company-mode . company-quickhelp-mode)
    :custom (company-quickhelp-delay 0.8)))


(use-package company-lsp
  :ensure t
  :config
  (company-lsp-enable-snippet t)
  (company-lsp-cache-candidates 'auto)
  :commands company-lsp)

(use-package ccls
  :ensure t
  :config (setq ccls-executable "/usr/bin/ccls")
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))

(use-package lsp-java
 :hook 'java-mode-hook #'lsp)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package latex-preview-pane
  :ensure t
  :init
  (latex-preview-pane-enable))

;(use-package telephone-line
;  :ensure t
;  :init
;  (telephone-line-mode 1))

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

(use-package smex
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'smex))

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package anzu
  :ensure t
  :diminish
  :bind
  ("C-r"   . anzu-query-replace-regexp)
  ("C-M-r" . anzu-query-replace-at-cursor-thing)
  :hook
  (after-init . global-anzu-mode))

(use-package logview
  :ensure t
  :defer t)

(use-package sudo-edit
  :ensure t)

(use-package google-translate
  :ensure t
  :bind
  ("M-o t" . google-translate-at-point)
  ("M-o T" . google-translate-at-point-reverse)
  :custom
  (google-translate-default-source-language "en")
  (google-translate-default-target-language "ja"))

(use-package google-this
  :ensure t)

(use-package all-the-icons
  :ensure t)

(use-package emojify
  :ensure t
  :config
  (setq emojify-company-tooltips-p t
        emojify-composed-text-p    nil)
  :hook (after-init . global-emojify-mode))

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

(use-package rainbow-mode
  :ensure t
  :hook (emacs-lisp-mode . rainbow-mode))

(use-package ranger
  :ensure t
  :config
  (ranger-override-dired-mode t))

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


;;; -------- JS config
(use-package rjsx-mode
  :ensure t
  :mode "\\.js\\'")

(defun setup-tide-mode()
  "Setup funcion for tide."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package tide
  :ensure t
  :after (rjsx-mode company flycheck)
  :hook (rjsx-mode . setup-tide-mode))

(use-package prettier-js
  :ensure t
  :after (rjsx-mode)
  :hook (rjsx-mode . prettier-js-mode))


;;; ------ Local packages(from github)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/awesome-tab"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/page-break-lines"))
;(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/origami.el"))
;(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/nerd-fonts.el"))

(defun setup-awesome-tab()
  (require 'awesome-tab)
  (setq awesome-tab-display-icon t)
  (awesome-tab-mode t))

(defun setup-page-break-lines()
  (require 'page-break-lines)
  (turn-on-page-break-lines-mode))


;(defun setup-nerd-fonts-el()
;  (require 'nerd-fonts))

(load "~/.emacs.d/lisp/custom-modes-config.el")
(load "~/.emacs.d/lisp/evil-config.el")
(load "~/.emacs.d/lisp/python-config.el")
(load "~/.emacs.d/lisp/git-config.el")
(load "~/.emacs.d/lisp/project-config.el")
(load "~/.emacs.d/lisp/org-config.el")
(load "~/.emacs.d/lisp/agenda-config.el")
(load "~/.emacs.d/line-mode.el")
;(load "~/.emacs.d/lisp/irony-config.el")

(setup-evil-packages)
(setup-project-packages)
(setup-custom-modes-packages)
(setup-python-packages)
(setup-git-packages)
(setup-awesome-tab)
(setup-page-break-lines)
;(setup-origami-mode)
;(setup-irony-packages)
;(setup-nerd-fonts-el)

;;; packages.el ends here
