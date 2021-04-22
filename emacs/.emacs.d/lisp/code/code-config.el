;;; code-config.el --- Packages for code features
;;; Commentary:
;;; Code features packages configuration
;;;
;;;    ██████╗ ██████╗ ██████╗ ███████╗
;;;   ██╔════╝██╔═══██╗██╔══██╗██╔════╝
;;;   ██║     ██║   ██║██║  ██║█████╗
;;;   ██║     ██║   ██║██║  ██║██╔══╝
;;;   ╚██████╗╚██████╔╝██████╔╝███████╗
;;;    ╚═════╝ ╚═════╝ ╚═════╝ ╚══════╝
;;;
;;; Packages on this file:
;;;
;;; * yasnippet, yasnippet-snippets
;;; * flycheck, flyspell, flyspell-popup
;;; * origami, format-all, whitespace-cleanup-mode
;;; * aggressive-indent, anzu, sudo-edit, bug-hunter
;;; * logview
;;;
;;; For more packages info, look for look for each one's github.
;;;
;;; Code:

(defun code-setup()
  "Call the packages for code editig."

  (use-package yasnippet
    :ensure t
    :config
    (yas-global-mode 1)
    :hook (go-mode . yas-minor-mode))

  (use-package yasnippet-snippets
    :ensure t)

  (use-package clojure-snippets
    :ensure t)

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

  (use-package flyspell-popup
    :ensure t
    :config
    (define-key flyspell-mode-map (kbd "C-;") #'flyspell-popup-correct)
    (add-hook 'flyspell-mode-hook #'flyspell-popup-auto-correct-mode))

  (use-package origami
    :ensure t
    :config
    (global-origami-mode))

  (use-package format-all
    :ensure t)

  ;;; https://github.com/purcell/whitespace-cleanup-mode
  (use-package whitespace-cleanup-mode
    :ensure t
    :config
    (setq  global-whitespace-cleanup-mode nil))

  ;;; https://github.com/Malabarba/aggressive-indent-mode
  (use-package aggressive-indent
    :ensure t
    :config
    (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
    (global-aggressive-indent-mode 0)
    (add-to-list 'aggressive-indent-excluded-modes 'html-mode))

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

  (use-package rainbow-delimiters
    :ensure t
    :hook
    (prog-mode . rainbow-delimiters-mode))

  (use-package sudo-edit
    :ensure t)

  (use-package bug-hunter
    :ensure t)

  (use-package logview
    :ensure t
    :defer t))


(defun set-c-code-style()
  "C indentation and code style."
  (setq-default c-default-style "linux")
  (setq-default c-basic-offset 4
                tab-width 4
                indent-tabs-mode nil))


(defun set-xml-code-style()
  "XML indentation."
  (add-hook 'xml-mode-hook
            (lambda ()
              (setq-default
               c-basic-offset 4
               tab-width 4
               indent-tabs-mode nil))))


(defun set-java-code-style()
  "Java indentation and code style."
  (add-hook 'java-mode-hook
            (lambda ()
              (setq-default
               c-basic-offset 4
               tab-width 4
               indent-tabs-mode nil))))


(code-setup)
(set-c-code-style)
(set-xml-code-style)
(set-java-code-style)

(provide 'code-config)
;;; code-config.el ends here
