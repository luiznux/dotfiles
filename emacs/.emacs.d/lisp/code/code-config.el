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

(use-package yasnippet
  :hook (after-init-hook . yas-global-mode)
  :config
  (yas-global-mode 1)
  (use-package yasnippet-snippets))

(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (add-to-list 'display-buffer-alist ;; custom flycheck buffer display(smaller and at the bottom)
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-below-selected)
                 (reusable-frames . visible)
                 (side            . bottom)
                 (window-height   . 0.2))))

(use-package flyspell
  :ensure nil
  :if (executable-find "aspell")
  :commands ispell-init-process
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
  (advice-add #'ispell-init-process :around #'message-off-advice))

(use-package flyspell-correct-ivy
  :bind ("C-M-:" . flyspell-correct-at-point)
  :config
  (when (eq system-type 'darwin)
    (progn
      (global-set-key (kbd "C-M-;") 'flyspell-correct-at-point)))
  (setq flyspell-correct-interface #'flyspell-correct-ivy))

(use-package flyspell-popup
  :init
  (define-key flyspell-mode-map (kbd "C-;") #'flyspell-popup-correct)
  (add-hook 'flyspell-mode-hook #'flyspell-popup-auto-correct-mode))

(use-package origami
  :init
  (global-origami-mode))

;; Autoclose brackets, quotes.
(use-package elec-pair
  :init
  (electric-pair-mode 1))

  ;;; https://github.com/purcell/whitespace-cleanup-mode
(use-package whitespace-cleanup-mode
  :init
  (setq  global-whitespace-cleanup-mode nil))

  ;;; https://github.com/Malabarba/aggressive-indent-mode
(use-package aggressive-indent
  :init
  (global-aggressive-indent-mode 0)
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))

(use-package anzu
  :bind
  ("C-r"   . anzu-query-replace-regexp)
  ("C-M-r" . anzu-query-replace-at-cursor-thing)
  :hook (after-init . global-anzu-mode)
  :init
  (setq anzu-replace-to-string-separator " => "
        anzu-deactivate-region           t
        anzu-mode-lighter                ""))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode))

;; Syntax highlighting of known Elisp symbols
;;(use-package highlight-defined
;;  :hook (emacs-lisp-mode . highlight-defined-mode)
;;  :init (setq highlight-defined-face-use-itself t))

(use-package quickrun
  :bind
  (("<f5>" . quickrun)
   ("M-<f5>" . quickrun-shell)
   ("C-c e" . quickrun)
   ("C-c C-e" . quickrun-shell)))

(use-package format-all)
(use-package sudo-edit)

(defun setup-project-packages()
  "Call project packages."

  (use-package ag)

  (use-package dumb-jump
    :commands xref-show-definitions-completing-read
    :init
    (setq  dumb-jump-prefer-searcher       'ag
           dumb-jump-force-searcher        'ag
           xref-show-definitions-function  #'xref-show-definitions-completing-read)
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)) ;; use M-. to go to definition

  (use-package ggtags
    :init
    (add-hook 'c-mode-common-hook
              (lambda ()
                (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                  (ggtags-mode 1)))))

  (use-package projectile
    :init
    (setq projectile-project-search-path '("~/projects/"))
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    (projectile-mode +1))

  (use-package treemacs
    :defer t
    :defines treemacs-resize-icons
    :commands treemacs-toggle-fixed-width treemacs--find-python3 treemacs-follow-mode treemacs-filewatch-mode treemacs-fringe-indicator-mode treemacs-git-mode
    :bind
    (:map global-map
          ("M-0"       . treemacs-select-window)
          ("C-x t 1"   . treemacs-delete-other-windows)
          ("C-x t t"   . treemacs)
          ("C-x t B"   . treemacs-bookmark)
          ("C-x t C-t" . treemacs-find-file)
          ("C-x t M-t" . treemacs-find-tag))
    :init
    (add-hook 'treemacs-mode-hook (lambda() (display-line-numbers-mode -1)))
    (add-hook 'treemacs-mode-hook (lambda() (treemacs-toggle-fixed-width)))

    (progn
      (setq treemacs-collapse-dirs                 (if (treemacs--find-python3) 3 0)
            treemacs-deferred-git-apply-delay      0.5
            treemacs-directory-name-transformer    #'identity
            treemacs-display-in-side-window        t
            treemacs-eldoc-display                 t
            treemacs-file-event-delay              5000
            treemacs-file-extension-regex          treemacs-last-period-regex-value
            treemacs-file-follow-delay             0.2
            treemacs-file-name-transformer         #'identity
            treemacs-follow-after-init             t
            treemacs-git-command-pipe              ""
            treemacs-goto-tag-strategy             'refetch-index
            treemacs-indentation                   2
            treemacs-indentation-string            " "
            treemacs-is-never-other-window         nil
            treemacs-max-git-entries               5000
            treemacs-missing-project-action        'ask
            treemacs-move-forward-on-expand        nil
            treemacs-no-png-images                 nil
            treemacs-no-delete-other-windows       t
            treemacs-project-follow-cleanup        nil
            treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
            treemacs-position                      'left
            treemacs-read-string-input             'from-child-frame
            treemacs-recenter-distance             0.1
            treemacs-recenter-after-file-follow    nil
            treemacs-recenter-after-tag-follow     nil
            treemacs-recenter-after-project-jump   'always
            treemacs-recenter-after-project-expand 'on-distance
            treemacs-show-cursor                   nil
            treemacs-show-hidden-files             t
            treemacs-silent-filewatch              nil
            treemacs-silent-refresh                nil
            treemacs-sorting                       'alphabetic-asc
            treemacs-space-between-root-nodes      t
            treemacs-tag-follow-cleanup            t
            treemacs-tag-follow-delay              1.5
            treemacs-user-mode-line-format         nil
            treemacs-user-header-line-format       nil
            treemacs-workspace-switch-cleanup      nil
            treemacs-width                         35
            treemacs-follow-mode                   t
            treemacs-filewatch-mode                t
            treemacs-resize-icons                  18
            treemacs-fringe-indicator-mode         t)

      (pcase (cons (not (null (executable-find "git")))
                   (not (null (treemacs--find-python3))))
        (`(t . t)
         (treemacs-git-mode 'deferred))
        (`(t . _)
         (treemacs-git-mode 'extended))))

    :config
    (use-package treemacs-evil
      :after treemacs evil)

    (use-package treemacs-projectile
      :after treemacs projectile)

    (use-package treemacs-icons-dired
      :after treemacs dired
      :init (treemacs-icons-dired-mode))

    (use-package treemacs-magit
      :after treemacs magit))

  (use-package minimap
    :custom
    (minimap-major-modes '(prog-mode))
    :custom-face
    '(minimap-font-face ((t (:height 32 :family "DejaVu Sans Mono"))))
    '(minimap-active-region-background ((t (:extend t :background "#232526"))))
    '(minimap-current-line-face ((t (:background "#344256"))))
    :init
    (setq minimap-window-location 'right
          minimap-update-delay 0.5
          minimap-highlight-line  t
          minimap-hide-scroll-bar nil
          minimap-display-semantic-overlays t)))

(defun setup-git-packages()
  "Call git packages."

  (use-package magit
    :defer t)

  (use-package diff-hl
    :init
    (global-diff-hl-mode)
    (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

  (use-package grip-mode
    :init
    (setq grip-preview-use-webkit t)))


(setup-git-packages)
(setup-project-packages)


(provide 'code-config)
;;; code-config.el ends here
