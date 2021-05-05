;;; project-config.el --- Packages for better experience  with projecs
;;; Commentary:
;;; Some packages for Emacs to deal better with projecs and others.
;;;
;;;  ██████╗ ██████╗  ██████╗      ██╗███████╗ ██████╗████████╗
;;;  ██╔══██╗██╔══██╗██╔═══██╗     ██║██╔════╝██╔════╝╚══██╔══╝
;;;  ██████╔╝██████╔╝██║   ██║     ██║█████╗  ██║        ██║
;;;  ██╔═══╝ ██╔══██╗██║   ██║██   ██║██╔══╝  ██║        ██║
;;;  ██║     ██║  ██║╚██████╔╝╚█████╔╝███████╗╚██████╗   ██║
;;;  ╚═╝     ╚═╝  ╚═╝ ╚═════╝  ╚════╝ ╚══════╝ ╚═════╝   ╚═╝
;;;
;;; Code:

(defun setup-project-packages()
  "Call project packages."

  (use-package ag)

  (use-package dumb-jump
    :config
    (setq dumb-jump-force-searcher 'ag)
    (setq dumb-jump-prefer-searcher 'ag)
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)) ;; use M-. to go to definition

  (use-package ggtags
    :config
    (add-hook 'c-mode-common-hook
              (lambda ()
                (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                  (ggtags-mode 1)))))

  (use-package projectile
    :config
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    (setq projectile-project-search-path '("~/projects/"))
    (projectile-mode +1))

  (use-package treemacs
    :defer t
    :init
    (with-eval-after-load 'winum
      (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
    :config
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
            treemacs-sorting                       'alphabetic-desc
            treemacs-space-between-root-nodes      t
            treemacs-tag-follow-cleanup            t
            treemacs-tag-follow-delay              1.5
            treemacs-user-mode-line-format         nil
            treemacs-user-header-line-format       nil
            treemacs-workspace-switch-cleanup      nil
            treemacs-width                         35)

      ;; The default width and height of the icons is 22 pixels. If you are
      ;; using a Hi-DPI display, uncomment this to double the icon size.
      ;; (treemacs-resize-icons 44)
      ;; (treemacs-load-theme "alltheicons")

      (treemacs-follow-mode t)
      (treemacs-filewatch-mode t)
      (treemacs-resize-icons 18)
      (treemacs-fringe-indicator-mode t)
      (pcase (cons (not (null (executable-find "git")))
                   (not (null (treemacs--find-python3))))
        (`(t . t)
         (treemacs-git-mode 'deferred))
        (`(t . _)
         (treemacs-git-mode 'simple))))
    :bind
    (:map global-map
          ("M-0"       . treemacs-select-window)
          ("C-x t 1"   . treemacs-delete-other-windows)
          ("C-x t t"   . treemacs)
          ("C-x t B"   . treemacs-bookmark)
          ("C-x t C-t" . treemacs-find-file)
          ("C-x t M-t" . treemacs-find-tag)))

  (use-package treemacs-evil
    :after treemacs evil)

  (use-package treemacs-projectile
    :after treemacs projectile)

  (use-package treemacs-icons-dired
    :after treemacs dired
    :config (treemacs-icons-dired-mode))

  (use-package treemacs-magit
    :after treemacs magit)

  (use-package treemacs-persp
    :after treemacs persp-mode
    :config (treemacs-set-scope-type 'Perspectives))

  (use-package minimap
    :custom
    (minimap-major-modes '(prog-mode))
    :config
    (setq minimap-window-location 'right
          minimap-update-delay 0.5
          minimap-highlight-line  t
          minimap-hide-scroll-bar nil
          minimap-display-semantic-overlays t)
    :custom-face
    '(minimap-font-face ((t (:height 32 :family "DejaVu Sans Mono"))))
    '(minimap-active-region-background ((t (:extend t :background "#232526"))))
    '(minimap-current-line-face ((t (:background "#344256"))))))

;;(use-package paren
;;  :hook
;;  (after-init . show-paren-mode)
;;  :custom-face
;;  (show-paren-match ((nil (:background "#44475a" :foreground "#f1fa8c")))) ;; :box t
;;  :custom
;;  (show-paren-style 'mixed)
;;  (show-paren-when-point-inside-paren t)
;;  (show-paren-when-point-in-periphery t))

(setup-project-packages)

(provide 'project-config)
;;; project-config.el ends here
