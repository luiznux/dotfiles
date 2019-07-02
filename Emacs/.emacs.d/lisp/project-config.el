;;; project-config.el --- Package configuration for evil packages
;;; Commentary:
;;; Emacs Evil configuration --- Package configuration for Emacs evil
;;; Code:

(defun setup-project-packages()
  (use-package ag
    :ensure t)
  
  (use-package dumb-jump
    :bind (("M-g o" . dumb-jump-go-other-window)
           ("M-g p" . dumb-jump-back)
           ("M-g j" . dumb-jump-go)
           ("M-g i" . dumb-jump-go-prompt)
           ("M-g x" . dumb-jump-go-prefer-external)
           ("M-g z" . dumb-jump-go-prefer-external-other-window))
    :init (dumb-jump-mode)
    :config
    (setq dumb-jump-force-searcher 'ag)
    (setq dumb-jump-prefer-searcher 'ag)
    :ensure t)
  
  (use-package ggtags
    :config
    (add-hook 'c-mode-common-hook
              (lambda ()
                (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                  (ggtags-mode 1))))
    :ensure t)
  
  (use-package projectile
    :ensure t
    :config
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    (setq projectile-project-search-path '("~/projects/"))
    (projectile-mode +1))
  
  (use-package neotree
    :ensure t
    :config
    (global-set-key [f8] 'neotree-toggle)
    (setq neo-theme
          (if (display-graphic-p) 'icons 'arrow)))
  
  (use-package all-the-icons
    :ensure t
    :defer t))


(provide 'project-config)

;;; project-config.el ends here
