;;; theme.el --- Loads theme
;;; Commentary:
;;; Emacs Packages configuration --- loads theme
;;;
;;; Code:

;;;(use-package base16-theme
;;;  :ensure t
;;;  :config
;;;  (load-theme 'base16-tomorrow-night t))

;;;(use-package badger-theme
;;;  :ensure t
;;;  :config (load-theme 'badger t))

;;;(use-package exotica-theme
;;;  :ensure t
;;;  :config (load-theme 'exotica t))

;;;(use-package inkpot-theme
;;;  :ensure t
;;;  :config (load-theme 'inkpot t))

(use-package doom-themes
  :ensure t)

(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode +1)
  (load-theme 'doom-one t))

;;; theme.el ends here
