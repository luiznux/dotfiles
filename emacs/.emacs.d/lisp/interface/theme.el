;;; theme.el --- Loads theme
;;; Commentary:
;;; Emacs Packages configuration --- loads theme
;;;
;;; Code:

;;(use-package base16-theme
;;  :config
;;  (load-theme 'base16-tomorrow-night t))

;;(use-package badger-theme
;;  :config (load-theme 'badger t))

;;(use-package exotica-theme
;;  :config (load-theme 'exotica t))

;;(use-package inkpot-theme
;;  :config (load-theme 'inkpot t))

(use-package doom-themes)

(use-package solaire-mode
  :config
  (solaire-global-mode +1)
  (load-theme 'doom-one t))

;;; theme.el ends here
