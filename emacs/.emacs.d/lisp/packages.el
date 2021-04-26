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
;;(setq package-enable-at-startup nil)
;;(setq packages-check-signature nil)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Code Packages
(load "~/.emacs.d/lisp/code/code-config.el")
(load "~/.emacs.d/lisp/code/project-config.el")
(load "~/.emacs.d/lisp/code/evil-config.el")
(load "~/.emacs.d/lisp/code/git-config.el")
(load "~/.emacs.d/lisp/code/lsp-config.el")

;; Interface
(load "~/.emacs.d/lisp/interface/interface.el")
(load "~/.emacs.d/lisp/interface/line-mode.el")
(load "~/.emacs.d/lisp/interface/theme.el")
(load "~/.emacs.d/lisp/interface/helm-config.el")
(load "~/.emacs.d/lisp/interface/company-config.el")

;; Others configs
(load "~/.emacs.d/lisp/extra/other-modes.el")
(load "~/.emacs.d/lisp/extra/customizations.el")

;; Org and Agenda config
(load "~/.emacs.d/lisp/org-and-agenda/org-config.el")
(load "~/.emacs.d/lisp/org-and-agenda/agenda-config.el")

;; Local Packages
;;(load "~/.emacs.d/lisp/local/local-packages.el")

;;; packages.el ends here
