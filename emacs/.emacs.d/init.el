;;; init.el --- Initialization file for Emacs
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;;
;;;  ███████╗███╗   ███╗ █████╗  ██████╗███████╗
;;;  ██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝
;;;  █████╗  ██╔████╔██║███████║██║     ███████╗
;;;  ██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║
;;;  ███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║
;;;  ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝
;;;
;;;
;;; Code:

;; Speed up Emacs startup time increasing
;; the garbage collector number of bytes

;;(packag-initialize)
(defvar emacs-gc-cons-threshold (if (display-graphic-p) 64000000 1600000)
  "The default value to use for `gc-cons-threshold'. If you experience freezing,
decrease this. If you experience stuttering, increase this.")

(defvar emacs-gc-cons-upper-limit (if (display-graphic-p) 512000000 128000000)
  "The temporary value for `gc-cons-threshold' to defer it.")

(defvar emacs-gc-timer (run-with-idle-timer 10 t #'garbage-collect)
  "Run garbarge collection when idle 10s.")

(defvar emacs-file-name-handler-alist file-name-handler-alist)

(setq file-name-handler-alist nil)
(setq gc-cons-threshold emacs-gc-cons-upper-limit
      gc-cons-percentage 0.5)

(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after startup."
            (setq gc-cons-threshold 20000000)

            ;; GC automatically while unfocusing the frame
            ;; `focus-out-hook' is obsolete since 27.1
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'focus-out-hook 'garbage-collect))

            ;; Avoid GCs while using `ivy'/`counsel'/`swiper' and `helm', etc.
            ;; @see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
            (defun my-minibuffer-setup-hook ()
              (setq gc-cons-threshold emacs-gc-cons-upper-limit))

            (defun my-minibuffer-exit-hook ()
              (setq gc-cons-threshold emacs-gc-cons-threshold))

            (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)))

;; Load path
;; Optimize: Force "lisp"" and "elpa" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("lisp" "elpa"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory (expand-file-name "lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)
(update-load-path)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-deactivate-region t)
 '(anzu-mode-lighter "")
 '(anzu-replace-to-string-separator " => ")
 '(blink-cursor-mode nil)
 '(evil-undo-system 'undo-tree)
 '(fci-rule-color "#dedede")
 '(global-auto-revert-mode t)
 '(helm-icons-provider 'all-the-icons)
 '(helm-minibuffer-history-key "M-p")
 '(line-spacing 0.2)
 '(package-selected-packages
   '(ivy-xref ivy-historian historian flx ivy-yasnippet ivy-prescient all-the-icons-ivy-rich all-the-icons-ivy ivy-mode amx ivy-posframe counsel-projectile counsel pyvenv memory-usage company-jedi helm-icons counsel-tramp counsel-world-clock swiper-helm async company-prescient lsp-python-ms lsp-pyright lsp-ivy company parrot ibuffer-projectile all-the-icons-ibuffer helm-smex helm-flx helm-swoop helm-fuzzier ido-vertical-mode cider clojure-mode csv-mode org-tree-slide highlight-symbol yasnippet-snippets yaml-mode whitespace-cleanup-mode which-key use-package undo-tree treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired treemacs-evil switch-window sudo-edit solaire-mode smex ranger rainbow-mode rainbow-delimiters pdf-tools org-download org-bullets nyan-mode minimap math-preview magit-todos lsp-ui lsp-origami lsp-dart logview latex-preview-pane highlight-indent-guides hide-mode-line helm google-translate google-this go-mode gitignore-mode github-pullrequest ggtags gcmh format-all flyspell-popup flycheck evil-surround evil-org evil-matchit evil-leader evil-collection esup emojify dumb-jump doom-themes doom-modeline dockerfile-mode docker dired-git-info diff-hl dashboard company-quickhelp company-posframe company-box clojure-snippets ccls bug-hunter anzu aggressive-indent ag))
 '(standard-indent 4))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :foreground "#c5c8c6" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "ADBO" :family "Source Code Pro"))))
 '(centaur-tabs-selected ((t (:background "#282c34" :foreground "#bbc2cf" :overline nil :underline "#51afef" :weight semi-bold :height 99 :width normal :family "Source Code Pro"))))
 '(ivy-minibuffer-match-face-1 ((t (:inherit font-lock-doc-face :foreground nil))))
 '(org-agenda-date-today ((t (:foreground "lime green" :weight ultra-bold))))
 '(org-scheduled ((t (:foreground "SlateBlue2"))))
 '(org-scheduled-previously ((t (:foreground "medium turquoise"))))
 '(org-scheduled-today ((t (:foreground "deep sky blue"))))
 '(quote (mode-line-inactive nil)))

(require 'packages)
;;; init.el ends here
