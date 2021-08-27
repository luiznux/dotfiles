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

(defvar emacs-gc-cons-threshold (if (display-graphic-p) 64000000 1600000)
  "The default value to use for `gc-cons-threshold'.
If you experience freezing, decrease this.
If you experience stuttering, increase this.")

(defvar emacs-gc-cons-upper-limit (if (display-graphic-p) 512000000 128000000)
  "The temporary value for `gc-cons-threshold' to defer it.")

(defvar emacs-gc-timer (run-with-idle-timer 10 t #'garbage-collect)
  "Run garbarge collection when idle 10s.")

(defvar emacs-file-name-handler-alist file-name-handler-alist)

;; avoid anoing message
(setq byte-compile-warnings '(cl-functions))

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
            (add-hook 'minibuffer-setup-hook
                      (lambda ()
                        "Enlarge gc cons threshold while entering minibuffer."
                        (setq gc-cons-threshold emacs-gc-cons-upper-limit)))
            (add-hook 'minibuffer-exit-hook
                      (lambda ()
                        "Recover gc cons threshold while exiting minibuffer."
                        (setq gc-cons-threshold emacs-gc-cons-threshold)))))


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

(require 'packages)

;; Core packages
(require 'customizations)
(require 'interface)

(require 'evil-config)
(require 'company-config)
(require 'ivy-config)
(require 'lsp-config)

(require 'code-config)
(require 'other-modes)

(require 'terminal-config)
(require 'clojure-config)
(require 'python-config)
(require 'web-config)

;; Org and Agenda config
(require 'org-config)
(require 'file-color-agenda)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(alert-fade-time 10)
 '(blink-cursor-mode nil)
 '(evil-undo-system 'undo-tree)
 '(fci-rule-color "#dedede")
 '(global-auto-revert-mode t)
 '(line-spacing 0.2)
 '(standard-indent 4))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :foreground "#c5c8c6" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "ADBO" :family "Source Code Pro"))))
 '(all-the-icons-ivy-rich-dir-face ((t (:inherit default))))
 '(centaur-tabs-selected ((t (:background "#282c34" :foreground "#bbc2cf" :overline nil :underline "#51afef" :weight semi-bold :height 99 :width normal :family "Source Code Pro"))))
 '(ivy-minibuffer-match-face-1 ((t (:inherit font-lock-doc-face :foreground nil))))
 '(macrostep-expansion-highlight-face ((t (:inherit tooltip :extend t))))
 '(org-agenda-date-today ((t (:foreground "lime green" :weight ultra-bold))))
 '(org-scheduled ((t (:foreground "SlateBlue2"))))
 '(org-scheduled-previously ((t (:foreground "medium turquoise"))))
 '(org-scheduled-today ((t (:foreground "deep sky blue"))))
 '(org-super-agenda-header ((t (:inherit default :foreground "#a3f7ff" :weight bold))))
 '(quote (mode-line-inactive nil)))

(message (emacs-init-time))
;;; init.el ends here
