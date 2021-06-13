;;; customizations.el --- Configs and key maps for Emacs.
;;; Commentary:
;;; Configure Emacs --- Configs and key maps for Emacs.
;;;
;;; Code:

;;----------------- Consts -------------------------

(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Are we running under X on a Mac system?")

(defconst sys/mac-ns-p
  (eq window-system 'ns)
  "Are we running on a GNUstep or Macintosh Cocoa display?")

(defconst sys/mac-cocoa-p
  (featurep 'cocoa)
  "Are we running with Cocoa on a Mac system?")

(defconst sys/mac-port-p
  (eq window-system 'mac)
  "Are we running a macport build on a Mac system?")

(defconst sys/cygwinp
  (eq system-type 'cygwin)
  "Are we running on a Cygwin system?")

(with-no-warnings
  (cond

   (sys/win32p
    ;; make PC keyboard's Win key or other to type Super or Hyper
    ;; (setq w32-pass-lwindow-to-system nil)
    (setq w32-lwindow-modifier 'super     ; Left Windows key
          w32-apps-modifier 'hyper)       ; Menu/App key
    (w32-register-hot-key [s-t]))

   (sys/mac-port-p
    ;; Compatible with Emacs Mac port
    (setq mac-option-modifier 'meta
          mac-command-modifier 'super)
    (bind-keys ([(super a)] . mark-whole-buffer)
               ([(super c)] . kill-ring-save)
               ([(super l)] . goto-line)
               ([(super q)] . save-buffers-kill-emacs)
               ([(super s)] . save-buffer)
               ([(super v)] . yank)
               ([(super w)] . delete-frame)
               ([(super z)] . undo)))))


;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)


(defun visual-config-modes()
  "Visual modes, remove tool and menu bar,remove scroll bar and display line numbers."
  (setq inhibit-startup-message t)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (show-paren-mode 1)
  (global-display-line-numbers-mode)
  (global-set-key [mouse-3] 'mouse-popup-menubar-stuff)
  ;;(setq-default show-trailing-whitespace t)
  ;; more smooth scrollig
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
        mouse-wheel-progressive-speed t
        mouse-wheel-follow-mouse 't
        scroll-step 1))

(defun reload-init-file ()
  "Reload Emacs configurations."
  (interactive)
  (load user-init-file))
(global-set-key (kbd "C-c C-l") #'reload-init-file)

(defun rename-this-file (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))


(defun set-default-indentation()
  "Configures the default indentation (4 spaces)."
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq indent-line-function 'insert-tab)
  (global-set-key (kbd "RET") 'newline-and-indent))

(defun enable-ido-mode()
  "Enables 'ido-mode'."
  (setq-default ido-enable-flex-matching t)
  (setq-default ido-everyehere t)
  (ido-mode 1))

(defun beginning-of-line++()
  "Go to first character on a line."
  (interactive)
  (if (bolp)
	  (back-to-indentation)
	(beginning-of-line)))

(defun read-path-variable-from-zshrc()
  (let ((path (shell-command-to-string ". ~/.zshrc; echo -n $PATH")))
    (setenv "PATH" path)
    (setq exec-path
          (append
           (split-string-and-unquote path ":")
           exec-path))))

(defun put-current-path-to-clipboard ()
  (interactive)
  (let ((file-path buffer-file-name)
        (dir-path default-directory))
    (cond (file-path
           (kill-new (expand-file-name file-path))
           (message "This file path is on the clipboard!"))
          (dir-path
           (kill-new (expand-file-name dir-path))
           (message "This directory path is on the clipboard!"))
          (t
           (error-message-string "Fail to get path name.")))))

(defun put-current-filename-to-clipboard ()
  (interactive)
  (let ((file-path buffer-file-name)
        (dir-path default-directory))
    (cond (file-path
           (kill-new (file-name-nondirectory file-path))
           (message "This file path is on the clipboard!"))
          (dir-path
           (kill-new (file-name-nondirectory dir-path))
           (message "This directory path is on the clipboard!"))
          (t
           (error-message-string "Fail to get path name.")))))

(defun put-current-filename-with-line-to-clipboard ()
  (interactive)
  (let ((file-path buffer-file-name)
        (dir-path default-directory))
    (cond (file-path
           (kill-new (format "%s:%s"
                             (file-name-nondirectory file-path)
                             (count-lines (point-min) (point))))
           (message "This file path is on the clipboard!"))
          (dir-path
           (kill-new (file-name-nondirectory dir-path))
           (message "This directory path is on the clipboard!"))
          (t
           (error-message-string "Fail to get path name.")))))

;; Linux coding style for Emacs.
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces.
IGNORED"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Add kernel style
            (c-add-style
             "linux-tabs-only"
             '("linux" (c-offsets-alist
                        (arglist-cont-nonempty
                         c-lineup-gcc-asm-reg
                         c-lineup-arglist-tabs-only))))))

(add-hook 'c-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name)))
              ;; Enable kernel mode for the appropriate files
              (when (and filename
                         (string-match (expand-file-name "~/Projects/linux")
                                       filename))
                (setq indent-tabs-mode t)
                (setq show-trailing-whitespace t)
                (c-set-style "linux-tabs-only")))))

;; Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

;; Source https://www.simplify.ba/articles/2016/01/25/display-buffer-alist/
(defun sasa/display-buffer (buffer &optional alist)
  "Select window for BUFFER (need to use word ALIST on the first line).
Returns thirth visible window if there are three visible windows, nil otherwise.
Minibuffer is ignored."
  (let ((wnr (if (active-minibuffer-window) 3 2)))
    (when (= (+ wnr 1) (length (window-list)))
      (let ((window (nth wnr (window-list))))
        (set-window-buffer window buffer)
        window)))
  )

(defvar sasa/help-temp-buffers '("^\\*Flycheck errors\\*$"
                                 "^\\*Completions\\*$"
                                 "^\\*Help\\*$"
                                 ;; Other buffers names...
                                 "^\\*Ido Completions\\*$"
                                 "^\\*Colors\\*$"
                                 "^\\*Async Shell Command\\*$"))

(while sasa/help-temp-buffers
  (add-to-list 'display-buffer-alist
               `(,(car sasa/help-temp-buffers)
                 (display-buffer-reuse-window
                  sasa/display-buffer
                  display-buffer-in-side-window)
                 (reusable-frames     . visible)
                 (side                . bottom)
                 (window-height       . 0.2)
                 ))
  (setq sasa/help-temp-buffers (cdr sasa/help-temp-buffers)))

(setq user-full-name "Luiz Tagliaferro"
      user-mail-address "luiztagli@hotmail.com")

(dolist (hook '(special-mode-hook
                term-mode-hook
                comint-mode-hook
                vterm-mode-hook
                compilation-mode-hook
                minibuffer-inactive-mode-hook
                minibuffer-setup-hook))
  (add-hook hook
            (lambda () (setq show-trailing-whitespace nil))))


(visual-config-modes)
(set-default-indentation)
(enable-ido-mode)
(read-path-variable-from-zshrc)

;; Autoclose brackets, quotes.
(electric-pair-mode 1)

;; Sets ibuffer as default.
(defalias 'list-buffers 'ibuffer)

(global-set-key (kbd "C-a") 'beginning-of-line++)


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
   '(smooth-scroll smooth-scrolling live-py-mode js2-refactor helpful macrostep highlight-defined overseer highlight org-alert org-fragtog org-superstar web-mode typescript-mode skewer-mode js2-mode ivy-xref ivy-historian historian flx ivy-yasnippet ivy-prescient all-the-icons-ivy-rich all-the-icons-ivy ivy-mode amx ivy-posframe counsel-projectile counsel pyvenv memory-usage company-jedi helm-icons counsel-tramp counsel-world-clock swiper-helm async company-prescient lsp-python-ms lsp-pyright lsp-ivy company parrot ibuffer-projectile all-the-icons-ibuffer helm-smex helm-flx helm-swoop helm-fuzzier ido-vertical-mode cider clojure-mode csv-mode org-tree-slide highlight-symbol yasnippet-snippets yaml-mode whitespace-cleanup-mode which-key use-package undo-tree treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired treemacs-evil switch-window sudo-edit solaire-mode smex ranger rainbow-mode rainbow-delimiters pdf-tools org-download org-bullets nyan-mode minimap math-preview magit-todos lsp-ui lsp-origami lsp-dart logview latex-preview-pane highlight-indent-guides hide-mode-line helm google-translate google-this go-mode gitignore-mode github-pullrequest ggtags gcmh format-all flyspell-popup flycheck evil-surround evil-org evil-matchit evil-leader evil-collection esup emojify dumb-jump doom-themes doom-modeline dockerfile-mode docker dired-git-info diff-hl dashboard company-quickhelp company-posframe company-box clojure-snippets ccls bug-hunter anzu aggressive-indent ag))
 '(standard-indent 4))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :foreground "#c5c8c6" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "ADBO" :family "Source Code Pro"))))
 '(centaur-tabs-selected ((t (:background "#282c34" :foreground "#bbc2cf" :overline nil :underline "#51afef" :weight semi-bold :height 99 :width normal :family "Source Code Pro"))))
 '(ivy-minibuffer-match-face-1 ((t (:inherit font-lock-doc-face :foreground nil))))
 '(macrostep-expansion-highlight-face ((t (:inherit tooltip :extend t))))
 '(org-agenda-date-today ((t (:foreground "lime green" :weight ultra-bold))))
 '(org-scheduled ((t (:foreground "SlateBlue2"))))
 '(org-scheduled-previously ((t (:foreground "medium turquoise"))))
 '(org-scheduled-today ((t (:foreground "deep sky blue"))))
 '(quote (mode-line-inactive nil)))


(provide 'customizations)
;;; customizations.el ends here
