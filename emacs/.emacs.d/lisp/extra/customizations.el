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

(defconst sys/gnu-linux
  (eq system-type 'gnu/linux)
  "Are we running on a Linux system?")


;;----------------- Defuns -------------------------

(defun witch-sys? ()
  "Defime hooks for some sys."
  (with-no-warnings
    (cond
     (sys/win32p
      ;; make PC keyboard's Win key or other to type Super or Hyper
      ;; (setq w32-pass-lwindow-to-system nil)
      (setq w32-lwindow-modifier 'super     ; Left Windows key
            w32-apps-modifier 'hyper)       ; Menu/App key
      (w32-register-hot-key [s-t])
      (encode-mode))

     ;; Compatible with Emacs Mac port
     (sys/mac-port-p
      ;; Keybonds
      (global-set-key [(hyper a)] 'mark-whole-buffer)
      (global-set-key [(hyper v)] 'yank)
      (global-set-key [(hyper c)] 'kill-ring-save)
      (global-set-key [(hyper s)] 'save-buffer)
      (global-set-key [(hyper l)] 'goto-line)
      (global-set-key [(hyper w)]
                      (lambda () (interactive) (delete-window)))
      (global-set-key [(hyper z)] 'undo)

      ;; mac switch meta key
      (defun mac-switch-meta nil
        "switch meta between Option and Command"
        (interactive)
        (if (eq mac-option-modifier nil)
            (progn
	          (setq mac-option-modifier 'meta)
	          (setq mac-command-modifier 'hyper)
	          )
          (progn
            (setq mac-option-modifier nil)
            (setq mac-command-modifier 'meta))))

      ;; alert style for macos
      (setq alert-default-style 'osx-notifier)

      (defun mac-toggle-max-window ()
        "This function toggles the frame-parameter fullscreen,
     so that I can maximise Emacs from within rather than relying
     on the external MacOS controls. "
        (interactive)
        (set-frame-parameter
         nil
         'fullscreen
         (if (frame-parameter nil 'fullscreen)
             nil
           'fullboth))))

     (sys/gnu-linux
      (setq alert-default-style 'libnotify)))))

(defun encode-mode()
  "Explicitly set the prefered coding systems to avoid annoying prompt from Emacs (especially on Microsoft Windows)."
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
  (modify-coding-system-alist 'process "*" 'utf-8))

(defun various-emacs-config()
  "Visual modes, remove tool and menu bar,remove scroll bar and display line numbers."
  (setq inhibit-startup-message           t
        delete-selection-mode             t
        menu-bar-mode                     nil
        tool-bar-mode                     nil
        scroll-bar-mode                   nil)
  (setq password-cache-expiry nil)

  ;; more smooth scrollig
  (setq mouse-wheel-progressive-speed   t
        mouse-wheel-scroll-amount       '(1 ((shift) . 1))
        mouse-wheel-follow-mouse        't
        scroll-step                     1)

  (setq user-full-name "Luiz Tagliaferro"
        user-mail-address "luiz@luiznux.com")

  (setq password-cache-expiry nil)
  (setq load-prefer-newer t)
  (setq auto-window-vscroll nil)

  (global-hl-line-mode)
  (show-paren-mode 1)
  (global-display-line-numbers-mode)
  (global-set-key [mouse-3] 'mouse-popup-menubar-stuff))

(defun reload-init-file ()
  "Reload Emacs configurations."
  (interactive)
  (load user-init-file))

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
  "Read the path variable from zshrc."
  (let ((path (shell-command-to-string ". ~/.zshrc; echo -n $PATH")))
    (setenv "PATH" path)
    (setq exec-path
          (append
           (split-string-and-unquote path ":")
           exec-path))))

(defun put-current-path-to-clipboard ()
  "Put the current path to clipboard."
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
  "Put the current filename to clipboard."
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
  "Put the current filename with line to clipboard."
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
;;(defun c-lineup-arglist-tabs-only (ignored)
;;  "Line up argument lists by tabs, not spaces.
;;IGNORED"
;;  (let* ((anchor (c-langelem-pos c-syntactic-element))
;;         (column (c-langelem-2nd-pos c-syntactic-element))
;;         (offset (- (1+ column) anchor))
;;         (steps (floor offset c-basic-offset)))
;;    (* (max steps 1)
;;       c-basic-offset)))
;;
;;(defun call-clineup-arg ()
;;  "Call the c-lineup."
;;  (add-hook 'c-mode-common-hook
;;            (lambda ()
;;              ;; Add kernel style
;;              (c-add-style
;;               "linux-tabs-only"
;;               '("linux" (c-offsets-alist
;;                          (arglist-cont-nonempty
;;                           c-lineup-gcc-asm-reg
;;                           c-lineup-arglist-tabs-only)))))))

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
        window))))
(defun sasa/call-help-temp-buffers ()
  "Call the other `sasa/display-buffer' func with args."

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
                   (window-height       . 0.2)))
    (setq sasa/help-temp-buffers (cdr sasa/help-temp-buffers))))

;; Remove useless whitespace before saving a file
(defun delete-trailing-whitespace-except-current-line ()
  "An alternative to `delete-trailing-whitespace'.
The original function deletes trailing whitespace of the current line."
  (interactive)
  (let ((begin (line-beginning-position))
        (end (line-end-position)))
    (save-excursion
      (when (< (point-min) (1- begin))
        (save-restriction
          (narrow-to-region (point-min) (1- begin))
          (delete-trailing-whitespace)
          (widen)))
      (when (> (point-max) (+ end 2))
        (save-restriction
          (narrow-to-region (+ end 2) (point-max))
          (delete-trailing-whitespace)
          (widen))))))
(defun smart-delete-trailing-whitespace ()
  "Invoke `delete-trailing-whitespace-except-current-line' on selected major modes only."
  (unless (member major-mode '(diff-mode))
    (delete-trailing-whitespace-except-current-line)))
(add-hook 'before-save-hook #'smart-delete-trailing-whitespace)

;;(defun gambeta-pandoc ()
;;  "Uma gambeta muito boa pra converter o arquivo org  atual em pdf."
;;  (interactive)
;;  (setq  my-buffer (current-buffer))
;;
;;
;;  )

(defun do-not-show-trailing-whitespace ()
  "Not show some whitespaces in some modes."
  (dolist (hook '(special-mode-hook
                  term-mode-hook
                  comint-mode-hook
                  vterm-mode-hook
                  compilation-mode-hook
                  minibuffer-inactive-mode-hook
                  minibuffer-setup-hook))

    (add-hook hook (lambda () (setq show-trailing-whitespace nil)))))

(defun simplify-prompts ()
  "Simplify Yes/No Prompts."
  (fset 'yes-or-no-p 'y-or-n-p)
  (setq use-dialog-box nil))

(witch-sys?)
(various-emacs-config)
(set-default-indentation)
(enable-ido-mode)
(read-path-variable-from-zshrc)
;;(call-clineup-arg)
(sasa/call-help-temp-buffers)
(do-not-show-trailing-whitespace)
(simplify-prompts)

;; Sets ibuffer as default.
(defalias 'list-buffers 'ibuffer)

;; Set-keys for `reload-init-file' and `beginning-of-line++'
(global-set-key (kbd "C-c C-l") #'reload-init-file)
(global-set-key (kbd "C-a") 'beginning-of-line++)


(provide 'customizations)
;;; customizations.el ends here
