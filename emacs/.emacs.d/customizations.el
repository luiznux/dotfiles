;;; customizations.el --- Configs and key maps for Emacs.
;;; Commentary:
;;; Configure Emacs --- Configs and key maps for Emacs.
;;;
;;; Code:

(defun visual-config-modes()
  "Visual modes. Removes tool and menu bar,
removes scroll bar and display line numbers."
  (setq inhibit-startup-message t)
  (setq-default show-trailing-whitespace t)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (show-paren-mode 1)
  (global-display-line-numbers-mode))

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

;; Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))


(setq user-full-name "Luiz Tagliaferro"
      user-mail-address "luiztagli@hotmail.com")


(visual-config-modes)
(set-default-indentation)
(enable-ido-mode)
(read-path-variable-from-zshrc)
(add-hook 'term-mode-hook (lambda ()
                            (setq show-trailing-whitespace nil)))

;; Autoclose brackets, quotes.
(electric-pair-mode 1)

;; Sets ibuffer as default.
(defalias 'list-buffers 'ibuffer)

(global-set-key (kbd "C-a") 'beginning-of-line++)

;;; customizations.el ends here
