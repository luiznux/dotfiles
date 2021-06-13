;;; line-mode.el  --- Configs for display mode line in emacs
;;;
;;; Commentary:
;;; Custom Emacs mode line package
;;;
;;; Code:

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  ;; Prevent flash of unstyled modeline at startup
  (unless after-init-time
    (setq doom-modeline--default-format mode-line-format)
    (setq-default mode-line-format nil))
  :config
  (setq doom-modeline-buffer-file-name-style      'truncate-with-project
        doom-modeline-icon                        t
        doom-modeline-major-mode-icon             t
        doom-modeline-buffer-state-icon           t
        doom-modeline-major-mode-color-icon       t
        doom-modeline-buffer-modification-icon    t
        doom-modeline-modal-icon                  t
        doom-modeline-lsp                         t
        doom-modeline-github                      t
        inhibit-compacting-font-caches            t
        doom-modeline-checker-simple-format       t
        doom-modeline-persp-name                  t
        doom-modeline-persp-icon                  t
        doom-modeline-bar-width                   2
        doom-modeline-minor-modes                 nil
        doom-modeline-enable-word-count           nil
        doom-modeline-buffer-encoding             nil)

  ;;  (add-hook 'focus-out-hook
  ;;            (lambda ()
  ;;              (copy-face 'mode-line '--mode-line-backup)
  ;;              (copy-face 'mode-line-inactive 'mode-line)))
  ;;
  ;;  (add-hook 'focus-in-hook
  ;;            (lambda ()
  ;;              (copy-face '--mode-line-backup 'mode-line)
  ;;              (doom-modeline-set-selected-window)))

  :custom-face
  '(mode-line-inactive nil)
  '(mode-line ((t (:family "Source Code Pro" :height 1))))
  '(mode-line-inactive ((t (:family "Source Code Pro" :height 1)))))

(use-package nyan-mode
  :custom
  (nyan-cat-face-number 1)
  (nyan-animate-nyancat t)
  :hook
  (doom-modeline-mode . nyan-mode))

(provide 'line-mode)
;;; line-mode.el ends here
