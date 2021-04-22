;;; package --- Summary
;;; Commentary:
;;; Custom Emacs mode line package
;;; Code:

(require 'doom-themes)

(use-package doom-modeline
  :ensure t
  :config
  (setq doom-modeline-buffer-file-name-style      'truncate-with-project
        doom-modeline-icon                        t
        doom-modeline-major-mode-icon             t
        doom-modeline-major-mode-color-icon       t
        doom-modeline-buffer-state-icon           t
        doom-modeline-buffer-modification-icon    t
        doom-modeline-modal-icon                  t
        doom-modeline-lsp                         t
        inhibit-compacting-font-caches            t
        doom-modeline-checker-simple-format       t
        doom-modeline-workspace-name              t
        doom-modeline-persp-name                  t
        doom-modeline-persp-icon                  t
        doom-modeline-minor-modes                 nil  ; desliga todos os minor mode na linha
        doom-modeline-enable-word-count           nil
        doom-modeline-buffer-encoding             nil) ;remove enconding
  ;;(doom-modeline-def-modeline 'main
  ;;  '(window-number matches buffer-info remote-host buffer-position parrot selection-info)
  ;;  '(misc-info persp-name lsp github debug minor-modes input-method major-mode process vcs checker))

  :custom-face
  '(mode-line ((t (:family "Source Code Pro" :height 1.0))))
  '(mode-line-inactive ((t (:family "Source Code Pro" :height 1.0))))

  :hook (after-init . doom-modeline-mode) (focus-in-hook . #'doom-modeline-set-selected-window))

(use-package nyan-mode
   :custom
   (nyan-cat-face-number 3)
   (nyan-animate-nyancat t)
   :hook
   (doom-modeline-mode . nyan-mode))

(provide 'line-mode-config)
;;; line-mode-config.el ends here
