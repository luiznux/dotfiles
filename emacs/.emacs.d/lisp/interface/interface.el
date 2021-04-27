;;; interface.el --- visual beauties for Emacs
;;; Commentary:
;;; Visual beauties for you code more happier :)
;;;
;;;       ⣿⣷⡶⠚⠉⢀⣤⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡿⠋⠠⣴⣿⣿⣿⣿⣶⣤⣤⣤
;;;       ⠿⠥⢶⡏⣸⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡿⠋⢀⣴⣷⣌⢿⣿⣿⣿⣿⣿⣿⣿
;;;       ⣍⡛⢷⣠⣿⣿⣿⣿⣿⣟⠻⣯⠽⣿⣿⠟⠁⣠⠿⠿⣿⣿⣎⠻⣿⣿⣿⡿⠟⣿
;;;       ⣿⣿⣦⠙⣿⣿⣿⣿⣿⣿⣷⣏⡧⠙⠁⣀⢾⣧    ⠈⣿⡟  ⠙⣫⣵⣶⠇⣋
;;;       ⣿⣿⣿⢀⣿⣿⣿⣿⣿⣿⣿⠟⠃⢀⣀⢻⣎⢻⣷⣤⣴⠟  ⣠⣾⣿⢟⣵⡆⢿
;;;       ⣿⣯⣄⢘⢻⣿⣿⣿⣿⡟⠁⢀⣤⡙⢿⣴⣿⣷⡉⠉⢀  ⣴⣿⡿⣡⣿⣿⡿⢆
;;;       ⠿⣿⣧⣤⡘⢿⣿⣿⠏  ⡔⠉⠉⢻⣦⠻⣿⣿⣶⣾⡟⣼⣿⣿⣱⣿⡿⢫⣾⣿
;;;       ⣷⣮⣝⣛⣃⡉⣿⡏  ⣾⣧⡀    ⣿⡇⢘⣿⠋    ⠻⣿⣿⣿⢟⣵⣿⣿⣿
;;;       ⣿⣿⣿⣿⣿⣿⣌⢧⣴⣘⢿⣿⣶⣾⡿⠁⢠⠿⠁⠜    ⣿⣿⣿⣿⡿⣿⣿⣿
;;;       ⣿⣿⣿⣿⣿⣿⣿⣦⡙⣿⣷⣉⡛⠋    ⣰⣾⣦⣤⣤⣤⣿⢿⠟⢋⣴⣿⣿⣿
;;;       ⣿⣿⣿⣿⣿⣿⣿⣿⣿⣌⢿⣿⣿⣿⣿⢰⡿⣻⣿⣿⣿⣿⣿⢃⣰⣫⣾⣿⣿⣿
;;;       ⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡆⠿⠿⠿⠛⢰⣾⡿⢟⣭⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
;;;
;;; Code:

(defun interface-setup()
  "Call all interface packages."

  (require 'iso-transl)
  (use-package highlight-indent-guides
    :ensure t
    :config
    (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
    (setq highlight-indent-guides-method 'character)
    (setq highlight-indent-guides-responsive 'top))

  (use-package highlight-symbol
    :bind
    (:map prog-mode-map
          ("M-o h" . highlight-symbol)
          ("M-p" . highlight-symbol-prev)
          ("M-n" . highlight-symbol-next)))

  (use-package which-key
    :ensure t
    :config
    (which-key-mode))

  (use-package smex
    :ensure t
    :config
    (global-set-key (kbd "M-x") 'smex))

  (use-package counsel-css
    :ensure t)

  ;; Add icons for emacs
  (use-package all-the-icons
    :ensure t)

  (use-package ibuffer
    :ensure nil
    :bind ("C-x C-b" . ibuffer)
    :init (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))
    :config
    ;; Display icons for buffers
    (use-package all-the-icons-ibuffer
      :ensure t
      :config
      (all-the-icons-ibuffer-mode 1)
      (with-eval-after-load 'counsel
        (with-no-warnings
          (defun my-ibuffer-find-file ()
            (interactive)
            (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                                       (if (buffer-live-p buf)
                                           (with-current-buffer buf
                                             default-directory)
                                         default-directory))))
              (counsel-find-file default-directory)))
          (advice-add #'ibuffer-find-file :override #'my-ibuffer-find-file)))))

  ;; Group ibuffer's list by project root
  (use-package ibuffer-projectile
    :ensure t
    :functions all-the-icons-octicon ibuffer-do-sort-by-alphabetic
    :hook ((ibuffer . (lambda ()
                        (ibuffer-projectile-set-filter-groups)
                        (unless (eq ibuffer-sorting-mode 'alphabetic)
                          (ibuffer-do-sort-by-alphabetic))))))

  (use-package emojify
    :ensure t
    :config
    (setq emojify-company-tooltips-p t
          emojify-composed-text-p    nil)
    :hook (after-init . global-emojify-mode))

  (use-package rainbow-mode
    :ensure t
    :hook (emacs-lisp-mode . rainbow-mode))

  (use-package dashboard
    :ensure t
    :init
    (progn
      (setq recentf-exclude '("/org/*")) ;prevent  show recent org-agenda files
      (setq dashboard-items '((recents   . 8)
                              (projects  .  7))))
    :config
    (dashboard-setup-startup-hook)

    (setq dashboard-set-heading-icons  t
          dashboard-set-file-icons     t
          dashboard-set-navigator      t
          dashboard-startup-banner     'logo)

    (setq dashboard-navigator-buttons
          `(;;line1
            ((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
              "Homepage"
              "Browse homepage"
              (lambda (&rest _) (browse-url "https://github.com/luiznux")))
             (" " "Refresh" "Refresh" (lambda (&rest _) (dashboard-refresh-buffer)) nil))))

    (add-hook 'dashboard-mode-hook (lambda () (org-agenda t "x")) (lambda () (ace-window)))
    (add-hook 'dashboard-mode-hook (lambda () (goto-char (point-min)))))

  (use-package centaur-tabs
    :ensure t
    :config
    (setq centaur-tabs-style                    "chamfer"
          centaur-tabs-height                   32
          centaur-tabs-set-icons                t
          centaur-tabs-set-bar                  'under
          x-underline-at-descent-line           t
          centaur-tabs-set-modified-marker      t
          centaur-tabs-show-navigation-buttons  t)

    (centaur-tabs-headline-match)
    (centaur-tabs-group-by-projectile-project)
    (centaur-tabs-mode t)
    (centaur-tabs-change-fonts "Source Code Pro" 102)
    (defun centaur-tabs-buffer-groups ()
      "`centaur-tabs-buffer-groups' control buffers' group rules.
 Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
 All buffer name start with * will group to \"Emacs\".
 Other buffer group by `centaur-tabs-get-group-name' with project name."
      (list
       (cond
	    ((or (string-equal "*" (substring (buffer-name) 0 1))
	         (memq major-mode '(magit-process-mode
				                magit-status-mode
				                magit-diff-mode
				                magit-log-mode
				                magit-file-mode
				                magit-blob-mode
				                magit-blame-mode
				                )))
	     "Emacs")
	    ((derived-mode-p 'prog-mode)
	     "Editing")
        ((derived-mode-p 'emacs-lisp-mode)
         "Elisp")
	    ((derived-mode-p 'dired-mode)
	     "Dired")
	    ((memq major-mode '(helpful-mode
			                help-mode))
	     "Help")
	    ((memq major-mode '(org-mode
			                org-agenda-clockreport-mode
			                org-src-mode
			                org-agenda-mode
			                org-beamer-mode
			                org-indent-mode
			                org-bullets-mode
			                org-cdlatex-mode
			                org-agenda-log-mode
			                diary-mode))
	     "OrgMode")
	    (t
	     (centaur-tabs-get-group-name (current-buffer))))))
    :hook
    (dashboard-mode . centaur-tabs-local-mode)
    (term-mode . centaur-tabs-local-mode)
    (calendar-mode . centaur-tabs-local-mode)
    (org-agenda-mode . centaur-tabs-local-mode)
    (helpful-mode . centaur-tabs-local-mode)
    :bind
    ("C-<prior>" . centaur-tabs-backward)
    ("C-<next>" . centaur-tabs-forward)
    ("C-c t" . centaur-tabs-counsel-switch-group)
    (:map evil-normal-state-map
	      ("g t" . centaur-tabs-forward)
	      ("g T" . centaur-tabs-backward)))

  (use-package pdf-view
    :ensure pdf-tools
    :diminish (pdf-view-midnight-minor-mode pdf-view-printer-minor-mode)
    :defines pdf-annot-activate-created-annotations
    :functions (my-pdf-view-set-midnight-colors my-pdf-view-set-dark-theme)
    :hook (pdf-view-mode . pdf-view-midnight-minor-mode)
    :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
    :magic ("%PDF" . pdf-view-mode)
    :bind (:map pdf-view-mode-map
                ("C-s" . isearch-forward))
    :init
    (setq pdf-annot-activate-created-annotations t)

    ;; Set dark theme
    (defun my-pdf-view-set-midnight-colors ()
      "Set pdf-view midnight colors."
      (setq pdf-view-midnight-colors
            `(,(face-foreground 'default) . ,(face-background 'default))))
    (my-pdf-view-set-midnight-colors)

    (defun my-pdf-view-set-dark-theme ()
      "Set pdf-view midnight theme as color theme."
      (my-pdf-view-set-midnight-colors)
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (eq major-mode 'pdf-view-mode)
            (pdf-view-midnight-minor-mode (if pdf-view-midnight-minor-mode 1 -1))))))
    (add-hook 'after-load-theme-hook #'my-pdf-view-set-dark-theme)
    :config
    ;; Build pdfinfo if needed, locking until it's complete
    (with-no-warnings
      (defun my-pdf-tools-install ()
        (unless (file-executable-p pdf-info-epdfinfo-program)
          (let ((wconf (current-window-configuration)))
            (pdf-tools-install t)
            (message "Building epdfinfo. Please wait for a moment...")
            (while compilation-in-progress
              ;; Block until `pdf-tools-install' is done
              (sleep-for 1))
            (when (file-executable-p pdf-info-epdfinfo-program)
              (set-window-configuration wconf)))))
      (advice-add #'pdf-view-decrypt-document :before #'my-pdf-tools-install)))

  (use-package latex-preview-pane
    :ensure t)

  (use-package math-preview
    :ensure t)

  (use-package parrot
    :ensure t
    :config
    (parrot-mode)
    (parrot-set-parrot-type 'emacs)
    (setq parrot-num-rotations 6)
    (add-hook 'evil-insert-state-entry-hook #'parrot-start-animation)
    (add-hook 'evil-visual-state-entry-hook #'parrot-start-animation)
    (add-hook 'evil-emacs-state-entry-hook  #'parrot-start-animation))

  (use-package ranger
    :ensure t
    :config
    (ranger-override-dired-mode t))

  (use-package hide-mode-line
    :ensure t
    :hook (((completion-list-mode
             completion-in-region-mode
             pdf-annot-list-mode
             flycheck-error-list-mode
             ido-mode
             lsp-treemacs-error-list-mode) . hide-mode-line-mode)))

  (use-package switch-window
    :ensure t)

  (use-package page-break-lines
    :ensure t)

  (use-package google-translate
    :ensure t
    :bind
    ("M-o t" . google-translate-at-point)
    ("M-o T" . google-translate-at-point-reverse)
    :custom
    (google-translate-default-source-language "en")
    (google-translate-default-target-language "ja"))

  (use-package google-this
    :ensure t))


(interface-setup)

(provide 'interface)
;;; interface.el ends here
