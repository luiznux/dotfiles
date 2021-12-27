;;; ivy-config.el --- Loads all `ivy' configs and packages
;;; Commentary:
;;; Ivy config.
;;;
;;;   ██╗██╗   ██╗██╗   ██╗
;;;   ██║██║   ██║╚██╗ ██╔╝
;;;   ██║██║   ██║ ╚████╔╝
;;;   ██║╚██╗ ██╔╝  ╚██╔╝
;;;   ██║ ╚████╔╝    ██║
;;;   ╚═╝  ╚═══╝     ╚═╝
;;;
;;; More info : https://github.com/abo-abo/swiper
;;;
;;; Code:

(use-package counsel
  :defines wiper-action-recenter
  :commands (ivy-immediate-done
             ivy-alt-done
             ivy-set-occur
             ivy-next-line
             ivy-previous-line
             )
  :preface
  (defun config-ivy-with-empty-ivy-extra-directories (f &rest args)
    (let ((ivy-extra-directories nil))
      (apply f args)))

  :bind (("M-x"     . 'counsel-M-x)
         ("C-x C-f" . #'counsel-find-file)
         ("C-x b" . ivy-switch-buffer)
         ("C-c U" . #'counsel-unicode-char)
         ("C-c i" . #'counsel-imenu)
         ("C-x f" . #'counsel-find-file)
         ("C-c y" . #'counsel-yank-pop)
         ("C-c r" . #'counsel-recentf)
         ("C-c v" . #'counsel-switch-buffer-other-window)
         ("C-h h" . #'counsel-command-history)
         ("C-c C-r" . #'ivy-resume)
         :map ivy-minibuffer-map
         ("M-j" . 'ivy-next-line)
         ("M-k" . 'ivy-previous-line))

  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))

  :config
  ;; Use C-j for immediate termination with the current value, and RET
  ;; for continuing completion for that directory. This is the ido
  ;; behaviour.
  (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)

  (setq ivy-height                     12
        ivy-use-virtual-buffers        t    ; Enable bookmarks and recentf
        ivy-fixed-height-minibuffer    t
        ivy-count-format               "(%d/%d) "
        ivy-on-del-error-function      #'ignore
        ivy-initial-inputs-alist       nil
        ivy-dynamic-exhibit-delay-ms   200
        enable-recursive-minibuffers   t
        wiper-action-recenter          t
        tab-always-indent              'complete
        ivy-flx-limit                  2000
        ivy-sort-matches-functions-alist '((ivy-completion-in-region . ivy--shorter-matches-first)))

  ;; ignore files
  (setq counsel-find-file-ignore-regexp
        "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)")

  (add-hook 'minibuffer-setup-hook
            (lambda () (setq-local show-trailing-whitespace nil)))
  (ivy-set-occur 'ivy-switch-buffer 'ivy-switch-buffer-occur)


  ;; Do not show extra directories when finding files.
  (setq ivy-extra-directories '("."))
  (advice-add #'counsel-find-file :around #'config-ivy-with-empty-ivy-extra-directories))

;; flx is used as the fuzzy-matching indexer backend for ivy.
(use-package flx
  :after ivy)

;; Enhance M-x
(use-package amx
  :init (setq amx-history-length 20))

(use-package ivy-xref
  :init
  (when (boundp 'xref-show-definitions-function)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; Better experience with icons
;; Enable it before`ivy-rich-mode' for better performance
(use-package all-the-icons-ivy-rich
  :hook (ivy-mode . all-the-icons-ivy-rich-mode))


(use-package all-the-icons-ivy
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup)
  :config
  (setq all-the-icons-ivy-file-commands
        '(counsel-find-file counsel-file-jump counsel-recentf counsel-projectile-find-file counsel-projectile-find-dir)))


(use-package ivy-rich
  :commands ivy-format-function-line
  :custom
  (sivy-rich-path-style 'abbrev)
  (ivy-rich-switch-buffer-align-virtual-buffer nil)
  (ivy-rich-project-root-cache-mode t)
  :init
  (setq ivy-rich-parse-remote-buffer nil)
  :config
  (defun ivy-rich-switch-buffer-icon (candidate)
    (with-current-buffer
        (get-buffer candidate)
      (let ((icon (all-the-icons-icon-for-mode major-mode)))
        (if (symbolp icon)
            (all-the-icons-icon-for-mode 'fundamental-mode)
          icon))))

  (setq ivy-rich-display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-switch-buffer-icon (:width 2))
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand)))))

  (defcustom ivy-filthy-rich-padding ?\s
    "The padding of `ivy-filthy-rich-delimiter'.
It is used when there are extra space.
The length of the pad has to be one.
If not, `ivy-filth-rich' will fallback to using space to pad.
Currently only support character, because `make-string' only accept that."
    :type 'character
    :group 'ivy-filthy-rich)

  (defcustom ivy-filthy-rich-pad-side 'right
    "The side which padding is pad to.
Either left or right.
Left means align right,
right means align left."
    :type 'symbol
    :group 'ivy-filthy-rich)

  (defcustom ivy-filthy-rich-max-length 0
    "The max length of one entry (one line on ivy buffer).
If it is zero, the max-length is (1- (frame-width))"
    :type 'number
    :group 'ivy-filthy-rich)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-mode))


(use-package counsel-projectile
  :bind (("C-c f" . #'counsel-projectile)
         ("C-c F" . #'counsel-projectile-switch-project))
  :hook (counsel-mode . counsel-projectile-mode)
  :init (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point)))


(use-package prescient
  :commands prescient-persist-mode
  :init (prescient-persist-mode 1))


(use-package ivy-prescient
  :commands ivy-prescient-re-builder
  :custom-face
  (ivy-minibuffer-match-face-1 ((t (:inherit font-lock-doc-face :foreground nil))))
  :init
  (defun ivy-prescient-non-fuzzy (str)
    "Generate an Ivy-formatted non-fuzzy regexp list for the given STR.
This is for use in `ivy-re-builders-alist'."
    (let ((prescient-filter-method '(literal regexp)))
      (ivy-prescient-re-builder str)))

  (setq ivy-prescient-retain-classic-highlighting t
        ivy-re-builders-alist
        '((counsel-ag . ivy-prescient-non-fuzzy)
          (counsel-rg . ivy-prescient-non-fuzzy)
          (counsel-pt . ivy-prescient-non-fuzzy)
          (counsel-grep . ivy-prescient-non-fuzzy)
          (counsel-imenu . ivy-prescient-non-fuzzy)
          (counsel-yank-pop . ivy-prescient-non-fuzzy)
          (swiper . ivy-prescient-non-fuzzy)
          (swiper-isearch . ivy-prescient-non-fuzzy)
          (swiper-all . ivy-prescient-non-fuzzy)
          (lsp-ivy-workspace-symbol . ivy-prescient-non-fuzzy)
          (lsp-ivy-global-workspace-symbol . ivy-prescient-non-fuzzy)
          (insert-char . ivy-prescient-non-fuzzy)
          (counsel-unicode-char . ivy-prescient-non-fuzzy)
          (t . ivy-prescient-re-builder))
        ivy-prescient-sort-commands
        '(:not swiper swiper-isearch ivy-switch-buffer
               lsp-ivy-workspace-symbol ivy-resume ivy--restore-session
               counsel-grep counsel-git-grep counsel-rg counsel-ag
               counsel-ack counsel-fzf counsel-pt counsel-imenu
               counsel-org-capture counsel-load-theme counsel-yank-pop
               counsel-recentf counsel-buffer-or-recentf))
  (ivy-prescient-mode 1))


;; Integrate yasnippet
(use-package ivy-yasnippet
  :bind ("C-c C-y" . ivy-yasnippet))


(provide 'ivy-config)
;;; ivy-config.el ends here
