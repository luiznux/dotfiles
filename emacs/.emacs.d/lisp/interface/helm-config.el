;;; helm-config.el --- Configuration for helm package
;;; Commentary:
;;;
;;; Code:

(use-package helm
  :config
  ;;(global-set-key (kbd "M-x") #'helm-M-x)
  ;;(global-set-key (kbd "C-x C-f") #'helm-find-files)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (define-key helm-map (kbd "M-j") 'helm-next-line)
  (define-key helm-map (kbd "M-k") 'helm-previous-line)
  (define-key helm-map (kbd "M-h") 'helm-next-source)
  (define-key helm-map (kbd "M-l") (kbd "RET"))
  (define-key helm-map (kbd "C-S-h") 'describe-key)
  (define-key helm-map [escape] 'helm-keyboard-quit))

(require 'helm-config)
(defun helm-config()

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t
        helm-echo-input-in-header-line        t)

  (defun spacemacs//helm-hide-minibuffer-maybe ()
    "Hide minibuffer in Helm session if we use the header line as input field."
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'face
                     (let ((bg-color (face-background 'default nil)))
                       `(:background ,bg-color :foreground ,bg-color)))
        (setq-local cursor-type nil))))

  (add-hook 'helm-minibuffer-set-up-hook
            'spacemacs//helm-hide-minibuffer-maybe)

  (setq helm-autoresize-max-height     0
        helm-autoresize-min-height     20
        helm-M-x-fuzzy-match           t ;; optional fuzzy matching for helm-M-x
        helm-buffers-fuzzy-matching    t
        helm-recentf-fuzzy-match       t)

  (global-set-key (kbd "C-x b") 'helm-mini)
  (helm-autoresize-mode 1))

(helm-config)

;;; helm-config.el ends here
