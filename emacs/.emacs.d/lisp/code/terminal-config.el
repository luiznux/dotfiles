
(use-package vterm
  :defines evil-move-cursor-back evil-insert-state-cursor
  :functions evil-collection-vterm-escape-stay
  :config
  (defun evil-collection-vterm-escape-stay ()
    "Go back to normal state but don't move
cursor backwards. Moving cursor backwards is the default vim behavior but it is
not appropriate in some cases like terminals."
    (setq-local evil-move-cursor-back nil))

  (add-hook 'vterm-mode-hook #'evil-collection-vterm-escape-stay))

(use-package  multi-vterm
  :after vterm
  :commands multi-vterm-next multi-vterm-prev
  :functions (vterm-send-return
              vterm--self-insert
              evil-insert-state
              evil-define-key
              evil-insert-resume)
  :config
  (add-hook 'vterm-mode-hook
			(lambda ()
			  (setq-local evil-insert-state-cursor 'box)
			  (evil-insert-state)))
  (define-key vterm-mode-map [return]                      #'vterm-send-return)

  (setq vterm-keymap-exceptions nil)
  (evil-define-key 'insert vterm-mode-map (kbd "C-e")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-f")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-a")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-v")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-b")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-w")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-u")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-n")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-m")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-p")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-j")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-k")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-r")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-t")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-g")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-c")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-SPC")    #'vterm--self-insert)
  (evil-define-key 'normal vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  (evil-define-key 'normal vterm-mode-map (kbd ",c")       #'multi-vterm)
  (evil-define-key 'normal vterm-mode-map (kbd ",n")       #'multi-vterm-next)
  (evil-define-key 'normal vterm-mode-map (kbd ",p")       #'multi-vterm-prev)
  (evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume))

(use-package vterm-toggle
  :after vterm
  :defines centaur-tabs-buffer-groups-function vterm-toggle--vterm-buffer-p-function
  :config
  (global-set-key [f2] 'vterm-toggle)
  (global-set-key [C-f2] 'vterm-toggle-cd)
  (define-key vterm-mode-map [(control return)]   #'vterm-toggle-insert-cd)
  (define-key vterm-mode-map (kbd "s-n") 'vterm-toggle-forward) ;Switch to next vterm buffer
  (define-key vterm-mode-map (kbd "s-p") 'vterm-toggle-backward) ;Switch to previous vterm buffer

  (setq vterm-toggle-cd-auto-create-buffer nil)

  (defun myssh()
    (interactive)
    (let ((default-directory "/ssh:root@host:~"))
      (vterm-toggle-cd)))

  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
               '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . bottom)
                 (dedicated . t) ;dedicated is supported in emacs27
                 (reusable-frames . visible)
                 (window-height . 0.3)))

  (setq centaur-tabs-buffer-groups-function 'vmacs-awesome-tab-buffer-groups)
  (defun vmacs-awesome-tab-buffer-groups ()
    "`vmacs-awesome-tab-buffer-groups' control buffers' group rules. "
    (list
     (cond
      ((derived-mode-p 'eshell-mode 'term-mode 'shell-mode 'vterm-mode)
       "Term")
      ((string-match-p (rx (or
                            "\*Helm"
                            "\*helm"
                            "\*tramp"
                            "\*Completions\*"
                            "\*sdcv\*"
                            "\*Messages\*"
                            "\*Ido Completions\*"
                            ))
                       (buffer-name))
       "Emacs")
      (t "Common"))))

  (setq vterm-toggle--vterm-buffer-p-function 'vmacs-term-mode-p)
  (defun vmacs-term-mode-p(&optional args)
    (derived-mode-p 'eshell-mode 'term-mode 'shell-mode 'vterm-mode)))

(provide 'terminal-config)
