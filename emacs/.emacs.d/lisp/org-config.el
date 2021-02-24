;;; org-config.el --- Emacs org mode config
;;; Commentary:
;;; Emacs org config
;;; Code:

(defun setup-org-packages ()

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-download
  :ensure t
  :hook ('dired-mode-hook 'org-download-enable)))

(defun org-capture-config ()
  "Org caputer config."
  (setq org-default-notes-file "~/org/capture.org")
  (global-set-key (kbd "C-c c") 'org-capture))

(defun org-tags-set ()
  "Set org agenda custom tags."
 (setq org-tag-alist '(("work " . ?w) ("coll " . ?c) ("personal" . ?p))))

(defun org-TODO-keywords ()
  "Add and customize org TODO keywords."
  (setq org-todo-keywords
        '((sequence "TODO" "NOTE" "IMPORTANT" "WAITING" "CANCELLED" "IN-PROGRESS"  "WORKING"  "DONE")))

  (setq org-todo-keyword-faces '(("TODO"         . (:foreground "#ff8080" :weight bold))
                                 ("NOTE"         . (:foreground "#ffe9aa" :weight bold))
                                 ("IMPORTANT"    . (:foreground "#f32020" :weight bold))
                                 ("WAITING"      . (:foreground "#ffb378" :weight bold))
                                 ("CANCELLED"    . (:foreground "#ff6c6b" :weight bold))
                                 ("IN-PROGRESS"  . (:foreground "#A020F0" :weight bold))
                                 ("WORKING"      . (:foreground "#4db5bd" :weight bold))
                                 ("DONE"         . (:foreground "#1E90FF" :weight bold)))))

(org-capture-config)
(org-tags-set)
(org-TODO-keywords)

(provide 'org-config)
;;; org-config.el ends here
