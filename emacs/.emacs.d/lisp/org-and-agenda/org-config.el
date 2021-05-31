;;; org-config.el --- Emacs org mode config
;;; Commentary:
;;; Emacs org config
;;; Code:

(defun setup-org-packages ()
  "Setup and call org packages."

  (use-package org-bullets
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

  (use-package org-download
    :hook ('dired-mode-hook 'org-download-enable))

  ;; Presentation
  (use-package org-tree-slide
    :diminish
    :functions (org-display-inline-images
                org-remove-inline-images)
    :bind (:map org-mode-map
                ("s-<f7>" . org-tree-slide-mode)
                :map org-tree-slide-mode-map
                ("<left>" . org-tree-slide-move-previous-tree)
                ("<right>" . org-tree-slide-move-next-tree)
                ("S-SPC" . org-tree-slide-move-previous-tree)
                ("SPC" . org-tree-slide-move-next-tree))
    :hook ((org-tree-slide-play . (lambda ()
                                    (text-scale-increase 4)
                                    (org-display-inline-images)
                                    (read-only-mode 1)))
           (org-tree-slide-stop . (lambda ()
                                    (text-scale-increase 0)
                                    (org-remove-inline-images)
                                    (read-only-mode -1))))
    :config
    (org-tree-slide-simple-profile)
    (setq org-tree-slide-skip-outline-level 2))

  (use-package org-superstar
    :hook (org-mode . org-superstar-mode))

  (use-package org-fragtog
    :config (add-hook 'org-mode-hook 'org-fragtog-mode))

  (use-package org-alert))


(defun org-capture-config ()
  "Org caputer config."

  (setq org-default-notes-file "~/org/cap .org")
  (global-set-key (kbd "C-c c") 'org-capture))


(defun org-tags-set ()
  "Set org agenda custom tags."

  (setq org-tag-alist '(("work " . ?w) ("coll" . ?c) ("personal" . ?p))))

(defun org-priority-config ()
  "Set org priority custom faces."
  (setq org-priority-faces
        '((?A . (:foreground "red"   ))
          (?B . (:foreground "yellow"))
          (?C . (:foreground "green" )))))


(defun org-TODO-keywords ()
  "Add and customize org TODO keywords."

  (setq org-todo-keywords
        '((sequence "TODO" "NOTE" "IMPORTANT" "WAITING" "IN-PROGRESS" "WORKING" "|" "CANCELLED" "DONE")))

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
(org-priority-config)
(org-TODO-keywords)
(setup-org-packages)

(provide 'org-config)
;;; org-config.el ends here
