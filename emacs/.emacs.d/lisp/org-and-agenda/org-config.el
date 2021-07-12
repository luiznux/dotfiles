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

  ;; Table of contents
  (use-package toc-org
    :hook (org-mode . toc-org-mode))

  (use-package org-autolist
    :hook (org-mode . (lambda () (org-autolist-mode))))

  (use-package org-alert)

  (use-package org-wild-notifier
    :config
    (setq org-wild-notifier-alert-times-property "NOTIFIER"
          org-wild-notifier-keyword-whitelist    '("TODO" "WAITING" "IMPORTANT" "DOING")
          org-wild-notifier-keyword-blacklist    '("NOTE"))
    (org-wild-notifier-mode)))

(defun org-capture-config ()
  "Org caputer config."

  (setq org-default-notes-file "~/org/cap .org")
  (global-set-key (kbd "C-c c") 'org-capture))


(defun org-tags-set ()
  "Set org agenda custom tags."

  (setq org-tag-alist '(("work" . ?w) ("coll" . ?c) ("project" . ?p))))

(defun org-priority-config ()
  "Set org priority custom faces."
  (setq org-priority-faces
        '((?A . (:foreground "red"   ))
          (?B . (:foreground "yellow"))
          (?C . (:foreground "green" )))))


(defun org-TODO-keywords ()
  "Add and customize org TODO keywords."

  (setq org-todo-keywords
        '((sequence "TODO(t)"
                    "NOTE(n)"
                    "WAITING(w)"
                    "IMPORTANT(i)"
                    "DOING(o)"
                    "|"
                    "DONE(d@)"
                    "CANCELLED(c@/!)")))

  (setq org-todo-keyword-faces '(("TODO"         . (:foreground "#ff8080" :weight bold))
                                 ("NOTE"         . (:foreground "#ffe9aa" :weight bold))
                                 ("WAITING"      . (:foreground "#ffb378" :weight bold))
                                 ("IMPORTANT"    . (:foreground "#f32020" :weight bold))
                                 ("DOING"        . (:foreground "#A020F0" :weight bold))
                                 ("CANCELLED"    . (:foreground "#ff6c6b" :weight bold))
                                 ("DONE"         . (:foreground "#1E90FF" :weight bold)))))

(defun org-config ()
  "Config org."
  (setq org-catch-invisible-edits                    'smart
        org-hide-emphasis-markers                    t
        org-pretty-entities                          nil
        org-startup-indented                         t
        org-log-done                                 'time

        ;; log time on rescheduling and changing deadlines
        org-log-reschedule                           'time
        org-log-redeadline                           'time

        ;; on links `RET' follows the link
        org-return-follows-link                      t
        org-reverse-note-order                       t

        ;; turn on speed keys for headlines
        org-use-speed-commands                       t)

  ;; Babel
  (setq org-confirm-babel-evaluate  nil
        org-src-fontify-natively    t
        org-src-tab-acts-natively   t)

  (defvar load-language-list '((emacs-lisp . t)
                               (clojure . t)
                               (lisp . t)
                               (eshell . t)
                               (shell . t)
                               (sql . t)
                               (C . t)
                               (java . t)
                               (python . t)
                               (sed . t)
                               (latex . t)
                               (js . t)
                               (css . t)))

  (use-package ob-go
    :init (cl-pushnew '(go . t) load-language-list))

  (use-package ob-http
    :init(cl-pushnew '(http . t) load-language-list))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-list)

  ;; easy templates special blocks in latex export
  (add-to-list 'org-structure-template-alist '("f" . "figure")))


(org-capture-config)
(org-tags-set)
(org-priority-config)
(org-TODO-keywords)
(org-config)
(setup-org-packages)

(provide 'org-config)
;;; org-config.el ends here
