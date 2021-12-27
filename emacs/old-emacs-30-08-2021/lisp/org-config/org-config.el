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
    :functions (org-display-inline-images org-remove-inline-images)
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

  (use-package org-make-toc
    :after org)

  (use-package org-sticky-header
    :hook (org-mode . org-sticky-header-mode))

  (use-package org-autolist
    :hook (org-mode . (lambda () (org-autolist-mode))))

  (use-package org-wild-notifier
    :config
    (setq org-wild-notifier-alert-times-property "NOTIFIER"
          org-wild-notifier-keyword-whitelist    '("TODO" "WAITING" "IMPORTANT" "DOING"))
    (org-wild-notifier-mode))

  (use-package org-gcal
    :if  (file-exists-p "~/org/org-api.el")
    :defines luiznux-client-id luiznux-client-secret
    :config
    (load "~/org/org-api.el") ;; file with the keys
    (setq org-gcal-client-id  luiznux-client-id
          org-gcal-client-secret luiznux-client-secret
          org-gcal-file-alist '(("luiztagli10@gmail.com" .  "~/org/gcal.org"))))

  (use-package verb
    :config
    (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

  (use-package org-roam
    :custom
    (org-roam-directory (file-truename "~/org/"))
    :bind (("C-c n l" . org-roam-buffer-toggle)
           ("C-c n f" . org-roam-node-find)
           ("C-c n g" . org-roam-graph)
           ("C-c n i" . org-roam-node-insert)
           ("C-c n c" . org-roam-capture)
           ;; Dailies
           ("C-c n j" . org-roam-dailies-capture-today))
    :config
    (org-roam-db-autosync-mode)
    ;; If using org-roam-protocol
    (require 'org-roam-protocol))

  (use-package org-web-tools)
  (use-package org-alert)
  (use-package org-sidebar))

(use-package org
  :ensure nil
  :bind ("C-c c" . 'org-capture)
  :defines org-babel-clojure-backend
  :init
  ;; load some org-modules
  (with-eval-after-load 'org
    (add-to-list 'org-modules 'org-habit t))

  (setq org-catch-invisible-edits         'smart
        org-log-done                      'time
        org-pretty-entities                nil
        org-hide-emphasis-markers          t
        org-startup-indented               t
        org-use-fast-todo-selection        t

        ;; log time on rescheduling and changing deadlines
        org-log-reschedule                 'time
        org-log-redeadline                 'time
        org-agenda-show-log                t

        ;; on links `RET' follows the link
        org-return-follows-link            t
        org-reverse-note-order             t

        ;; turn on speed keys for headlines
        org-use-speed-commands             t

        ;; Set `org-agenda' custom tags
        org-tag-alist                      '(("work" . ?w)
                                             ("agenda" . ?a)
                                             ("project" . ?p)
                                             ("capture" . ?c))

        ;; Set `org' priority custom faces
        org-priority-faces                 '((?A . (:foreground "red"   ))
                                             (?B . (:foreground "yellow"))
                                             (?C . (:foreground "green" )))

        ;; Add and customize org TODO keywords
        org-todo-keywords                  (quote ((sequence "TODO(t)" "DOING(o!)" "|" "DONE(d!)")
                                                   (sequence "IMPORTANT(i)" "WAITING(w@/!)" "|" "CANCELLED(c@/!)")))

        org-todo-keyword-faces             '(("TODO"         . (:foreground "#ff8080" :weight bold))
                                             ("IMPORTANT"    . (:foreground "#f32020" :weight bold))
                                             ("WAITING"      . (:foreground "#ffb378" :weight bold))
                                             ("DOING"        . (:foreground "#A020F0" :weight bold))
                                             ("CANCELLED"    . (:foreground "#ff6c6b" :weight bold))
                                             ("DONE"         . (:foreground "#1E90FF" :weight bold)))
        ;; config `org-capture'
        org-default-notes-file             "~/org/cap .org"

        ;; `org-babel' config
        org-confirm-babel-evaluate         nil
        org-src-fontify-natively           t
        org-src-tab-acts-natively          t
        org-src-window-setup               'current-window
        ;; `cider' backend for org babel
        org-babel-clojure-backend          'cider)

  ;; cool message for scratch  ( ͡° ͜ʖ ͡°)
  (setq initial-major-mode 'org-mode
        initial-scratch-message "Eai seu *CORNO* 🐂 \n\n#+begin_src\n\n#+end_src")

  ;; varlist for `org-babel' languages
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
                               (css . t)
                               (verb . t)))

  (use-package ob-go
    :init (cl-pushnew '(go . t) load-language-list))

  (use-package ob-http
    :init(cl-pushnew '(http . t) load-language-list))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-list)

  (defun org-src--construct-edit-buffer-name (org-buffer-name lang)
    "Construct the buffer name for a source editing buffer."
    (concat org-buffer-name " (org src)"))

  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

  ;; easy templates special blocks in latex export
  (add-to-list 'org-structure-template-alist '("f" . "figure"))


  ;; Redo agenda after capturing.
  (add-hook 'org-capture-after-finalize-hook 'org-agenda-maybe-redo)

  :config
  (setup-org-packages))


(use-package org-habit
  :ensure nil
  :init
  (setq org-habit-completed-glyph           10003
        org-habit-today-glyph               10082
        org-habit-graph-column              60
        org-habit-preceding-days            2
        org-habit-following-days            2
        org-habit-show-all-today            nil
        org-habit-following-days            5))


(use-package org-agenda
  :ensure nil
  :functions renewOrgBuffer org-agenda-maybe-redo org-agenda-files
  :init
  (setq org-agenda-skip-deadline-prewarning-if-scheduled   t
        org-agenda-skip-scheduled-delay-if-deadline        t
        org-agenda-skip-deadline-if-done                   t
        org-agenda-breadcrumbs-separator                   " ❱ "
        org-agenda-prefer-last-repeat                      t
        org-agenda-show-future-repeats                     t
        org-agenda-skip-unavailable-files                  t
        org-agenda-compact-blocks                          nil
        org-agenda-block-separator                         61
        org-agenda-span                                    6
        calendar-week-start-day                            1
        org-agenda-current-time-string                     " ᐊ┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈ NOW "
        org-agenda-time-grid                               '((daily today require-timed)
                                                             (800 1000 1200 1400 1600 1800 2000)
                                                             " ...... " "----------------")

        org-directory                                      "~/org"
        org-agenda-files                                   (quote
                                                            ("~/org/agenda .org"
                                                             "~/org/project.org"
                                                             "~/org/bday .org"
                                                             "~/org/coll.org"
                                                             "~/org/cap .org"
                                                             "~/org/work .org"))

        org-agenda-custom-commands                        '(("x" "Simple agenda view"
                                                             ((agenda "")))))
  ;; Diary
  ;;org-agenda-include-diary                           t
  ;;diary-file                                         "~/org/diary"
  ;;calendar-mark-diary-entries-flag                   t
  ;;calendar-view-diary-initially-flag                 t
  ;;calendar-mark-diary-entries-flag                   t
  ;;Remove default holidays
  ;;holiday-christian-holidays                         nil
  ;;holiday-oriental-holidays                          nil
  ;;holiday-bahai-holidays                             nil
  ;;holiday-islamic-holidays                           nil
  ;;holiday-hebrew-holidays                            nil
  ;; Calendar Hooks
  ;;(add-hook 'diary-display-hook 'fancy-diary-display)
  (add-hook 'today-visible-calendar-hook 'calendar-mark-today)
  ;;(add-hook 'list-diary-entries-hook 'sort-diary-entries t)

  :config
  (defun mpereira/org-paste-clipboard-image ()
    "TODO: docstring."
    (interactive)
    (if (executable-find "pngpaste")
        (let ((image-file (concat temporary-file-directory
                                  (make-temp-name "org-image-paste-")
                                  ".png")))
          (call-process-shell-command (concat "pngpaste " image-file))
          (insert (concat  "#+CAPTION: " (read-string "Caption: ") "\n"))
          (insert (format "[[file:%s]]" image-file))
          (org-display-inline-images))
      (message "Requires pngpaste in PATH")))

  (defun my/save-all-agenda-buffers ()
    "Function used to save all agenda buffers that are currently open,
based on `org-agenda-files'."
    (interactive)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
        (set-buffer buffer)
        (when (member (buffer-file-name)
                      (mapcar 'expand-file-name (org-agenda-files t)))
          (save-buffer)))))

  (defun renewOrgBuffer ()
    (interactive)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'org-agenda-mode)
          (org-agenda-maybe-redo)))))

  ;; save all the agenda files after each capture
  ;; (add-hook 'org-agenda-finalize-hook 'my/save-all-agenda-buffers)

  ;; Auto save agenda files
  ;;(add-hook 'org-agenda-mode-hook
  ;;          (lambda ()
  ;;            (add-hook 'auto-save-hook 'org-save-all-org-buffers nil t)
  ;;            (auto-save-mode)))
  ;; Auto rebuild agenda buffer after 30 seconds
  (run-with-timer 3 600 #'renewOrgBuffer))

;;(cl-defun ap/org-set-level-faces (&key (first-parent 'outline-1))
;;   (solarized-with-color-variables 'dark
;;                                   (require 'color)
;;                                   (require 'dash)
;;                                   (require 'org-inlinetask)
;;                                   (let ((org-level-color-list (-cycle (list red orange yellow green cyan blue violet magenta))))
;;                                     (cl-flet ((modify-color (color) (thread-first color
;;                                                                       (color-desaturate-name 30))))
;;                                       (cl-loop for level from 1 to (1- org-inlinetask-min-level)
;;                                                for face = (intern (format "org-level-%s" level))
;;                                                for parent = (cl-case level
;;                                                               (1 (list first-parent 'highlight))
;;                                                               (t (intern (format "org-level-%s" (1- level)))))
;;                                                for height = (cond ((= level 1) 1.3)
;;                                                                   ((<= level 4) 0.9)
;;                                                                   (t 1.0))
;;                                                for weight = (if (<= level 8) 'bold 'normal)
;;                                                unless (internal-lisp-face-p face)
;;                                                do (custom-declare-face face `((t :inherit ,(intern (format "outline-%s" (1- level)))))
;;                                                                        (format "Face for Org level %s headings." (1- level)))
;;                                                do (set-face-attribute face nil
;;                                                                       :inherit parent
;;                                                                       :foreground (modify-color (nth level org-level-color-list))
;;                                                                       :height height
;;                                                                       :weight weight
;;                                                                       :overline t)
;;                                                collect face into faces
;;                                                finally do (defconst org-level-faces faces)
;;                                                finally do (setq org-n-level-faces (length org-level-faces)))))))

;;(ap/org-set-level-faces :first-parent 'variable-pitch)


(provide 'org-config)
;;; org-config.el ends here
