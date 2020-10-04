;;; agenda.el --- Configs Vim keymaps for agenda.
;;; Commentary:
;;;
;;; Code:

(defun agenda-vim-mode ()
  "Agenda vim keybindings."
  (eval-after-load 'org-agenda
    '(progn
       (evil-set-initial-state 'org-agenda-mode 'normal)
       (evil-define-key 'normal org-agenda-mode-map
         (kbd "<RET>") 'org-agenda-switch-to
         (kbd "\t") 'org-agenda-goto

         "q" 'org-agenda-quit
         "r" 'org-agenda-redo
         "S" 'org-save-all-org-buffers
         "gj" 'org-agenda-goto-date
         "gJ" 'org-agenda-clock-goto
         "gm" 'org-agenda-bulk-mark
         "go" 'org-agenda-open-link
         "s" 'org-agenda-schedule
         "+" 'org-agenda-priority-up
         "," 'org-agenda-priority
         "-" 'org-agenda-priority-down
         "y" 'org-agenda-todo-yesterday
         "n" 'org-agenda-add-note
         "t" 'org-agenda-todo
         ":" 'org-agenda-set-tags
         ";" 'org-timer-set-timer
         "I" 'helm-org-task-file-headings
         "i" 'org-agenda-clock-in-avy
         "O" 'org-agenda-clock-out-avy
         "u" 'org-agenda-bulk-unmark
         "x" 'org-agenda-exit
         "j"  'org-agenda-next-line
         "k"  'org-agenda-previous-line
         "vt" 'org-agenda-toggle-time-grid
         "va" 'org-agenda-archives-mode
         "vw" 'org-agenda-week-view
         "vl" 'org-agenda-log-mode
         "vd" 'org-agenda-day-view
         "vc" 'org-agenda-show-clocking-issues
         "g/" 'org-agenda-filter-by-tag
         "o" 'delete-other-windows
         "gh" 'org-agenda-holiday
         "gv" 'org-agenda-view-mode-dispatch
         "f" 'org-agenda-later
         "b" 'org-agenda-earlier
         "c" 'helm-org-capture-templates
         "e" 'org-agenda-set-effort
         "n" nil  ; evil-search-next
         "{" 'org-agenda-manipulate-query-add-re
         "}" 'org-agenda-manipulate-query-subtract-re
         "A" 'org-agenda-toggle-archive-tag
         "." 'org-agenda-goto-today
         "0" 'evil-digit-argument-or-evil-beginning-of-line
         "<" 'org-agenda-filter-by-category
         ">" 'org-agenda-date-prompt
         "F" 'org-agenda-follow-mode
         "D" 'org-agenda-deadline
         "H" 'org-agenda-holidays
         "J" 'org-agenda-next-date-line
         "K" 'org-agenda-previous-date-line
         "L" 'org-agenda-recenter
         "P" 'org-agenda-show-priority
         "R" 'org-agenda-clockreport-mode
         "Z" 'org-agenda-sunrise-sunset
         "T" 'org-agenda-show-tags
         "X" 'org-agenda-clock-cancel
         "[" 'org-agenda-manipulate-query-add
         "g\\" 'org-agenda-filter-by-tag-refine
         "]" 'org-agenda-manipulate-query-subtract))))

(defun org-TODO-keywords ()
  "Add and customize org TODO keywords."

  (setq org-todo-keywords
        '((sequence "TODO" "NOTE" "IN-PROGRESS" "WAITING" "DONE")))

  (setq org-todo-keyword-faces '(("TODO"         . (:foreground "#ff8080" :weight bold))
                                 ("NOTE"         . (:foreground "#ffe9aa" :weight bold))
                                 ("IN-PROGRESS"  . (:foreground "#A020F0" :weight bold))
                                 ("WAITING"      . (:foreground "#ffb378" :weight bold))
                                 ("DONE"         . (:foreground "#1E90FF" :weight bold)))))

(defun schedule-and-deadline-config ()
  "Set delays and prewarnings for its."
  (setq org-agenda-skip-scheduled-delay-if-deadline t
        org-agenda-skip-deadline-prewarning-if-scheduled t))

(defun org-capture-config ()
  "Org caputer config."
  (setq org-default-notes-file "~/org/capture.org")
  (global-set-key (kbd "C-c c") 'org-capture))

(agenda-vim-mode)
(org-TODO-keywords)
(schedule-and-deadline-config)
(org-capture-config)

;;; agenda.el ends here.
