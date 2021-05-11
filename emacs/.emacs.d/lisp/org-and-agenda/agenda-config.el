;;; agenda-config.el --- Package for config org agenda
;;; Commentary:
;;; Emacs org agenda config
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


(defun org-agenda-custom-config ()

  (setq org-agenda-custom-commands
        '(("x" "Describe command here" agenda "")))

  (setq org-agenda-skip-deadline-prewarning-if-scheduled   t
        org-agenda-skip-scheduled-delay-if-deadline        t
        org-agenda-prefer-last-repeat                      t
        org-agenda-show-future-repeats                     nil
        org-catch-invisible-edits                          'smart
        org-agenda-compact-blocks                          t
        org-hide-emphasis-markers                          t
        org-pretty-entities                                nil
        org-startup-indented                               t
        org-agenda-span                                    15 ; show 15 days in agenda
        org-log-done                                       'time)

  ;; Babel
  (setq org-confirm-babel-evaluate  nil
        org-src-fontify-natively    t
        org-src-tab-acts-natively   t)


  (setq org-agenda-files ;set org agenda files
        '("~/org/agenda .org" "~/org/personal.org" "~/org/bday .org" "~/org/coll.org" "~/org/work .org")))

;;; Custom agenda defuns
(defun ll/org/agenda/color-headers-with (tag col col2)
  "Color agenda lines matching TAG with color COL(foreground) and COL2(background)."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward tag nil t)
                                        ;(Unless (find-in-line "\\[#[A-Z]\\]")
    (let ((todo-end (or (ll/org/agenda/find-todo-word-end)
                        (point-at-bol)))
          (tags-beginning (or (find-in-line " :" t)
                              (point-at-eol))))
      (add-text-properties todo-end
                           tags-beginning
                           `(face (:foreground ,col :background ,col2 :weight bold))))))
;; Helper definitions
(setq ll/org/agenda-todo-words
      '("work :" "coll:" "personal:" "agenda :" "bday :"))


(defun find-in-line (needle &optional beginning count)
  "Find the position of the start of NEEDLE in the current line.
If BEGINNING is non-nil, find the beginning of NEEDLE in the
current line.  If COUNT is non-nil, find the COUNT'th occurrence
from the left."

  (save-excursion
    (beginning-of-line)

    (let ((found (re-search-forward needle (point-at-eol) t count)))
      (if beginning
          (match-beginning 0)
        found))))


(defun ll/org/agenda/find-todo-word-end ()

  "Find TODO keyword on the end."

  (reduce (lambda (a b) (or a b))
          (mapcar #'find-in-line ll/org/agenda-todo-words)))


(defun ll/org/colorize-headings ()
  "Color all headings with :pers: colors."

  (ll/org/agenda/color-headers-with "work :" "#2d2d2d" "#FA74B2")
  (ll/org/agenda/color-headers-with "coll:" "#2d2d2d" "#c792ea")
  (ll/org/agenda/color-headers-with "personal:" "#2d2d2d" "#839ce4")
  (ll/org/agenda/color-headers-with "agenda :" "#2d2d2d" "#da8548")
  (ll/org/agenda/color-headers-with "bday :" "#2d2d2d" "#89ddff"))

;;-------------------

(agenda-vim-mode)
(org-agenda-custom-config)
(add-hook 'org-agenda-finalize-hook #'ll/org/colorize-headings)

(provide 'agenda-config)
;;; agenda-config.el ends here
