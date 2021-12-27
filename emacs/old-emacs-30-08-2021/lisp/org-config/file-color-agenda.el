;;; agenda-config.el --- Package for config org agenda
;;; Commentary:
;;; Emacs org agenda config
;;; Code:

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
      '("work :" "coll:" "project:" "agenda :" "bday :" "cap :"))


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
  (ll/org/agenda/color-headers-with "project:" "#2d2d2d" "#839ce4")
  (ll/org/agenda/color-headers-with "agenda :" "#2d2d2d" "#da8548")
  (ll/org/agenda/color-headers-with "bday :" "#2d2d2d" "#89ddff")
  (ll/org/agenda/color-headers-with "cap :" "#2d2d2d" "#c3e88d"))


(add-hook 'org-agenda-finalize-hook #'ll/org/colorize-headings)

(provide 'file-color-agenda)
;;; file-color-agenda.el ends here
