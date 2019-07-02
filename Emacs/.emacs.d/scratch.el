;;; scratch.el --- Function to load scratch message file
;;; Commentary:
;;; Function to load scratch message file
;;;
;;; Code:

(defun load-scratch-message ()
  "Load scratchbuffermsg.txt content in the scratch buffer."
  (let ((filename "~/.emacs.d/scratchbuffermsg.txt"))
    (when (and (file-exists-p filename)
               (get-buffer "*scratch*"))
      (with-current-buffer "*scratch*"
        (erase-buffer)
        (insert-file-contents filename)))))

(load-scratch-message)

;;; scratch.el ends here
