;;; arduino-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "arduino-mode" "arduino-mode.el" (23801 42411
;;;;;;  712743 85000))
;;; Generated autoloads from arduino-mode.el

(autoload 'arduino-sketch-new "arduino-mode" "\
A command to create new `SKETCH' in ARDUINO_HOME (~/Arduino).

\(fn SKETCH)" t nil)

(autoload 'arduino-mode "arduino-mode" "\
Major mode for editing Arduino code.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.pde\\'" . arduino-mode))

(add-to-list 'auto-mode-alist '("\\.ino\\'" . arduino-mode))

;;;***

;;;### (autoloads nil "ede-arduino" "ede-arduino.el" (23801 42411
;;;;;;  684742 983000))
;;; Generated autoloads from ede-arduino.el

(defvar ede-arduino-preferences-file "~/.arduino/preferences.txt" "\
The location of personl preferences for the arduino IDE.
Note: If this changes, we need to also update the autoload feature.")

(custom-autoload 'ede-arduino-preferences-file "ede-arduino" t)

(autoload 'ede-arduino-root "ede-arduino" "\
Get the root project directory for DIR.
The only arduino sketches allowed are those configured by the arduino IDE
in their sketch directory.

If BASEFILE is non-nil, then convert root to the project basename also.

Consider expanding this at some later date.

\(fn &optional DIR BASEFILE)" nil nil)

(autoload 'ede-arduino-file "ede-arduino" "\
Get a file representing the root of this arduino project.
It is a file ending in .pde or .ino that has the same basename as
the directory it is in.  Optional argument DIR is the directory
to check.

\(fn &optional DIR)" nil nil)

(autoload 'ede-arduino-load "ede-arduino" "\
Return an Arduino project object if there is one.
Return nil if there isn't one.
Argument DIR is the directory it is created for.
ROOTPROJ is nil, sinc there is only one project for a directory tree.

\(fn DIR &optional ROOTPROJ)" nil nil)

(require 'ede/auto)

(add-to-list 'ede-project-class-files (ede-project-autoload :name "Arduino sketch" :file 'ede-arduino :proj-root-dirmatch (ede-project-autoload-dirmatch :fromconfig (expand-file-name ede-arduino-preferences-file) :configregex "^sketchbook.path=\\([^\n]+\\)$" :configregexidx 1) :proj-file 'ede-arduino-file :proj-root 'ede-arduino-root :load-type 'ede-arduino-load :class-sym 'ede-arduino-project :safe-p t :new-p t) t)

;;;***

;;;### (autoloads nil "flycheck-arduino" "flycheck-arduino.el" (23801
;;;;;;  42411 704743 56000))
;;; Generated autoloads from flycheck-arduino.el

(autoload 'flycheck-arduino-setup "flycheck-arduino" "\
Setup Flycheck Arduino.
Add `arduino' to `flycheck-checkers'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("arduino-mode-init.el" "arduino-mode-pkg.el")
;;;;;;  (23801 42411 692743 12000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; arduino-mode-autoloads.el ends here
