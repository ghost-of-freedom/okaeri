;;; okaeri.el --- personal startup buffer                     -*- lexical-binding: t; -*-

;; Copyright (C) 2023 wanko

;; Author: wanko
;; Keywords: dashboard startup splash buffer initial
;; Version: 1.0.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a package similar to dashboard.el; it replaces the default splash
;; buffer with a customized one. This is not a package for the community, I have
;; written it for my own convenience and use. Thus, I would advise against using
;; it directly, as the customization and accommodation of different workflows are
;; not at a level I would be satisfied with had it been a package written for the
;; general public. It can serve as a good starting point or reference if the reader
;; wants to write something of their own, however. Nevertheless, for the daring, here
;; are the usage/configuration instructions:

;; For the buffer to work correctly, (okaeri-init) needs to be somewhere in the
;; configuration. This sets up the hooks and initial buffer. You can still open the
;; dashboard without it with (okaeri-open), but it will not resize or refresh on
;; window changes.

;; Then, you will want to give value to at least one of the -logo, -title, -body, or
;; -footer variables, so that something is displayed in the buffer. Everything except
;; -body takes a string; I don't think strings with newlines will center correctly, but
;; I haven't tested it. okaeri-body is a bit more interesting, so I will elaborate on it:

;; okaeri-body should be a list, where every odd element is a string that will be displayed
;; in the first column, and where every odd element is a list of properties accepted by the
;; make-button family of functions in button.el. For example, something like this:

;; (list "Quit emacs"
;;      `(action kill-emacs help-echo "Quits Emacs"))

;; If the function given as action is bound in okaeri-mode-map, its binding will be shown
;; in the second column. Because functions given as action argument to buttons are passed
;; up to 1 argument - the button, convenience functions are given for wrapping the actual
;; call to the function, and making sure the wrapper is used in both the mode-map binding
;; and action (so that they can match). Since example would probably be the best explanation:

;; (defun okaeri-button-wrapped-kill-emacs (&optional button)
;;   (kill-emacs 255))
;; (define-key okaeri-mode-map "q" 'okaeri-button-wrapped-kill-emacs)
;; (setq okaeri-body
;;       (list "Quit Emacs"
;; 	    `(action okaeri-button-wrapped-kill-emacs help-echo "Kill emacs")))

;; and

;; (define-key okaeri-mode-map "q" (okaeri-button-wrapper "kill-emacs"))
;; (setq okaeri-body
;;       (list "Quit Emacs"
;; 	    (okaeri-link-props "Kill emacs" "kill-emacs" 255)))

;; are functionally equivalent.

;; You can also set okaeri-hide-when-file-on-cmdline to NIL if you want the okaeri
;; buffer to show up even if files to open were given as arguments to emacs on startup.

;; Taking everything above into consideration, a sample configuration using use-package
;; would look something like this:

;; (use-package okaeri
;;   :config
;;   (setq okaeri-title "Welcome!")
;;   (setq okaeri-body
;; 	(list "Quit Emacs"
;; 	      (okaeri-link-props "Quits Emacs using kill-emacs" "kill-emacs")))
;;   (define-key okaeri-mode-map "q" (okaeri-button-wrapper "kill-emacs"))
;;   (okaeri-init))
  

;;; Code:


;; Imports

(require 'shr)
(require 'cl-lib)


;; Global variables

(defvar okaeri-buffer-name "*Okaeri*"
  "The name to use for the startup buffer.")

(defvar okaeri-buffer nil
  "The buffer shown on startup.")

(defvar okaeri-body-line-sep "        "
  "The separator to use when joining a dashboard action with its keybinding.")

(defvar okaeri-logo nil
  "Path to image to insert as logo.  Set to NIL to disable.")

(defvar okaeri-title nil
  "Text to insert in the title.  Set to NIL to disable.")

(defvar okaeri-body nil
  "Text to insert in the body.  Set to NIL to disable.")

(defvar okaeri-footer nil
  "Text to insert in the footer.  Set to NIL to disable.")

(defvar okaeri-logo-separator "\n\n"
  "String to insert after the logo.")

(defvar okaeri-title-separator "\n\n"
  "String to insert after the title.")

(defvar okaeri-body-separator "\n"
  "String to insert after the body.")

(defvar okaeri-footer-separator "\n"
  "String to insert after the footer.")

(defvar okaeri-hide-when-file-on-cmdline t
  "Whether to hide the window with buffer at startup when file was provided on command line.")

(defvar okaeri-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for okaeri mode.")


;; Major mode setup

(defgroup okaeri nil
  "Custom startup screen."
  :group 'applications)

(define-derived-mode okaeri-mode special-mode "Okaeri"
  "Show custom startup screen."
  :group 'okaeri
  :syntax-table nil
  :abbrev-table nil)


;; Faces

(defface okaeri-title
  `((t :inherit default
       :height 1000
       :extend nil))
  "okaeri")

(defface okaeri-body
  `((t :inherit default
       :height 300
       :extend nil))
  "okaeri")

(defface okaeri-footer
  `((t :inherit default))
  "okaeri")

(defface okaeri-keybind
  `((t :inherit default
       :height 300
       :extend nil))
  "okaeri")


;; Functions and code proper

(defun okaeri-prepare-body (labels-and-props)
  "Accepts a list of the form '(STRING '(LIST-OF-PROPS-FOR-BUTTON) ...)
and returns: A property list containing strings for both colums, with
face applied, and their lenghts, and the length of the longest line, 
which is used to align all the lines to the center. "
  (cl-loop for label in labels-and-props by #'cddr
           and props in (cdr labels-and-props) by #'cddr
           with max-string-len = 0
           with max-line-len = 0
           with result = nil
           do
           (let* ((string (apply #'make-text-button
				 (okaeri-apply-face 'okaeri-body label)
				 nil
				 props))
                  (keybind (key-description (where-is-internal (plist-get props 'action) nil t)))
                  (keybind (okaeri-apply-face 'okaeri-keybind
                                                       (if (string= keybind "")
                                                           "none"
                                                         keybind)))
                  (string-len (shr-string-pixel-width string))
                  (keybind-len (shr-string-pixel-width keybind)))
             (setq max-string-len
                   (max max-string-len string-len)
                   max-line-len
                   (max max-line-len (okaeri-body-line-len string-len keybind-len)))
             (push (list 'string string
                         'string-len string-len
                         'keybind keybind
                         'keybind-len keybind-len)
                   result))
           finally
           (return (cl-values (nreverse result) max-line-len))))

(defun okaeri-apply-face (face string)
  "Apply face to string in a such a way that it works both with, and
without font-lock-mode active."
  (let ((face (or face 'default)))
    (propertize string 'face face 'font-lock-face face)))

(defun okaeri-body-line-len (len1 len2)
  "Calculate the total length of the line in body:
the first column (action), the second column (keybind),
and the separator between the two."
  (+ len1 len2 (shr-string-pixel-width okaeri-body-line-sep)))

(defun okaeri-calc-middle (string-or-pixel-width)
  "Calculate the number of pixels required for the prefix,
so that a string, or a hypothetical string of given width,
is centered horizontally in the current window."
  (/ (- (window-body-width nil t) (if (stringp string-or-pixel-width)
                                      (shr-string-pixel-width string-or-pixel-width)
                                    string-or-pixel-width))
     2))

(defun okaeri-insert-logo ()
  "Insert the centered image pointed to by path in okaeri-logo
into the current buffer."
  (let ((image (create-image okaeri-logo)))
    (insert (propertize " " 'display `(space . (:align-to (- center (0.5 . ,image))))))
    (insert-image image)
    (insert okaeri-logo-separator)))

(defun okaeri-insert-title ()
  "Insert the centered text in okaeri-title into the current buffer."
  (let* ((string (okaeri-apply-face 'okaeri-title okaeri-title))
         (align (okaeri-calc-middle string))
         (prefix (propertize " " 'display `(space . (:align-to (,align))))))
    (insert (propertize string 'line-prefix prefix 'indent-prefix prefix)))
  (insert okaeri-title-separator))

(defun okaeri-insert-body ()
  "Insert the propertized text returned by okaeri-prepare-body
centered in the current buffer. "
  (cl-multiple-value-bind (plists max-line-len)
      (okaeri-prepare-body okaeri-body)
    (cl-loop for plist in plists
             with align = (okaeri-calc-middle max-line-len)
             do
	     ;; When the number supplied :align-to is enclosed in parentheses,
	     ;; it gets interpreted as pixel width instead of character width,
	     ;; hence (,align) instead of ,align.
             (let ((prefix (propertize " " 'display `(space . (:align-to (,align)))))
                   (offset (+ align (- max-line-len (plist-get plist 'keybind-len)))))
               (insert (propertize (plist-get plist 'string)
                                   'line-prefix prefix
                                   'indent-prefix prefix))
               (insert (propertize " " 'display `(space . (:align-to (+ left (,offset))))))
               (insert (plist-get plist 'keybind))
               (insert "\n")))
    (insert okaeri-body-separator)))

(defun okaeri-insert-footer ()
  "Insert the centered text in okaeri-footer into the current buffer."
  (let* ((string (okaeri-apply-face 'okaeri-footer okaeri-footer))
         (align (okaeri-calc-middle string))
         (prefix (propertize " " 'display `(space . (:align-to (,align))))))
    (insert (propertize string 'line-prefix prefix 'indent-prefix prefix)))
  (insert okaeri-footer-separator))

(defun okaeri-close-window-if-file ()
  "Close the window with the okaeri buffer on startup if a file
was provided on the command line.

Can be hooked to either 'emacs-startup-hook, 'window-setup-hook,
or equivalent startup hook."
  ;; This is probably very sloppy, but I don't know what arguments
  ;; might be left in command-line-args other than the path to emacs
  ;; executable, and the provided files. Once I figure that out,
  ;; I might add additional conditions to make this more robust.
  (when (and okaeri-hide-when-file-on-cmdline
	     (>= (length command-line-args) 2))
    (delete-window (get-buffer-window okaeri-buffer))))
(add-hook 'window-setup-hook #'okaeri-close-window-if-file)

(defun okaeri-ensure-no-hl-line ()
  "Ensure that hl-line-mode is disabled in the current buffer."
  (hl-line-mode -1)
  (setq-local global-hl-line-mode nil))


;; This function is the main entry point. If you are
;; reading the code, I would recommend starting here.
(defun okaeri-reload ()
  "Create and fill the okaeri-buffer."
  (setq okaeri-buffer (get-buffer-create okaeri-buffer-name))
  (with-current-buffer okaeri-buffer
    (when (not (eq major-mode 'okaeri-mode))
      (okaeri-mode))
    (setq-local truncate-partial-width-windows nil
		truncate-lines t)
    (setq cursor-type nil)
    (okaeri-ensure-no-hl-line)
    (when (fboundp #'page-break-lines-mode)
      (page-break-lines-mode +1))
    (setq buffer-read-only nil)
    (erase-buffer)
    (when okaeri-logo
      (okaeri-insert-logo))
    (when okaeri-title
      (okaeri-insert-title))
    (when okaeri-body
      (okaeri-insert-body))
    (when okaeri-footer
      (okaeri-insert-footer))
    (setq buffer-read-only t)
    (current-buffer)))


;; For external use

(defun okaeri-init (&rest args)
  "Set up for okaeri-buffer to appear on startup,
and to adjust to any resizes of the window."
  (add-hook 'window-configuration-change-hook #'okaeri-reload)
  (add-hook 'window-size-change-functions #'okaeri-reload)
  (setq initial-buffer-choice #'okaeri-reload))

(defun okaeri-open ()
  "Open the okaeri buffer in window, and kill the other windows."
  (interactive)
  (delete-window)
  (switch-to-buffer (okaeri-reload)))


;; Convenience functions/macros for external use

(defmacro okaeri-button-wrapper (command &rest args)
  "Returns a function called okaeri-button-wrapper-<command>,
that accepts a button, and runs COMMAND with ARGS.
COMMAND has to be a string representing the name of a function."
  ;; Use defun instead of a lambda to make it possible to find
  ;; the keybinding in active map; lambdas cannot be used, because
  ;; (eq lambda1 lambda1) returns nil.
  `(defun ,(intern (concat "okaeri-button-wrapped-" command)) (&optional button)
       (interactive)
       (,(intern command) ,@args)))

(defmacro okaeri-link-props (help-echo command &rest command-args)
  "Convenience macro that returns the list of properties
to apply to the button in okaeri-body."
  `(list 'action (okaeri-button-wrapper ,command ,@command-args) 'help-echo ,help-echo))

(defun okaeri-prepend-icon (icon text &optional raise)
  "Add icon to text, optionally adjusing its vertical height by RAISE.
RAISE is -0.12 by default, because that's what works for me with the fonts
I'm using; I didn't make this package for other people to use.

If you are using this package for some reason, feel free to override this function;
I would recommend the :override advice. Or you can write your own."
  (concat (propertize icon 'display `(raise ,(or raise -0.12))) " " text))

(provide 'okaeri)
;;; okaeri.el ends here
