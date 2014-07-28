;;; vim-modeline.el --- Vim modeline support

;; Copyright (C) 2012  Seong-Kook Shin

;; Author: Seong-Kook Shin <cinsky@gmail.com>
;; Keywords: emulations, matching

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; VIM (includes vi and ex) uses its own per-file configuration called
;; 'modeline', which is almost similar to the file local variables in Emacs.


;;; Code:

(eval-when-compile (require 'cl))

(defgroup vim-modeline nil
  "Facilities for setting file options based on vim modeline.")

(defcustom vim-modeline/modelines 5
  "Default number of lines for searching modelines in the
  beginning of the buffer, and the end of the buffer."
  :type 'integer
  :group 'vim-modeline)

(defcustom vim-modeline/options-alist
  '(("shiftwidth" . vim-modeline/shiftwidth)
    ("sw" . vim-modeline/shiftwidth)
    ("textwidth" . vim-modeline/textwidth)
    ("tw" . vim-modeline/textwidth)
    ("tabstop" . vim-modeline/tabstop)
    ("ts" . vim-modeline/tabstop)
    ("softtabstop" . vim-modeline/tabstop)
    ("sts" . vim-modeline/tabstop)
    ("number" . vim-modeline/number)
    ("nu" . vim-modeline/number)
    ("nonumber" . vim-modeline/nonumber)
    ("nonu" . vim-modeline/nonumber)
    ("expandtab" . vim-modeline/expandtab)
    ("et" . vim-modeline/expandtab)
    ("noexpandtab" . vim-modeline/expandtab)
    ("noet" . vim-modeline/expandtab))
  "Alist of VIM option names and handlers"
  :type '(alist :key-type (string :tag "Option") :value-type (function :tag "Handler function"))
  :group 'vim-modeline)

;;;
;;; From the VIM help messages
;;;
;;; There are two forms of modelines.  The first form:
;;;     [text]{white}{vi:|vim:|ex:}[white]{options}
;;;
;;; [text]              any text or empty
;;; {white}             at least one blank character (<Space> or <Tab>)
;;; {vi:|vim:|ex:}      the string "vi:", "vim:" or "ex:"
;;; [white]             optional white space
;;; {options}   a list of option settings, separated with white space or ':',
;;;             where each part between ':' is the argument for a ":set"
;;;             command (can be empty)
;;;
;;; Example:
;;;    vi:noai:sw=3 ts=6
;;;
;;; The second form (this is compatible with some versions of Vi):
;;;
;;;     [text]{white}{vi:|vim:|ex:}[white]se[t] {options}:[text]
;;;
;;; [text]              any text or empty
;;; {white}             at least one blank character (<Space> or <Tab>)
;;; {vi:|vim:|ex:}      the string "vi:", "vim:" or "ex:"
;;; [white]             optional white space
;;; se[t]               the string "set " or "se " (note the space)
;;; {options}   a list of options, separated with white space, which is the
;;;             argument for a ":set" command
;;; :           a colon
;;; [text]              any text or empty
;;;
;;; Example:
;;;    /* vim: set ai tw=75: */
;;;
;;; The white space before {vi:|vim:|ex:} is required.  This minimizes
;;; the chance that a normal word like "lex:" is caught.  There is one
;;; exception: "vi:" and "vim:" can also be at the start of the line
;;; (for compatibility with version 3.0).  Using "ex:" at the start of
;;; the line will be ignored (this could be short for "example:").

(defun vim-modeline/split-options (input &optional delims)
"Split INPUT into substrings bounded by matches for DELIMS.

DELIMS is a string that contains one or more delimiter
characters.  If a delimiter character is followed by a backslash
character, it is interpreted as a normal character."
  (let ((delims (append delims nil))
        (len (length input)) (idx 0) (start 0)
        (prev 0) result)
    (while (< idx len)
      (when (and (memq (aref input idx) delims)
                 (not (eq prev ?\\)))
        (if (< start idx)
            (push (substring input start idx) result))
        (setq start (1+ idx)))
      (setq prev (aref input idx))
      (setq idx (1+ idx)))
    (if (< start idx)
        (push (substring input start idx) result))
    (nreverse result)))

(defun vim-modeline/parse-options (options &optional alist)
  "Return ALIST of VIM options.

OPTIONS is a list of string where each element has either
\"NAME=VALUE\" or \"NAME\"."
  (dolist (pairs options)
    (when (string-match "\\`\\([^=]+\\)\\(:?=\\(.*\\)\\)?\\'" pairs)
      (let ((key (match-string 1 pairs))
            (val (match-string 3 pairs)))
        (let ((elem (assoc key alist)))
          (if elem
              (setcdr elem val)
            (push (cons key val) alist))))))
  alist)

;;; TODO: change below function to use buffer contents with limit.
(defun vim-modeline/extract-option-line (&optional bound)
  "Extract the {options} part of the VIM modeline in the given line.

If the buffer contains the modeline, this function returns a
form (delims . options), where DELIMS is a string that contains
one or more delimiter characters for options, and OPTIONS is a
string that contains VIM \"{options}\" part.

Otherwise this function returns nil."
  (let ((pos (point)) result)
    (when (re-search-forward
           "[[:blank:]]+\\(?:vi:\\|vim:\\|ex:\\)[[:blank:]]?set? " bound t)
      ;; INPUT matches for the second form of modeline.
      (let ((start (match-end 0))
            (end (line-end-position)))
        (goto-char (1- start))
        (when (re-search-forward "[^\\]:" end t)
          (setq result (cons " " (buffer-substring-no-properties
                                  start (1- (match-end 0))))))))
    (when (not result)
      (goto-char pos)
      (when (re-search-forward "[[:blank:]]+\\(vi:\\|vim:\\|ex:\\)[[:blank:]]?"
                               bound t)
        ;; INPUT matches for the first form of modeline.
        (setq result (cons " :" (buffer-substring-no-properties
                                 (match-end 0) (line-end-position))))))
    result))

(defun vim-modeline/extract-option-line-deprecated (&optional limit)
  "Extract the {options} part of the VIM modeline in the given line.

If the bufferINPUT contains the modeline, this function returns a
form (delims . options), where DELIMS is a string that contains
one or more delimiter characters for options, and OPTIONS is a
string that contains \"{options}\" part.

Otherwise this function returns nil."
  (let (result)
    (when (string-match "[[:blank:]]+\\(?:vi:\\|vim:\\|ex:\\)[[:blank:]]?set? "
                        input)
      ;; INPUT matches for the second form of modeline.
      (let ((idx (match-end 0))
            (len (length input))
            (prev 0)
            start end)
        (setq start idx
              end (catch 'vim-modeline/parse/found
                    (while (< idx len)
                      (if (and (eq (aref input idx) ?\:)
                               (not (eq prev ?\\)))
                          (throw 'vim-modeline/parse/found idx))
                      (setq prev (aref input idx)
                            idx (1+ idx)))))
        (if end
            (setq result (cons " " (substring input start end)))
          (lwarn :vim-modeline :warning
                 "wrong/suspicious modeline syntax detected"))))
    (when (and (not result)
               (string-match "[[:blank:]]+\\(vi:\\|vim:\\|ex:\\)[[:blank:]]?"
                             input))
      ;; INPUT matches for the first form of modeline.
      (setq result (cons " :" (substring input (match-end 0)))))
    result))

(defun vim-modeline/get ()
  "Get the alist of VIM-style file local variables (a.k.a. VIM modelines)"
  (let (input result)
    (save-excursion
      (save-restriction
        (save-match-data
          (goto-char (point-min))
          (forward-line vim-modeline/modelines)
          (let ((bound (point)))
            (goto-char (point-min))
            (setq input (vim-modeline/extract-option-line bound))
            (if input
                (setq result
                      (append result
                              (vim-modeline/split-options (cdr input)
                                                          (car input))))))
          (goto-char (point-max))
          (forward-line (- vim-modeline/modelines))
          (setq input (vim-modeline/extract-option-line nil))
          (if input
              (setq result
                    (append result
                            (vim-modeline/split-options (cdr input)
                                                        (car input))))))))
    (vim-modeline/parse-options result)))

;;;###autoload
(defun vim-modeline/do ()
  (let ((options (vim-modeline/get)))
    (dolist (opt options)
      (let* ((name (car opt)) (value (cdr opt))
             (handler (cdr (assoc name vim-modeline/options-alist))))
        (and handler
             (funcall handler name value options))))))


(defun vim-modeline/tabstop (name &optional value options)
  (let ((offset (string-to-number value)))
    (when (or (> offset 0) (< offset 40))
      (message "vim-modeline: set tabstop to %S" offset)
      (cond ((or (string-equal name "tabstop")
                 (string-equal name "ts"))
             ;; `tab-width' is automatically buffer-local
             (unless (or (assoc "sts" options)
                         (assoc "smarttab" options))
               ;; I don't know how to implement VIM smarttab in Emacs.
               ;;
               ;; It seems that if both 'softtabstop' and 'tabstop' are defined,
               ;; VIM tries to use 'softtabstop' value than 'tabstop'.
               ;; Thus, if OPTIONS have 'softtabstop', do nothing.
               (setq tab-width offset)))
            ((or (string-equal name "softtabstop")
                 (string-equal name "sts"))
             (setq tab-width offset))))))


(defun vim-modeline/shiftwidth (name &optional value options)
  (let ((offset (string-to-number value)))
    (when (or (> offset 0) (< offset 40))
      (message "vim-modeline: set shiftwidth to %S" offset)
      (cond (;; For CC-mode related
             (local-variable-p 'c-basic-offset)
             (setq c-basic-offset offset))
            (;; For LISP-related
             (memq major-mode '(emacs-lisp-mode
                                lisp-mode
                                lisp-interaction-mode))
             (make-local-variable 'lisp-indent-offset)
             (setq lisp-indent-offset offset))
            (;; For python mode
             (memq major-mode '(python-mode))
             (make-local-variable 'python-indent)
             (setq python-indent offset))
            (;; For lua
             (memq major-mode '(lua-mode))
             (make-local-variable 'lua-indent-level)
             (setq lua-indent-level offset))
            (;; For HTML
             (memq major-mode '(html-mode))
             (make-local-variable 'sgml-basic-offset)
             (setq sgml-basic-offset offset))
            (t
             (lwarn :vim-modeline :warning
                    "shiftwidth for %S is not supported." major-mode))))))


(defun vim-modeline/textwidth (name &optional value options)
  (let ((width (string-to-number value)))
    (when (or (> width 20))
      (message "vim-modeline: set textwidth to %S" width)
      (setq fill-column width))))


(defun vim-modeline/number (name &optional value options)
  (if (fboundp 'linum-mode)
      (linum-mode 1)
    (lwarn :vim-modeline :warning
           "number option requested but linum-mode is not installed")))

(defun vim-modeline/nonumber (name &optional value options)
  (if (fboundp 'linum-mode)
      (linum-mode 0)))

(defun vim-modeline/expandtab (name &optional value options)
  (setq indent-tabs-mode nil))

(defun vim-modeline/noexpandtab (name &optional value options)
  (setq indent-tabs-mode t))


(provide 'vim-modeline)
;;; vim-modeline.el ends here
