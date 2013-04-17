;;; scriptify.el --- Make executable script from current buffer contents -*- lexical-binding: t -*-

;; Copyright (C) 2013 Victor Deryagin

;; Author: Victor Deryagin <vderyagin@gmail.com>
;; Created: 16 Apr 2013
;; Version: 0.1.0
;; Keywords: convenience files languages tools
;; X-URL: https://github.com/vderyagin/scriptify

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Make executable script out of current buffer.

;; * insert shebang
;; * chop off filename extension
;; * move to directory for scripts (optional)
;; * make executable.

;;; Code:

(defgroup scriptify nil
  "Make executable script out of current buffer contents."
  :prefix "scriptify-"
  :group 'convenience)

(defcustom scriptify-scripts-directory nil
  "Directory to put scripts in."
  :type '(choice (const :tag "None" nil)
                 (directory :must-match t))
  :group 'scriptify)

(defcustom scriptify-shebang-alist
  '((ruby-mode . "#! /usr/bin/env ruby")
    (perl-mode . "#! /usr/bin/env perl")
    (cperl-mode . "#! /usr/bin/env perl")
    (python-mode . "#! /usr/bin/env python")
    (php-mode . "#! /usr/bin/env php")
    (js-mode . "#! /usr/bin/env node")
    (js2-mode . "#! /usr/bin/env node")
    (js3-mode . "#! /usr/bin/env node")
    (lua-mode . "#! /usr/bin/env lua")
    (io-mode . "#! /usr/bin/env io")
    (groovy-mode . "#! /usr/bin/env groovy")
    (haskell-mode . "#! /usr/bin/env runhaskell")
    (scala-mode . "#! /usr/bin/env scala\n!#")
    (awk-mode . (lambda () (format "#! %s -f" (executable-find "awk"))))
    (lisp-mode . (lambda () (format "#! %s --script" (executable-find "sbcl"))))
    (emacs-lisp-mode . (lambda () (format "#! %s --script" (executable-find "emacs"))))
    (sh-mode . (lambda () (format "#! %s" (executable-find (symbol-name sh-shell))))))
  "Alist specifying associations between major modes and shebangs.
In each alist element car is major mode symbol, cdr is either a
string, representing shebang appropriate for given major mode, or
function that return one."
  :type '(alist :key-type symbol
                :value-type (choice string function))
  :group 'scriptify)

;;;###autoload
(defun scriptify ()
  "Make executable script out of current buffer.

* insert shebang according to `scriptify-shebang-alist'
* chop off filename extension
* move to `scriptify-scripts-directory' (if set)
* make executable."
  (interactive)
  (scriptify--add-shebang)
  (scriptify--move-file)
  (scriptify--make-file-executable (buffer-file-name)))

(defun scriptify--add-shebang ()
  "Add shebang to the beginning of the current buffer if it is not already there."
  (unless (scriptify--shebang-present-p)
    (let ((shebang (scriptify--shebang-for-major-mode major-mode)))
      (unless shebang
        (error "No valid shebang for `%s'" major-mode))
      (save-excursion
        (goto-char (point-min))
        (insert shebang "\n")))))

(defun scriptify--shebang-present-p ()
  "Return t if current buffer includes shebang, nil otherwise."
  (and
   (> (point-max) 3)
   (string= "#!" (buffer-substring-no-properties 1 3))))

(defun scriptify--shebang-for-major-mode (mode)
  "Return shebang appropriate for major mode MODE as a string, nil if not found."
  (let ((shebang (cdr (assoc mode scriptify-shebang-alist))))
    (when (functionp shebang)
      (setq shebang (funcall shebang)))
    (when (stringp shebang)
      shebang)))

(defun scriptify--move-file ()
  "Move script where it's supposed to be."
  (scriptify--check-scripts-directory)

  (let ((old-location (buffer-file-name))
        (new-location (scriptify--new-full-name)))
    (if (equal old-location new-location)
        (save-buffer)
      (when (file-exists-p new-location)
        (error "Script '%s' already exists" new-location))
      (if (and old-location (file-exists-p old-location))
          (rename-file old-location new-location)
        (write-file new-location)))))

(defun scriptify--new-basename ()
  "Return basename for script."
  (replace-regexp-in-string
   (rx (or (and string-start (+ (or "*" whitespace)))
           (and (+ (or "*" whitespace)) string-end)))
   ""
   (file-name-base (buffer-name))))

(defun scriptify--new-dirname ()
  "Return directory in which to put script."
  (file-name-as-directory
   (or scriptify-scripts-directory
       (and (buffer-file-name)
            (file-name-directory (buffer-file-name)))
       default-directory)))

(defun scriptify--new-full-name ()
  "Return new full name of script."
  (expand-file-name (scriptify--new-basename)
                    (scriptify--new-dirname)))

(defun scriptify--make-file-executable (filename)
  "Make file FILENAME executable."
  (let* ((file (expand-file-name filename))
         (modes (file-modes file))
         (new-modes (file-modes-symbolic-to-number "u+x" modes)))
    (set-file-modes file new-modes)))

(defun scriptify--check-scripts-directory ()
  "Check if `scriptify-scripts-directory' is valid.
Raises error if it is neither nil nor valid directory on
filesystem."
  (let ((dir scriptify-scripts-directory))
    (when (and dir (not (file-directory-p dir)))
      (error "'%s' is not a valid directory" dir))))

(provide 'scriptify)

;;; scriptify.el ends here
