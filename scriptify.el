;;; scriptify.el --- Make executable script from current buffer contents -*- lexical-binding: t -*-

;; Copyright (C) 2013 Victor Deryagin

;; Author: Victor Deryagin <vderyagin@gmail.com>
;; Created: 16 Apr 2013
;; Version: 0.0.1
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

;; M-x scriptify does the following:
;; - insert shebang in the beginning of the buffer (if it is not already there)
;; - chop off extension (if any) of file
;; - move file to special directory for scripts (optional)
;; - set executable bit on

;;; Code:

(defvar scriptify-shebang-alist
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
In each alist elemet car is major mode symbol, cdr is either a
string, representing shebang appropriate for given major mode, or
function that return one.")

(defvar scriptify-scripts-directory nil
  "Directory to put scripts in.")

(defun scriptify ()
  "Perform the thing."
  (interactive)
  (scriptify--insert-shebang)
  (scriptify--move-file)
  (scriptify--make-file-executable))

(defun scriptify--insert-shebang ()
  "Insert shebang in current buffer if it is not already there.
Raise error when there is no known valid shebang for current `major-mode'."
  (unless (scriptify--shebang-p (current-buffer))
    (let ((shebang (cdr (assoc major-mode scriptify-shebang-alist))))
      (unless shebang
        (error "Don't know what valid shebang for `%s' is" (symbol-name major-mode)))
      (save-excursion
        (when (functionp shebang)
          (setq shebang (funcall shebang)))
        (goto-char 1)
        (insert shebang "\n")))))

(defun scriptify--shebang-p (buffer)
  "Return t when BUFFER already has shebang, nil otherwise."
  (with-current-buffer buffer
    (and
     (> (point-max) 3)                  ; more then 2 chars in buffer
     (string= "#!" (buffer-substring-no-properties 1 3)))))

(defun scriptify--move-file ()
  "Chop off extension and move file to `scriptify-scripts-directory' if specified."
  (when (and scriptify-scripts-directory
             (not (file-directory-p scriptify-scripts-directory)))
    (error "'%s' is not a valid directory" scriptify-scripts-directory))
  (let* ((current-path (buffer-file-name))
         (dir (expand-file-name (or scriptify-scripts-directory default-directory)))
         (file (file-name-base (or current-path (buffer-name))))
         (new-path (expand-file-name file dir)))
    (if (equal current-path new-path)
        (save-buffer)
      (when (file-exists-p new-path)
        (error "Script '%s' already exists" file))
      (if (and current-path (file-exists-p current-path))
          (rename-file current-path new-path)
        (write-file new-path)))))

(defun scriptify--make-file-executable ()
  "Make currently opened file executable."
  (let* ((file (buffer-file-name))
         (modes (file-modes file))
         (new-modes (file-modes-symbolic-to-number "u+x" modes)))
    (set-file-modes file new-modes)))

(provide 'scriptify)

;;; scriptify.el ends here
