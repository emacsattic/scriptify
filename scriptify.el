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

(defun scriptify--add-shebang ()
  "Add shebang to the beginning of the current buffer if it is not already there."
  (unless (scriptify--shebang-present-p)
    (let ((shebang
           (cdr (assoc major-mode scriptify-shebang-alist))))
      (when (functionp shebang)
        (setq shebang (funcall shebang)))
      (unless (stringp shebang)
        (error "No valid shebang for `%s'" major-mode))
      (save-excursion
        (goto-char (point-min))
        (insert shebang "\n")))))

(defun scriptify--shebang-present-p ()
  "Return t if current buffer includes shebang, nil otherwise."
  (and
   (> (point-max) 3)
   (string= "#!" (buffer-substring-no-properties 1 3))))

(provide 'scriptify)

;;; scriptify.el ends here
