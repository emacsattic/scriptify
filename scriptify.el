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

(defun scriptify--shebang-present-p ()
  "Return t if current buffer includes shebang, nil otherwise."
  (and
   (> (point-max) 3)
   (string= "#!" (buffer-substring-no-properties 1 3))))

(provide 'scriptify)

;;; scriptify.el ends here
