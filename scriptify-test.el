(require 'scriptify)

(ert-deftest shebang-present-p-empty-buffer ()
  (with-temp-buffer
    (should-not (scriptify--shebang-present-p))))

(ert-deftest shebang-present-p-non-empty-buffer-without-shebang ()
  (with-temp-buffer
    (insert "foo\nbar\nbaz\n")
    (should-not (scriptify--shebang-present-p))))

(ert-deftest shebang-present-p-buffer-with-shebang ()
  (with-temp-buffer
    (insert "#! /usr/bin/perl -w")
    (should (scriptify--shebang-present-p))))

(ert-deftest add-shebang-empty-buffer ()
  (with-temp-buffer
    (perl-mode)
    (should-not (scriptify--shebang-present-p))
    (scriptify--add-shebang)
    (should (scriptify--shebang-present-p))))

(ert-deftest add-shebang-non-empty-buffer ()
  (with-temp-buffer
    (insert "foo\nbar\nbaz\n")
    (perl-mode)
    (should-not (scriptify--shebang-present-p))
    (scriptify--add-shebang)
    (should (scriptify--shebang-present-p))))

(ert-deftest add-shebang-buffer-with-shebang ()
  (with-temp-buffer
    (insert "#! /usr/bin/perl -w")
    (perl-mode)
    (let ((content-before (buffer-string))
          content-after)
      (scriptify--add-shebang)
      (setq content-after (buffer-string))
      (should (string= content-before content-after)))))

(ert-deftest add-shebang-different-shebangs-in-different-modes ()
  (let ((scriptify-shebang-alist
         '((perl-mode . "#! /usr/bin/env perl")
           (js-mode . "#! /usr/bin/env node"))))
    (with-temp-buffer
      (perl-mode)
      (scriptify--add-shebang)
      (should (string= "#! /usr/bin/env perl\n"
                       (buffer-string))))
    (with-temp-buffer
      (js-mode)
      (scriptify--add-shebang)
      (should (string= "#! /usr/bin/env node\n"
                       (buffer-string))))))

(ert-deftest add-shebang-modes-with-no-valid-shebang ()
  (let (scriptify-shebang-alist)
    (with-temp-buffer
      (asm-mode)
      (let* ((err (should-error (scriptify--add-shebang)))
             (type (car err))
             (msg (cadr err)))
        (should (eq type 'error))
        (should (equal msg "No valid shebang for `asm-mode'"))))))

(ert-deftest add-shebang-function-in-alist ()
  (let ((scriptify-shebang-alist
         '((perl-mode . (lambda () (format "#! %s -w" "/opt/perl"))))))
    (with-temp-buffer
      (perl-mode)
      (scriptify--add-shebang)
      (should (string= "#! /opt/perl -w\n"
                       (buffer-string))))))

(ert-deftest shebang-for-major-mode ()
  (let ((scriptify-shebang-alist
         '((perl-mode . "#! /usr/bin/env perl")
           (js-mode . "#! /usr/bin/env node")
           (lisp-mode . (lambda ()
                          (format "#! %s --script"
                                  "/opt/bin/sbcl"))))))
    (should
     (equal (scriptify--shebang-for-major-mode 'perl-mode)
            "#! /usr/bin/env perl"))
    (should
     (equal (scriptify--shebang-for-major-mode 'js-mode)
            "#! /usr/bin/env node"))
    (should
     (equal (scriptify--shebang-for-major-mode 'lisp-mode)
            "#! /opt/bin/sbcl --script"))
    (should
     (equal (scriptify--shebang-for-major-mode 'asm-mode) nil))))

;; Local variables:
;; flycheck-mode: nil
;; End:
