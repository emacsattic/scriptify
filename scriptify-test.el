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

(ert-deftest new-basename-buffer-with-no-file ()
  (with-temp-buffer
    (should-not (buffer-file-name))

    (rename-buffer "foo")
    (should (string= (scriptify--new-basename) "foo"))

    (rename-buffer "bar.pl")
    (should (string= (scriptify--new-basename) "bar"))

    (rename-buffer "  *quux*  ")
    (should (string= (scriptify--new-basename) "quux"))))

(ert-deftest new-basename-buffer-associated-with-file ()
  (let* ((name "foo.rb")
         (path (expand-file-name name "~")))
    (should-not (file-exists-p path))

    (with-temp-buffer
      (rename-buffer name)
      (setq buffer-file-name path)

      (should (string= (scriptify--new-basename) "foo")))))

(ert-deftest new-dirname-no-default-scripts-directory ()
  (let ((default-directory "/tmp/")
        scriptify-scripts-directory)
    (with-temp-buffer
      (should (string= default-directory
                       (scriptify--new-dirname)))

      (setq buffer-file-name "/var/foo")

      (should (string= "/var/"
                       (scriptify--new-dirname))))))

(ert-deftest new-dirname-with-default-scripts-directory ()
  (let ((default-directory "/tmp/")
        (scriptify-scripts-directory "/var/"))
    (with-temp-buffer
      (should (string= scriptify-scripts-directory
                       (scriptify--new-dirname))))))

(ert-deftest check-scripts-directory ()
  (let (scriptify-scripts-directory)
    (scriptify--check-scripts-directory)

    (setq scriptify-scripts-directory "/")
    (scriptify--check-scripts-directory)

    (setq scriptify-scripts-directory 42)
    (should-error (scriptify--check-scripts-directory))

    (setq scriptify-scripts-directory "/non/existant/dir")
    (should-error (scriptify--check-scripts-directory))))
