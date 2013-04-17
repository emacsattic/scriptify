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

;; Local variables:
;; flycheck-mode: nil
;; End:
