EMACS=emacs
EMACS_QUICK=$(EMACS) --quick --directory .

SOURCE_FILES=*.el
TEST_FILES=*-test.el
BYTE_CODE=*.elc

COMPILE=--eval "(setq byte-compile-error-on-warn t)" --batch --funcall batch-byte-compile-if-not-done $(SOURCE_FILES)
RUN_TESTS=--load $(TEST_FILES) --batch --funcall ert-run-tests-batch-and-exit

all: compile test-travis test

compile:
	$(EMACS_QUICK) $(COMPILE)

test:
	$(EMACS_QUICK) $(RUN_TESTS)

clean:
	rm -f $(BYTE_CODE)
