EMACS ?= emacs
PACKAGE := meeting-mode
PACKAGE_FILE := $(PACKAGE).el
TEST_FILE := test/$(PACKAGE)-tests.el
ELPA_DIR := .elpa
PACKAGE_LINT_STAMP := $(ELPA_DIR)/.package-lint-installed

.PHONY: all test compile lint checkdoc check clean

all: check lint checkdoc compile test

check:
	@if grep -nE '(leader-map|define-key|global-set-key|local-set-key)' $(PACKAGE_FILE); then \
	  echo "Unexpected configuration-specific code in $(PACKAGE_FILE)"; \
	  exit 1; \
	fi

test:
	$(EMACS) -Q --batch -L . -l $(TEST_FILE) \
	  -f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -Q --batch -L . \
	  --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(PACKAGE_FILE)

lint: $(PACKAGE_LINT_STAMP)
	$(EMACS) -Q --batch \
	  --eval '(progn (require (quote package)) (setq package-user-dir (expand-file-name "$(ELPA_DIR)")) (package-initialize) (require (quote package-lint)) (setq package-lint-batch-fail-on-warnings t))' \
	  -f package-lint-batch-and-exit $(PACKAGE_FILE)

checkdoc:
	$(EMACS) -Q --batch -L . \
	  --eval '(progn (require (quote checkdoc)) (checkdoc-file "$(PACKAGE_FILE)"))'

$(PACKAGE_LINT_STAMP):
	mkdir -p $(ELPA_DIR)
	$(EMACS) -Q --batch \
	  --eval '(progn (require (quote package)) (setq package-user-dir (expand-file-name "$(ELPA_DIR)") package-archives (quote (("melpa" . "https://melpa.org/packages/")))) (package-initialize) (package-refresh-contents) (package-install (quote package-lint)))'
	@touch $@

clean:
	rm -f *.elc
	rm -rf $(ELPA_DIR)
