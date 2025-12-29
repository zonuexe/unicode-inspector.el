EMACS ?= emacs
EASK ?= eask

all: autoloads install compile lint

install:
	$(EASK) install

compile:
	$(EASK) compile

autoloads:
	$(EASK) generate autoloads

clean:
	$(EASK) clean all

test: clean all
	$(EASK) test ert ./unicode-inspector-test.el

lint: checkdoc check-declare

checkdoc:
	$(EASK) lint checkdoc

check-declare:
	$(EASK) lint declare

.PHONY: all autoloads checkdoc check-declare clean compile install lint test
