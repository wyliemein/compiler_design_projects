######################################################

COMPILER=boa
EXT=boa

ZIPFILE=$(COMPILER).zip
ZIPCONTENTS=$(COMPILER).cabal LICENSE Makefile stack.yaml bin c-bits lib tests limit.sh

######################################################

STACK=stack --allow-different-user
STACKEXEC=$(STACK) exec --
COMPILEREXEC=$(STACKEXEC) $(COMPILER)# +RTS -M500M -RTS
GHCICOMMAND=$(STACKEXEC) ghci

# Max 5 seconds, 500 MB of memory and 50 MB of file
LIMIT=./limit.sh 5 500 50

UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
  FORMAT=elf32
else
ifeq ($(UNAME), Darwin)
  FORMAT=macho
endif
endif

.PHONY: all test build clean distclean tags zip ghci ghcid

all: test

test: clean
	$(STACK) test --test-arguments="--num-threads 1"

build:
	$(STACK) build

tests/output/%.anf: tests/input/%.$(EXT)
	$(LIMIT) $(STACKEXEC) anf $< > $@

tests/output/%.result: tests/output/%.run
	$(LIMIT) $< > $@

tests/output/%.run: tests/output/%.o c-bits/main.c
	clang -g -m32 -o $@ c-bits/main.c $<

tests/output/%.o: tests/output/%.s
	nasm -f $(FORMAT) -o $@ $<

tests/output/%.s: tests/input/%.$(EXT)
	$(LIMIT) $(COMPILEREXEC) $< > $@

clean:
	rm -rf tests/output/*.o tests/output/*.s tests/output/*.dSYM tests/output/*.run tests/output/*.log tests/output/*.result tests/output/*.anf

distclean: clean
	$(STACK) clean

tags:
	hasktags -x -c lib/

zip:
	rm -f $(ZIPFILE)
	zip -r $(ZIPFILE) $(ZIPCONTENTS) -x '*/\.*' -x@.gitignore

ghci:
	$(GHCICOMMAND)

ghcid:
	ghcid --command="$(GHCICOMMAND)"
