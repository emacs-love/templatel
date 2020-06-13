.POSIX:
EMACS = emacs

SRC = templatel.el
TESTSRC = templatel-tests.el

OBJ = templatel.elc
TESTOBJ = templatel-tests.elc

ALLOBJS = $(OBJ) $(TESTOBJ)

all: $(ALLOBJS)

$(OBJ): $(SRC)
$(TESTOBJ): $(OBJ) $(TESTSRC)

clean:; rm -f $(ALLOBJS)

check: $(TESTOBJ)
	$(EMACS) -batch -Q -L . -l $(TESTOBJ) -f ert-run-tests-batch

.SUFFIXES: .el .elc
.el.elc:
	$(EMACS) -batch -Q -L . -f batch-byte-compile $<
