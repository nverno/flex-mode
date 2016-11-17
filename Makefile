emacs      ?= emacs
wget       ?= wget

SRC        = 
FSRC       = test/test.flex
LIB        = -lfl
CPPINCLUDE = -I.
CFLAGS     = -g -Wall -Wno-unused -Wno-write-strings ${CPPINCLUDE}
CGEN       = ${SRC:.flex=-lex.cc}
FFLAGS     = -d -o${CGEN}
OUTPUT     = test/test.output

CC         = g++
flex       = flex ${FFLAGS}

.PHONY: clean distclean test
all: test
test: 
	$(emacs) -Q -batch -L . -l ert -l test/flex-tests.el \
	-f ert-run-tests-batch-and-exit

lexer: ${SRC} ${CGEN}
	${CC} ${CFLAGS} $^ ${LIB} -o $@

${CGEN}: ${FSRC}
	${flex} ${FSRC}

README.md: el2markdown.el flex-mode.el
	$(emacs) -batch -l $< $(el) -f el2markdown-write-readme
	$(RM) $@~

.INTERMEDIATE: el2markdown.el
el2markdown.el:
	$(wget) -q -O $@ "https://github.com/Lindydancer/el2markdown/raw/master/el2markdown.el"

clean:
	$(RM) *~ ${CGEN} ${lexer} *.o *.exe *.out *.output

distclean: clean
	$(RM) *autoloads.el *loaddefs.el TAGS *.elc
