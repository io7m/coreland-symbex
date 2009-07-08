# auto generated - do not edit

default: all

all:\
UNIT_TESTS/lexer UNIT_TESTS/lexer.ali UNIT_TESTS/lexer.o symbex-lex.ali \
symbex-lex.o symbex-parse.ali symbex-parse.o symbex.a symbex.ali symbex.o

UNIT_TESTS/lexer:\
ada-bind ada-link UNIT_TESTS/lexer.ald UNIT_TESTS/lexer.ali symbex.ali \
symbex-lex.ali
	./ada-bind UNIT_TESTS/lexer.ali
	./ada-link UNIT_TESTS/lexer UNIT_TESTS/lexer.ali

UNIT_TESTS/lexer.ali:\
ada-compile UNIT_TESTS/lexer.adb symbex-lex.ali
	./ada-compile UNIT_TESTS/lexer.adb

UNIT_TESTS/lexer.o:\
UNIT_TESTS/lexer.ali

ada-bind:\
conf-adabind conf-systype conf-adatype conf-adabflags conf-adafflist flags-cwd

ada-compile:\
conf-adacomp conf-adatype conf-systype conf-adacflags conf-adafflist flags-cwd

ada-link:\
conf-adalink conf-adatype conf-systype conf-adaldflags

ada-srcmap:\
conf-adacomp conf-adatype conf-systype

ada-srcmap-all:\
ada-srcmap conf-adacomp conf-adatype conf-systype

cc-compile:\
conf-cc conf-cctype conf-systype

cc-link:\
conf-ld conf-ldtype conf-systype

cc-slib:\
conf-systype

conf-adatype:\
mk-adatype
	./mk-adatype > conf-adatype.tmp && mv conf-adatype.tmp conf-adatype

conf-cctype:\
conf-cc mk-cctype
	./mk-cctype > conf-cctype.tmp && mv conf-cctype.tmp conf-cctype

conf-ldtype:\
conf-ld mk-ldtype
	./mk-ldtype > conf-ldtype.tmp && mv conf-ldtype.tmp conf-ldtype

conf-systype:\
mk-systype
	./mk-systype > conf-systype.tmp && mv conf-systype.tmp conf-systype

mk-adatype:\
conf-adacomp conf-systype

mk-cctype:\
conf-cc conf-systype

mk-ctxt:\
mk-mk-ctxt
	./mk-mk-ctxt

mk-ldtype:\
conf-ld conf-systype conf-cctype

mk-mk-ctxt:\
conf-cc conf-ld

mk-systype:\
conf-cc conf-ld

symbex-lex.ads:\
symbex.ali

symbex-lex.ali:\
ada-compile symbex-lex.adb symbex.ali symbex-lex.ads
	./ada-compile symbex-lex.adb

symbex-lex.o:\
symbex-lex.ali

symbex-parse.ads:\
symbex.ali symbex-lex.ali

symbex-parse.ali:\
ada-compile symbex-parse.adb symbex.ali symbex-parse.ads
	./ada-compile symbex-parse.adb

symbex-parse.o:\
symbex-parse.ali

symbex.a:\
cc-slib symbex.sld symbex-lex.o symbex-parse.o symbex.o
	./cc-slib symbex symbex-lex.o symbex-parse.o symbex.o

symbex.ali:\
ada-compile symbex.ads symbex.ads
	./ada-compile symbex.ads

symbex.o:\
symbex.ali

clean-all: obj_clean ext_clean
clean: obj_clean
obj_clean:
	rm -f UNIT_TESTS/lexer UNIT_TESTS/lexer.ali UNIT_TESTS/lexer.o symbex-lex.ali \
	symbex-lex.o symbex-parse.ali symbex-parse.o symbex.a symbex.ali symbex.o
ext_clean:
	rm -f conf-adatype conf-cctype conf-ldtype conf-systype mk-ctxt

regen:\
ada-srcmap ada-srcmap-all
	./ada-srcmap-all
	cpj-genmk > Makefile.tmp && mv Makefile.tmp Makefile
