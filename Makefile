# auto generated - do not edit

default: all

all:\
UNIT_TESTS/dump UNIT_TESTS/dump.ali UNIT_TESTS/dump.o UNIT_TESTS/lexer \
UNIT_TESTS/lexer.ali UNIT_TESTS/lexer.o UNIT_TESTS/parser UNIT_TESTS/parser.ali \
UNIT_TESTS/parser.o ctxt/bindir.o ctxt/ctxt.a ctxt/dlibdir.o ctxt/fakeroot.o \
ctxt/incdir.o ctxt/repos.o ctxt/slibdir.o ctxt/version.o deinstaller \
deinstaller.o install-core.o install-error.o install-posix.o install-win32.o \
install.a installer installer.o instchk instchk.o insthier.o symbex-conf \
symbex-conf.o symbex-lex.ali symbex-lex.o symbex-parse.ali symbex-parse.o \
symbex-walk.ali symbex-walk.o symbex.a symbex.ali symbex.o

# Mkf-deinstall
deinstall: deinstaller conf-sosuffix
	./deinstaller
deinstall-dryrun: deinstaller conf-sosuffix
	./deinstaller dryrun

# Mkf-install
install: installer postinstall conf-sosuffix
	./installer
	./postinstall

install-dryrun: installer conf-sosuffix
	./installer dryrun

# Mkf-instchk
install-check: instchk conf-sosuffix
	./instchk

# Mkf-test
tests:
	(cd UNIT_TESTS && make)
tests_clean:
	(cd UNIT_TESTS && make clean)

# -- SYSDEPS start
flags-stack-ada:
	@echo SYSDEPS stack-ada-flags run create flags-stack-ada 
	@(cd SYSDEPS/modules/stack-ada-flags && ./run)
libs-stack-ada-S:
	@echo SYSDEPS stack-ada-libs-S run create libs-stack-ada-S 
	@(cd SYSDEPS/modules/stack-ada-libs-S && ./run)
_sysinfo.h:
	@echo SYSDEPS sysinfo run create _sysinfo.h 
	@(cd SYSDEPS/modules/sysinfo && ./run)


stack-ada-flags_clean:
	@echo SYSDEPS stack-ada-flags clean flags-stack-ada 
	@(cd SYSDEPS/modules/stack-ada-flags && ./clean)
stack-ada-libs-S_clean:
	@echo SYSDEPS stack-ada-libs-S clean libs-stack-ada-S 
	@(cd SYSDEPS/modules/stack-ada-libs-S && ./clean)
sysinfo_clean:
	@echo SYSDEPS sysinfo clean _sysinfo.h 
	@(cd SYSDEPS/modules/sysinfo && ./clean)


sysdeps_clean:\
stack-ada-flags_clean \
stack-ada-libs-S_clean \
sysinfo_clean \


# -- SYSDEPS end


UNIT_TESTS/dump:\
ada-bind ada-link UNIT_TESTS/dump.ald UNIT_TESTS/dump.ali symbex.ali \
symbex-lex.ali symbex-parse.ali symbex-walk.ali
	./ada-bind UNIT_TESTS/dump.ali
	./ada-link UNIT_TESTS/dump UNIT_TESTS/dump.ali

UNIT_TESTS/dump.ali:\
ada-compile UNIT_TESTS/dump.adb symbex-lex.ali symbex-parse.ali symbex-walk.ali
	./ada-compile UNIT_TESTS/dump.adb

UNIT_TESTS/dump.o:\
UNIT_TESTS/dump.ali

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

UNIT_TESTS/parser:\
ada-bind ada-link UNIT_TESTS/parser.ald UNIT_TESTS/parser.ali symbex.ali \
symbex-lex.ali symbex-parse.ali
	./ada-bind UNIT_TESTS/parser.ali
	./ada-link UNIT_TESTS/parser UNIT_TESTS/parser.ali

UNIT_TESTS/parser.ali:\
ada-compile UNIT_TESTS/parser.adb symbex-lex.ali symbex-parse.ali
	./ada-compile UNIT_TESTS/parser.adb

UNIT_TESTS/parser.o:\
UNIT_TESTS/parser.ali

ada-bind:\
conf-adabind conf-systype conf-adatype conf-adabflags conf-adafflist flags-cwd \
	flags-stack-ada

ada-compile:\
conf-adacomp conf-adatype conf-systype conf-adacflags conf-adafflist flags-cwd \
	flags-stack-ada

ada-link:\
conf-adalink conf-adatype conf-systype conf-adaldflags conf-aldfflist \
	libs-stack-ada-S

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

conf-sosuffix:\
mk-sosuffix
	./mk-sosuffix > conf-sosuffix.tmp && mv conf-sosuffix.tmp conf-sosuffix

conf-systype:\
mk-systype
	./mk-systype > conf-systype.tmp && mv conf-systype.tmp conf-systype

# ctxt/bindir.c.mff
ctxt/bindir.c: mk-ctxt conf-bindir
	rm -f ctxt/bindir.c
	./mk-ctxt ctxt_bindir < conf-bindir > ctxt/bindir.c

ctxt/bindir.o:\
cc-compile ctxt/bindir.c
	./cc-compile ctxt/bindir.c

ctxt/ctxt.a:\
cc-slib ctxt/ctxt.sld ctxt/bindir.o ctxt/dlibdir.o ctxt/fakeroot.o \
ctxt/incdir.o ctxt/repos.o ctxt/slibdir.o ctxt/version.o
	./cc-slib ctxt/ctxt ctxt/bindir.o ctxt/dlibdir.o ctxt/fakeroot.o ctxt/incdir.o \
	ctxt/repos.o ctxt/slibdir.o ctxt/version.o

# ctxt/dlibdir.c.mff
ctxt/dlibdir.c: mk-ctxt conf-dlibdir
	rm -f ctxt/dlibdir.c
	./mk-ctxt ctxt_dlibdir < conf-dlibdir > ctxt/dlibdir.c

ctxt/dlibdir.o:\
cc-compile ctxt/dlibdir.c
	./cc-compile ctxt/dlibdir.c

# ctxt/fakeroot.c.mff
ctxt/fakeroot.c: mk-ctxt conf-fakeroot
	rm -f ctxt/fakeroot.c
	./mk-ctxt ctxt_fakeroot < conf-fakeroot > ctxt/fakeroot.c

ctxt/fakeroot.o:\
cc-compile ctxt/fakeroot.c
	./cc-compile ctxt/fakeroot.c

# ctxt/incdir.c.mff
ctxt/incdir.c: mk-ctxt conf-incdir
	rm -f ctxt/incdir.c
	./mk-ctxt ctxt_incdir < conf-incdir > ctxt/incdir.c

ctxt/incdir.o:\
cc-compile ctxt/incdir.c
	./cc-compile ctxt/incdir.c

# ctxt/repos.c.mff
ctxt/repos.c: mk-ctxt conf-repos
	rm -f ctxt/repos.c
	./mk-ctxt ctxt_repos < conf-repos > ctxt/repos.c

ctxt/repos.o:\
cc-compile ctxt/repos.c
	./cc-compile ctxt/repos.c

# ctxt/slibdir.c.mff
ctxt/slibdir.c: mk-ctxt conf-slibdir
	rm -f ctxt/slibdir.c
	./mk-ctxt ctxt_slibdir < conf-slibdir > ctxt/slibdir.c

ctxt/slibdir.o:\
cc-compile ctxt/slibdir.c
	./cc-compile ctxt/slibdir.c

# ctxt/version.c.mff
ctxt/version.c: mk-ctxt VERSION
	rm -f ctxt/version.c
	./mk-ctxt ctxt_version < VERSION > ctxt/version.c

ctxt/version.o:\
cc-compile ctxt/version.c
	./cc-compile ctxt/version.c

deinstaller:\
cc-link deinstaller.ld deinstaller.o insthier.o install.a ctxt/ctxt.a
	./cc-link deinstaller deinstaller.o insthier.o install.a ctxt/ctxt.a

deinstaller.o:\
cc-compile deinstaller.c install.h ctxt.h
	./cc-compile deinstaller.c

install-core.o:\
cc-compile install-core.c install.h
	./cc-compile install-core.c

install-error.o:\
cc-compile install-error.c install.h
	./cc-compile install-error.c

install-posix.o:\
cc-compile install-posix.c install.h
	./cc-compile install-posix.c

install-win32.o:\
cc-compile install-win32.c install.h
	./cc-compile install-win32.c

install.a:\
cc-slib install.sld install-core.o install-posix.o install-win32.o \
install-error.o
	./cc-slib install install-core.o install-posix.o install-win32.o \
	install-error.o

install.h:\
install_os.h

installer:\
cc-link installer.ld installer.o insthier.o install.a ctxt/ctxt.a
	./cc-link installer installer.o insthier.o install.a ctxt/ctxt.a

installer.o:\
cc-compile installer.c ctxt.h install.h
	./cc-compile installer.c

instchk:\
cc-link instchk.ld instchk.o insthier.o install.a ctxt/ctxt.a
	./cc-link instchk instchk.o insthier.o install.a ctxt/ctxt.a

instchk.o:\
cc-compile instchk.c ctxt.h install.h
	./cc-compile instchk.c

insthier.o:\
cc-compile insthier.c ctxt.h install.h
	./cc-compile insthier.c

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

mk-sosuffix:\
conf-systype

mk-systype:\
conf-cc conf-ld

symbex-conf:\
cc-link symbex-conf.ld symbex-conf.o ctxt/ctxt.a
	./cc-link symbex-conf symbex-conf.o ctxt/ctxt.a

symbex-conf.o:\
cc-compile symbex-conf.c ctxt.h _sysinfo.h
	./cc-compile symbex-conf.c

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

symbex-walk.ads:\
symbex.ali symbex-parse.ali

symbex-walk.ali:\
ada-compile symbex-walk.adb symbex.ali symbex-walk.ads
	./ada-compile symbex-walk.adb

symbex-walk.o:\
symbex-walk.ali

symbex.a:\
cc-slib symbex.sld symbex-lex.o symbex-parse.o symbex-walk.o symbex.o
	./cc-slib symbex symbex-lex.o symbex-parse.o symbex-walk.o symbex.o

symbex.ali:\
ada-compile symbex.ads symbex.ads
	./ada-compile symbex.ads

symbex.o:\
symbex.ali

clean-all: sysdeps_clean tests_clean obj_clean ext_clean
clean: obj_clean
obj_clean:
	rm -f UNIT_TESTS/dump UNIT_TESTS/dump.ali UNIT_TESTS/dump.o UNIT_TESTS/lexer \
	UNIT_TESTS/lexer.ali UNIT_TESTS/lexer.o UNIT_TESTS/parser UNIT_TESTS/parser.ali \
	UNIT_TESTS/parser.o ctxt/bindir.c ctxt/bindir.o ctxt/ctxt.a ctxt/dlibdir.c \
	ctxt/dlibdir.o ctxt/fakeroot.c ctxt/fakeroot.o ctxt/incdir.c ctxt/incdir.o \
	ctxt/repos.c ctxt/repos.o ctxt/slibdir.c ctxt/slibdir.o ctxt/version.c \
	ctxt/version.o deinstaller deinstaller.o install-core.o install-error.o \
	install-posix.o install-win32.o install.a installer installer.o instchk \
	instchk.o insthier.o symbex-conf symbex-conf.o symbex-lex.ali symbex-lex.o \
	symbex-parse.ali symbex-parse.o symbex-walk.ali symbex-walk.o symbex.a \
	symbex.ali symbex.o
ext_clean:
	rm -f conf-adatype conf-cctype conf-ldtype conf-sosuffix conf-systype mk-ctxt

regen:\
ada-srcmap ada-srcmap-all
	./ada-srcmap-all
	cpj-genmk > Makefile.tmp && mv Makefile.tmp Makefile
