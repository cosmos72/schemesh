
# C compiler
CC=cc

# optimized build
CFLAGS=-O2 -pipe -Wall -W -Wextra
LDFLAGS=-s

# debug build
# CFLAGS=-g -pipe -Wall -W -Wextra
# LDFLAGS=-g

# Autodetect Chez Scheme installation.
# Alternatively, you can manually specify it, as for example:
#  CHEZ_SCHEME_DIR=/usr/local/lib/csv10.0.0/ta6le
#  CHEZ_SCHEME_KERNEL=/usr/local/lib/csv10.0.0/ta6le/kernel.o
CHEZ_SCHEME_DIR:=$(shell ./utils/find_chez_scheme_dir.sh)
CHEZ_SCHEME_KERNEL:=$(shell ./utils/find_chez_scheme_kernel.sh $(CHEZ_SCHEME_DIR))

# required libraries
OS:=$(strip $(shell uname -o))

LIBS_TERMINFO:=-ltinfo
ifeq ($(OS), Android)
  LIBS_UTIL:=
else ifeq ($(OS), Darwin)
  LIBS_TERMINFO:=-lncurses
  LIBS_UTIL:=-liconv
else ifeq ($(OS), FreeBSD)
  LIBS_UTIL:=-lutil
else # GNU/Linux
  LIBS_UTIL:=-luuid
  LIBS_EXTRA_STATIC:=-lxxhash
endif

LIBS_COMMON:=$(CHEZ_SCHEME_KERNEL) -lz -llz4 $(LIBS_TERMINFO) $(LIBS_UTIL)
LIBS_OS:=-ldl -lm -lpthread
LIBS:=$(LIBS_COMMON) $(LIBS_OS)


# installation directories. Names and values are taken from GNU Makefile conventions
# and can be overridden from 'make' command line
prefix      = /usr/local
exec_prefix = $(prefix)
bindir      = $(exec_prefix)/bin
libdir      = $(exec_prefix)/lib

SCHEMESH_DIR = $(libdir)/schemesh

# not used by default
SCHEME2K_DIR = $(libdir)/scheme2k


# installation programs. Names and values are taken from GNU Makefile conventions
# and can be overridden from 'make' command line
INSTALL         = install
INSTALL_PROGRAM = $(INSTALL)
INSTALL_DATA    = $(INSTALL) -m 644
MKDIR_P         = mkdir -p


# C compiler for shared libraries (not used by default)
CC_SO=cc
# additional C compiler flags for C shared library (not used by default)
CFLAGS_SO=-fPIC
# linker flags for C shared library (not used by default)
LDFLAGS_SO=-shared


######################################################################################
# no user-serviceable parts below this line
######################################################################################

VERSION_STR=1.0.0

## by default, only compile and install additional programs that do *not* require
## external dependencies (libcurl, libsqlite3 ...)

default: countdown schemesh schemesh_test schemesh_so

clean: clean_schemesh clean_scheme2k clean_utils
	rm -f *~ *.o *.so

install:     install_countdown   install_schemesh
uninstall: uninstall_countdown uninstall_schemesh

installdirs:
	$(MKDIR_P) '$(DESTDIR)$(bindir)'
	$(MKDIR_P) '$(DESTDIR)$(SCHEMESH_DIR)'


######################################################################################
# all programs and libraries, including additional ones that require
# external dependencies (libcurl, libsqlite3 ...)
######################################################################################

all: default scheme2k utils

clean_all: clean

install_all:     install_schemesh   install_scheme2k   install_utils
uninstall_all: uninstall_schemesh uninstall_scheme2k uninstall_utils


######################################################################################
# schemesh
######################################################################################
SCHEMESH_SO=libschemesh_$(VERSION_STR).so

SRCS=containers/containers.c eval.c posix/posix.c
OBJS=containers.o eval.o os.o posix.o


containers.o: containers/containers.c containers/containers.h eval.h
	$(CC) -o $@ -c $< $(CFLAGS) -I'$(CHEZ_SCHEME_DIR)'

eval.o: eval.c eval.h
	$(CC) -o $@ -c $< $(CFLAGS) -I'$(CHEZ_SCHEME_DIR)'

posix.o: posix/posix.c containers/containers.h eval.h posix/endpoint.h posix/fd.h posix/fs.h posix/pid.h posix/posix.h posix/signal.h posix/socket.h posix/tty.h os/os.h
	$(CC) -o $@ -c $< $(CFLAGS) -I'$(CHEZ_SCHEME_DIR)' -DCHEZ_SCHEME_DIR='$(CHEZ_SCHEME_DIR)'

os.o: os/process.c os/os.h os/process_all.h os/process_linux.h os/process_unsupported.h
	$(CC) -o $@ -c $< $(CFLAGS) -I'$(CHEZ_SCHEME_DIR)' -DCHEZ_SCHEME_DIR='$(CHEZ_SCHEME_DIR)'

main.o: main.c containers/containers.h eval.h load.h posix/posix.h
	$(CC) -o $@ -c $< $(CFLAGS) -I'$(CHEZ_SCHEME_DIR)' -DSCHEMESH_DIR='$(SCHEMESH_DIR)'

test.o: test/test.c containers/containers.h eval.h load.h posix/posix.h
	$(CC) -o $@ -c $< $(CFLAGS) -I'$(CHEZ_SCHEME_DIR)' -DSCHEMESH_DIR='$(SCHEMESH_DIR)'


schemesh: main.o $(OBJS)
	$(CC) -o $@ $^ $(LDFLAGS) -L'$(CHEZ_SCHEME_DIR)' $(LIBS)

# not compiled by default. Requires Chez Scheme >= 10.0.0, GNU or CLANG assembler, and GNU ld or CLANG lld
#
# embeds Chez Scheme boot files petite.boot scheme.boot
# embeds libschemesh_VERSION.so
# links against system-wide static libraries where feasible
main_embed.o: main.c containers/containers.h eval.h load.h posix/posix.h
	$(CC) -o $@ -c $< $(CFLAGS) -I'$(CHEZ_SCHEME_DIR)'  -DSCHEMESH_STATIC

asm_embed.o: asm_embed.S $(SCHEMESH_SO)
	$(CC) -o $@ -c $< $(CFLAGS) -DCHEZ_SCHEME_DIR='$(CHEZ_SCHEME_DIR)'

ifeq ($(OS), FreeBSD)
  schemesh_static: main_embed.o asm_embed.o $(OBJS)
	$(CC) -o $@ $^ $(LDFLAGS) -L'$(CHEZ_SCHEME_DIR)' -static $(LIBS)
else # Android, GNU/Linux
  schemesh_static: main_embed.o asm_embed.o $(OBJS)
	$(CC) -o $@ $^ $(LDFLAGS) -L'$(CHEZ_SCHEME_DIR)' -Wl,-Bstatic $(LIBS_COMMON) $(LIBS_EXTRA_STATIC) -Wl,-Bdynamic $(LIBS_OS)
endif

schemesh_test: test.o $(OBJS)
	$(CC) -o $@ $^ $(LDFLAGS) -L'$(CHEZ_SCHEME_DIR)' $(LIBS)

schemesh_so: $(SCHEMESH_SO)

$(SCHEMESH_SO): schemesh_test
	./schemesh_test


clean_schemesh:
	rm -f schemesh schemesh_static schemesh_test $(SCHEMESH_SO) libscheme2k_temp.so $(OBJS)

install_schemesh: schemesh schemesh_so installdirs
	$(INSTALL_PROGRAM) schemesh '$(DESTDIR)$(bindir)'
	$(INSTALL_DATA) $(SCHEMESH_SO) '$(DESTDIR)$(SCHEMESH_DIR)'

# schemesh_static is not compiled nor installed by default
install_schemesh_static: schemesh_static installdirs
	$(INSTALL_PROGRAM) schemesh_static '$(DESTDIR)$(bindir)'

uninstall_schemesh:
	rm -f '$(DESTDIR)$(bindir)/schemesh' '$(DESTDIR)$(SCHEMESH_DIR)/$(SCHEMESH_SO)'

uninstall_schemesh_static:
	rm -f '$(DESTDIR)$(bindir)/schemesh_static


################################################################################
# additional programs. Some require external libraries (libcurl, libsqlite3 ...)
################################################################################

utils:                     countdown           dir           http           parse_sqlite           proc
clean_utils:         clean_countdown     clean_dir     clean_http     clean_parse_sqlite     clean_proc
install_utils:     install_countdown   install_dir   install_http   install_parse_sqlite   install_proc
uninstall_utils: uninstall_countdown uninstall_dir uninstall_http uninstall_parse_sqlite uninstall_proc


################################################################################
# C program: countdown
# minimal 'sleep' reimplementation, suspends counting when stopped
################################################################################

countdown: c/countdown.c
	$(CC) -o $@ $< $(CFLAGS)  $(LDFLAGS)

clean_countdown:
	rm -f countdown

install_countdown: countdown installdirs
	$(INSTALL_PROGRAM) countdown '$(DESTDIR)$(bindir)'

uninstall_countdown:
	rm -f '$(DESTDIR)$(bindir)/countdown'


################################################################################
# C program: dir
# minimal 'ls' reimplementation, with JSON output
################################################################################

dir: c/dir.c c/writer.h
	$(CC) -o $@ $< $(CFLAGS) $(LDFLAGS)

clean_dir:
	rm -f dir

install_dir: dir installdirs
	$(INSTALL_PROGRAM) dir '$(DESTDIR)$(bindir)'

uninstall_dir:
	rm -f '$(DESTDIR)$(bindir)/dir'


################################################################################
# optional C program: http
# wraps libcurl
################################################################################

http: c/http.c
	$(CC) -o $@ $< $(CFLAGS) -lcurl $(LDFLAGS)

clean_http:
	rm -f http

install_http: http installdirs
	$(INSTALL_PROGRAM) http '$(DESTDIR)$(bindir)'

uninstall_http:
	rm -f '$(DESTDIR)$(bindir)/http'


################################################################################
# optional C program: parse_sqlite
# wraps libsqlite3
################################################################################

parse_sqlite: c/parse_sqlite.c c/writer.h
	$(CC) -o $@ $< $(CFLAGS) -lsqlite3 $(LDFLAGS)

clean_parse_sqlite:
	rm -f parse_sqlite

install_parse_sqlite: parse_sqlite installdirs
	$(INSTALL_PROGRAM) parse_sqlite '$(DESTDIR)$(bindir)'

uninstall_parse_sqlite:
	rm -f '$(DESTDIR)$(bindir)/parse_sqlite'


################################################################################
# C program: proc
# minimal 'ps' reimplementation, with JSON output
################################################################################

proc: c/proc.c c/proc_common.h c/proc_freebsd.h c/proc_linux.h c/proc_macos.h c/proc_unsupported.h c/writer.h
	$(CC) -o $@ $< $(CFLAGS) $(LIBS_UTIL) $(LDFLAGS)

clean_proc:
	rm -f proc

install_proc: proc installdirs
	$(INSTALL_PROGRAM) proc '$(DESTDIR)$(bindir)'

uninstall_proc:
	rm -f '$(DESTDIR)$(bindir)/proc'


################################################################################
# additional libraries. not compiled by default
################################################################################

scheme2k:                     scheme2k_c_so           scheme2k_so
clean_scheme2k:         clean_scheme2k_c_so     clean_scheme2k_so
install_scheme2k:     install_scheme2k_c_so   install_scheme2k_so
uninstall_scheme2k: uninstall_scheme2k_c_so uninstall_scheme2k_so

install_scheme2k_dirs:
	$(MKDIR_P) '$(DESTDIR)$(SCHEME2K_DIR)'


################################################################################
# additional Scheme library: libscheme2k_VERSION.so
# contains the LGPL-licensed subset of schemesh
################################################################################

SCHEME2K_SO=libscheme2k_$(VERSION_STR).so

scheme2k_so: $(SCHEME2K_SO)

$(SCHEME2K_SO): schemesh_test
	./schemesh_test --compile_scheme2k_so

clean_scheme2k_so:
	rm -f $(SCHEME2K_SO) libscheme2k_temp.so

install_scheme2k_so: scheme2k_so install_scheme2k_dirs
	$(INSTALL_DATA) $(SCHEME2K_SO) '$(DESTDIR)$(SCHEME2K_DIR)'

uninstall_scheme2k_so:
	rm -f '$(DESTDIR)$(SCHEME2K_DIR)/$(SCHEME2K_SO)'


################################################################################
# additional C shared library: libscheme2k_c_VERSION.so
# contains LGPL-licensed C functions needed by libscheme2k_VERSION.so
################################################################################

SCHEME2K_C_SO=libscheme2k_c_$(VERSION_STR).so

scheme2k_c_so: $(SCHEME2K_C_SO)

$(SCHEME2K_C_SO): $(SRCS)
	$(CC_SO) -o $@ $^ $(CFLAGS) $(CFLAGS_SO) -I'$(CHEZ_SCHEME_DIR)' -DCHEZ_SCHEME_DIR='$(CHEZ_SCHEME_DIR)' -DSCHEMESH_DIR='$(SCHEMESH_DIR)' $(LDFLAGS) $(LDFLAGS_SO)

clean_scheme2k_c_so:
	rm -f $(SCHEME2K_C_SO)

install_scheme2k_c_so: scheme2k_c_so install_scheme2k_dirs
	$(INSTALL_DATA) $(SCHEME2K_C_SO) '$(DESTDIR)$(SCHEME2K_DIR)'

uninstall_scheme2k_c_so:
	rm -f '$(DESTDIR)$(SCHEME2K_DIR)/$(SCHEME2K_C_SO)'


################################################################################

