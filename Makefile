
# C compiler
CC=cc

# optimized build
CFLAGS=-O2 -pipe -Wall -W -Wextra
LDFLAGS=-s

# debug build
# CFLAGS=-g -pipe -Wall -W -Wextra
# LDFLAGS=-g

# C compiler with additional flags for C shared library (not compiled by default)
CC_SO=$(CC) -shared -fPIC

# Autodetect Chez Scheme installation.
# Alternatively, you can manually specify it, as for example:
#  CHEZ_SCHEME_DIR=/usr/local/lib/csv10.0.0/ta6le
#  CHEZ_SCHEME_KERNEL=/usr/local/lib/csv10.0.0/ta6le/kernel.o
CHEZ_SCHEME_DIR:=$(shell ./utils/find_chez_scheme_dir.sh)
CHEZ_SCHEME_KERNEL:=$(shell ./utils/find_chez_scheme_kernel.sh $(CHEZ_SCHEME_DIR))

# required libraries
LIB_ICONV:=$(shell uname -o | grep -q -E '(Android|Darwin)' && echo -liconv)
LIB_UUID:=$(shell uname -o | grep -q -E '(FreeBSD|Darwin)' || echo -luuid)

LIBS=$(CHEZ_SCHEME_KERNEL) -lz -llz4 -lncurses -ldl -lm -lpthread $(LIB_UUID) $(LIB_ICONV)


# installation directories. Names and values are taken from GNU Makefile conventions
# and can be overridden from "make" command line
prefix      = /usr/local
exec_prefix = $(prefix)
bindir      = $(exec_prefix)/bin
libdir      = $(exec_prefix)/lib

SCHEMESH_LIBDIR = $(libdir)/schemesh


# installation programs. Names and values are taken from GNU Makefile conventions
# and can be overridden from "make" command line
INSTALL         = install
INSTALL_PROGRAM = $(INSTALL)
INSTALL_DATA    = $(INSTALL) -m 644
MKDIR_P         = mkdir -p



######################################################################################
# no user-serviceable parts below this line
######################################################################################
LIBSCHEMESH_SO=libschemesh_0.8.1.so
LIBSCHEMESH_C_SO=libschemesh_c_0.8.1.so

SRCS=containers/containers.c eval.c posix/posix.c shell/shell.c
OBJS=containers.o eval.o posix.o shell.o

all: schemesh schemesh_test $(LIBSCHEMESH_SO)

clean:
	rm -f *~ *.o *.so schemesh schemesh_test

containers.o: containers/containers.c containers/containers.h eval.h
	$(CC) -o $@ -c $< $(CFLAGS) -I"$(CHEZ_SCHEME_DIR)"

eval.o: eval.c eval.h
	$(CC) -o $@ -c $< $(CFLAGS) -I"$(CHEZ_SCHEME_DIR)"

posix.o: posix/posix.c posix/posix.h eval.h
	$(CC) -o $@ -c $< $(CFLAGS) -I"$(CHEZ_SCHEME_DIR)"

shell.o: shell/shell.c shell/shell.h containers/containers.h eval.h posix/posix.h
	$(CC) -o $@ -c $< $(CFLAGS) -I"$(CHEZ_SCHEME_DIR)" -DCHEZ_SCHEME_DIR="$(CHEZ_SCHEME_DIR)" -DSCHEMESH_LIBDIR="$(SCHEMESH_LIBDIR)"




main.o: main.c main.h eval.h shell/shell.h
	$(CC) -o $@ -c $< $(CFLAGS) -I"$(CHEZ_SCHEME_DIR)"

test.o: test.c test.h eval.h shell/shell.h
	$(CC) -o $@ -c $< $(CFLAGS) -I"$(CHEZ_SCHEME_DIR)"


schemesh: main.o $(OBJS)
	$(CC) -o $@ $^ $(LDFLAGS) -L"$(CHEZ_SCHEME_DIR)" $(LIBS)

schemesh_test: test.o $(OBJS)
	$(CC) -o $@ $^ $(LDFLAGS) -L"$(CHEZ_SCHEME_DIR)" $(LIBS)

$(LIBSCHEMESH_SO): schemesh_test
	./schemesh_test


installdirs:
	$(MKDIR_P) "$(DESTDIR)$(bindir)"
	$(MKDIR_P) "$(DESTDIR)$(SCHEMESH_LIBDIR)"

install: all installdirs
	$(INSTALL_PROGRAM) schemesh "$(DESTDIR)$(bindir)"
	$(INSTALL_DATA) $(LIBSCHEMESH_SO) "$(DESTDIR)$(SCHEMESH_LIBDIR)"

uninstall:
	rm -f "$(DESTDIR)$(bindir)/schemesh" "$(DESTDIR)$(SCHEMESH_LIBDIR)/$(LIBSCHEMESH_SO)" "$(DESTDIR)$(SCHEMESH_LIBDIR)/$(LIBSCHEMESH_C_SO)"


# by default, C shared library is not compiled.
c_so: $(LIBSCHEMESH_C_SO)

$(LIBSCHEMESH_C_SO): $(SRCS)
	$(CC_SO) -o $@ $^ $(CFLAGS) -I"$(CHEZ_SCHEME_DIR)" -DCHEZ_SCHEME_DIR="$(CHEZ_SCHEME_DIR)" -DSCHEMESH_LIBDIR="$(SCHEMESH_LIBDIR)" $(LDFLAGS)

install_c_so: $(LIBSCHEMESH_C_SO) installdirs
	$(INSTALL_DATA) $(LIBSCHEMESH_C_SO) "$(DESTDIR)$(SCHEMESH_LIBDIR)"
