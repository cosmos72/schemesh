
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
LIB_ICONV:=$(shell uname -o | grep -q -E '(Android|Darwin)' && echo -liconv)
LIB_UUID:=$(shell uname -o | grep -q -E '(FreeBSD|Darwin)' || echo -luuid)

LIBS=$(CHEZ_SCHEME_KERNEL) -lz -llz4 -lncurses -ldl -lm -lpthread $(LIB_UUID) $(LIB_ICONV)


# installation directories. Names and values are taken from GNU Makefile conventions
# and can be overridden from 'make' command line
prefix      = /usr/local
exec_prefix = $(prefix)
bindir      = $(exec_prefix)/bin
libdir      = $(exec_prefix)/lib

SCHEMESH_DIR = $(libdir)/schemesh


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

######################################################################################
# schemesh rules
######################################################################################
SCHEMESH_SO=libschemesh_0.9.2.so

SRCS=containers/containers.c eval.c posix/posix.c shell/shell.c
OBJS=containers.o eval.o posix.o shell.o

all: schemesh schemesh_test $(SCHEMESH_SO) countdown

schemesh_so: $(SCHEMESH_SO)

clean:
	rm -f *~ *.o *.so schemesh schemesh_test countdown

containers.o: containers/containers.c containers/containers.h eval.h
	$(CC) -o $@ -c $< $(CFLAGS) -I'$(CHEZ_SCHEME_DIR)'

eval.o: eval.c eval.h
	$(CC) -o $@ -c $< $(CFLAGS) -I'$(CHEZ_SCHEME_DIR)'

posix.o: posix/posix.c posix/posix.h posix/signal.h eval.h
	$(CC) -o $@ -c $< $(CFLAGS) -I'$(CHEZ_SCHEME_DIR)'

shell.o: shell/shell.c shell/shell.h containers/containers.h eval.h posix/posix.h
	$(CC) -o $@ -c $< $(CFLAGS) -I'$(CHEZ_SCHEME_DIR)' -DCHEZ_SCHEME_DIR='$(CHEZ_SCHEME_DIR)' -DSCHEMESH_DIR='$(SCHEMESH_DIR)'

main.o: main.c eval.h shell/shell.h
	$(CC) -o $@ -c $< $(CFLAGS) -I'$(CHEZ_SCHEME_DIR)'

test.o: test/test.c eval.h shell/shell.h
	$(CC) -o $@ -c $< $(CFLAGS) -I'$(CHEZ_SCHEME_DIR)'


schemesh: main.o $(OBJS)
	$(CC) -o $@ $^ $(LDFLAGS) -L'$(CHEZ_SCHEME_DIR)' $(LIBS)

schemesh_test: test.o $(OBJS)
	$(CC) -o $@ $^ $(LDFLAGS) -L'$(CHEZ_SCHEME_DIR)' $(LIBS)

$(SCHEMESH_SO): schemesh_test
	./schemesh_test

countdown: utils/countdown.c
	$(CC) -o $@ $< $(CFLAGS)  $(LDFLAGS)


installdirs:
	$(MKDIR_P) '$(DESTDIR)$(bindir)'
	$(MKDIR_P) '$(DESTDIR)$(SCHEMESH_DIR)'

install: all installdirs
	$(INSTALL_PROGRAM) schemesh countdown '$(DESTDIR)$(bindir)'
	$(INSTALL_DATA) $(SCHEMESH_SO) '$(DESTDIR)$(SCHEMESH_DIR)'

uninstall:
	rm -f '$(DESTDIR)$(bindir)/schemesh' '$(DESTDIR)$(bindir)/countdown' '$(DESTDIR)$(SCHEMESH_DIR)/$(SCHEMESH_SO)'


################################################################################
# optional libraries
################################################################################

# by default, do *not* compile C shared libraries and optional Scheme libraries

batteries: chez_batteries_so chez_batteries_c_so chez_curl_c_so

install_batteries: install_chez_batteries_so install_chez_batteries_c_so install_chez_curl_c_so


################################################################################
# optional Scheme libraries
################################################################################

CHEZ_BATTERIES_SO=libchez_batteries_0.9.2.so

chez_batteries_so: $(CHEZ_BATTERIES_SO)

compile_chez_batteries.o: utils/compile_chez_batteries.c eval.h
	$(CC) -o $@ -c $< $(CFLAGS) -I'$(CHEZ_SCHEME_DIR)' -DCHEZ_SCHEME_DIR='$(CHEZ_SCHEME_DIR)'

compile_chez_batteries: compile_chez_batteries.o $(OBJS)
	$(CC) -o $@ $^ $(LDFLAGS) -L'$(CHEZ_SCHEME_DIR)' $(LIBS)

$(CHEZ_BATTERIES_SO): compile_chez_batteries
	./compile_chez_batteries

install_chez_batteries_so: $(CHEZ_BATTERIES_SO) installdirs
	$(INSTALL_DATA) $(CHEZ_BATTERIES_SO) '$(DESTDIR)$(SCHEMESH_DIR)'

################################################################################
# optional C shared library libchez_batteries_c_X.Y.Z.so
# contains C functions needed by libchez_batteries_X.Y.Z.so
################################################################################

CHEZ_BATTERIES_C_SO=libchez_batteries_c_0.9.2.so

chez_batteries_c_so: $(CHEZ_BATTERIES_C_SO)

$(CHEZ_BATTERIES_C_SO): $(SRCS)
	$(CC_SO) -o $@ $^ $(CFLAGS) $(CFLAGS_SO) -I'$(CHEZ_SCHEME_DIR)' -DCHEZ_SCHEME_DIR='$(CHEZ_SCHEME_DIR)' -DSCHEMESH_DIR='$(SCHEMESH_DIR)' $(LDFLAGS) $(LDFLAGS_SO)

install_chez_batteries_c_so: $(CHEZ_BATTERIES_C_SO) installdirs
	$(INSTALL_DATA) $(CHEZ_BATTERIES_C_SO) '$(DESTDIR)$(SCHEMESH_DIR)'


################################################################################
# optional C shared library libchez_curl_c_X.Y.Z.so
# wraps libcurl and is needed by function (http-url->port)
################################################################################

LIB_CURL=-lcurl

CHEZ_CURL_C_SO=libchez_curl_c_0.9.2.so

chez_curl_c_so: $(CHEZ_CURL_C_SO)

$(CHEZ_CURL_C_SO): port/http.c
	$(CC_SO) -o $@ $^ $(CFLAGS) $(CFLAGS_SO) $(LIB_CURL) $(LDFLAGS) $(LDFLAGS_SO)

install_chez_curl_c_so: $(CHEZ_CURL_C_SO) installdirs
	$(INSTALL_DATA) $(CHEZ_CURL_C_SO) '$(DESTDIR)$(SCHEMESH_DIR)'
