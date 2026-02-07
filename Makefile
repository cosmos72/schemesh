
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

######################################################################################
# schemesh rules
######################################################################################
SCHEMESH_SO=libschemesh_0.9.3.so

SRCS=containers/containers.c eval.c posix/posix.c
OBJS=containers.o eval.o os.o posix.o

all: schemesh schemesh_test $(SCHEMESH_SO) countdown

schemesh_so: $(SCHEMESH_SO)

clean:
	rm -f *~ *.o *.so schemesh schemesh_test countdown

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

scheme2k:      scheme2k_so scheme2k_c_so
scheme2k_full: scheme2k_so scheme2k_c_so scheme2k_http_c_so
batteries:     scheme2k_full

install_scheme2k:      install_scheme2k_so install_scheme2k_c_so
install_scheme2k_full: install_scheme2k_so install_scheme2k_c_so install_scheme2k_http_c_so
install_batteries:     install_scheme2k_full

install_scheme2k_dirs:
	$(MKDIR_P) '$(DESTDIR)$(SCHEME2K_DIR)'

################################################################################
# optional Scheme library: libscheme2k_VERSION.so
################################################################################

SCHEME2K_SO=libscheme2k_0.9.3.so

scheme2k_so: $(SCHEME2K_SO)

$(SCHEME2K_SO): schemesh_test
	./schemesh_test --compile_scheme2k_so

install_scheme2k_so: $(SCHEME2K_SO) install_scheme2k_dirs
	$(INSTALL_DATA) $(SCHEME2K_SO) '$(DESTDIR)$(SCHEME2K_DIR)'

################################################################################
# optional C shared library: libscheme2k_c_VERSION.so
# contains C functions needed by libscheme2k_VERSION.so
################################################################################

SCHEME2K_C_SO=libscheme2k_c_0.9.3.so

scheme2k_c_so: $(SCHEME2K_C_SO)

$(SCHEME2K_C_SO): $(SRCS)
	$(CC_SO) -o $@ $^ $(CFLAGS) $(CFLAGS_SO) -I'$(CHEZ_SCHEME_DIR)' -DCHEZ_SCHEME_DIR='$(CHEZ_SCHEME_DIR)' -DSCHEMESH_DIR='$(SCHEMESH_DIR)' $(LDFLAGS) $(LDFLAGS_SO)

install_scheme2k_c_so: $(SCHEME2K_C_SO) install_scheme2k_dirs
	$(INSTALL_DATA) $(SCHEME2K_C_SO) '$(DESTDIR)$(SCHEME2K_DIR)'


################################################################################
# optional C shared library: libscheme2k_http_c_VERSION.so
# wraps libcurl and is needed by function (http-url->port)
################################################################################

LIB_CURL=-lcurl

SCHEME2K_HTTP_C_SO=libscheme2k_http_c_0.9.3.so

scheme2k_http_c_so: $(SCHEME2K_HTTP_C_SO)

$(SCHEME2K_HTTP_C_SO): io/http.c
	$(CC_SO) -o $@ $^ $(CFLAGS) $(CFLAGS_SO) $(LIB_CURL) $(LDFLAGS) $(LDFLAGS_SO)

install_scheme2k_http_c_so: $(SCHEME2K_HTTP_C_SO) install_scheme2k_dirs
	$(INSTALL_DATA) $(SCHEME2K_HTTP_C_SO) '$(DESTDIR)$(SCHEME2K_DIR)'

################################################################################

clean_scheme2k:
	rm -f libscheme2k_temp.so $(SCHEME2K_SO) $(SCHEME2K_C_SO) $(SCHEME2K_HTTP_C_SO)

clean_batteries: clean_scheme2k

