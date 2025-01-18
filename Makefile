
# C compiler
CC=cc

# debug build
CFLAGS=-g -pipe -Wall -W -Wextra
LDFLAGS=-g

# optimized build
#  CFLAGS=-O2 -pipe -Wall -W -Wextra
#  LDFLAGS=-s

CP=cp
MKDIR_P=mkdir -p
INSTALL=install

INSTALL_DIR=/usr/local
INSTALL_BINDIR=$(INSTALL_DIR)/bin
INSTALL_LIBDIR=$(INSTALL_DIR)/lib/schemesh


# Autodetect Chez Scheme installation.
# Alternatively, you can manually specify it, as for example:
#  CHEZ_SCHEME_DIR=/usr/local/lib/csv10.0.0/ta6le
#  CHEZ_SCHEME_KERNEL=/usr/local/lib/csv10.0.0/ta6le/kernel.o
CHEZ_SCHEME_DIR:=$(shell ./utils/find_chez_scheme_dir.sh)
CHEZ_SCHEME_KERNEL:=$(shell ./utils/find_chez_scheme_kernel.sh $(CHEZ_SCHEME_DIR))

LIB_ICONV:=$(shell uname -o | grep -q Android && echo -liconv)

LIBS=$(CHEZ_SCHEME_KERNEL) -lz -llz4 -lncurses -ldl -lm -lpthread -luuid $(LIB_ICONV)

#
# no user-serviceable parts below this line
#
LIBSCHEMESH_SO=libschemesh_0.7.so

OBJS=containers.o eval.o posix.o shell.o signal.o

all: schemesh schemesh_test

clean:
	rm -f *~ *.o *.so schemesh schemesh_test

containers.o: containers/containers.c containers/containers.h eval.h
	$(CC) -o $@ -c $< $(CFLAGS) -I$(CHEZ_SCHEME_DIR)

eval.o: eval.c eval.h
	$(CC) -o $@ -c $< $(CFLAGS) -I$(CHEZ_SCHEME_DIR)

main.o: main.c main.h eval.h shell/shell.h
	$(CC) -o $@ -c $< $(CFLAGS) -I$(CHEZ_SCHEME_DIR)

posix.o: posix/posix.c posix/posix.h eval.h posix/signal.h
	$(CC) -o $@ -c $< $(CFLAGS) -I$(CHEZ_SCHEME_DIR)

signal.o: posix/signal.c posix/signal.h eval.h posix/posix.h
	$(CC) -o $@ -c $< $(CFLAGS) -I$(CHEZ_SCHEME_DIR)

shell.o: shell/shell.c shell/shell.h containers/containers.h eval.h posix/posix.h posix/signal.h
	$(CC) -o $@ -c $< $(CFLAGS) -I$(CHEZ_SCHEME_DIR) -DCHEZ_SCHEME_DIR="$(CHEZ_SCHEME_DIR)" -DINSTALL_LIBDIR="$(INSTALL_LIBDIR)"

test.o: test.c test.h shell/shell.h
	$(CC) -o $@ -c $< $(CFLAGS) -I$(CHEZ_SCHEME_DIR)


schemesh: main.o $(OBJS)
	$(CC) -o $@ $^ $(LDFLAGS) -L$(CHEZ_SCHEME_DIR) $(LIBS)

schemesh_test: test.o $(OBJS)
	$(CC) -o $@ $^ $(LDFLAGS) -L$(CHEZ_SCHEME_DIR) $(LIBS)

$(LIBSCHEMESH_SO): schemesh
	./schemesh --compile-source-dir=.

install: schemesh $(LIBSCHEMESH_SO)
	$(INSTALL) schemesh $(INSTALL_BINDIR) || $(CP) schemesh $(INSTALL_BINDIR)
	$(MKDIR_P) $(INSTALL_LIBDIR)
	$(INSTALL) $(LIBSCHEMESH_SO) $(INSTALL_LIBDIR) || $(CP) $(LIBSCHEMESH_SO) $(INSTALL_LIBDIR)
