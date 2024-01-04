
# C compiler
CC=cc

# debug build
CFLAGS=-g -pipe -Wall -W -Wextra
LDFLAGS=-g

# optimized build
#  CFLAGS=-O2 -pipe -Wall -W -Wextra
#  LDFLAGS=-s

# Autodetect Chez Scheme installation.
# Alternatively, you can manually specify it, as for example:
#  CHEZ_SCHEME_DIR=/usr/local/lib/csv9.5.9/ta6le
#  CHEZ_SCHEME_KERNEL=/usr/local/lib/csv9.5.9/ta6le/kernel.o
CHEZ_SCHEME_DIR:=$(shell ./utils/find_chez_scheme_dir.sh)
CHEZ_SCHEME_KERNEL:=$(shell ./utils/find_chez_scheme_kernel.sh $(CHEZ_SCHEME_DIR))

LIBS=$(CHEZ_SCHEME_KERNEL) -lz -llz4 -lncurses -ldl -lm -lpthread -luuid

#
# no user-serviceable parts below this line
#
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
	$(CC) -o $@ -c $< $(CFLAGS) -I$(CHEZ_SCHEME_DIR) -DCHEZ_SCHEME_DIR="$(CHEZ_SCHEME_DIR)"

test.o: test.c test.h shell/shell.h
	$(CC) -o $@ -c $< $(CFLAGS) -I$(CHEZ_SCHEME_DIR)


schemesh: main.o $(OBJS)
	$(CC) -o $@ $^ $(LDFLAGS) -L$(CHEZ_SCHEME_DIR) $(LIBS)

schemesh_test: test.o $(OBJS)
	$(CC) -o $@ $^ $(LDFLAGS) -L$(CHEZ_SCHEME_DIR) $(LIBS)
