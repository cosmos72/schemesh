
CFLAGS=-g -Wall -W

#CHEZ_SCHEME_DIR=/usr/local/lib/csv9.5.9/ta6le
CHEZ_SCHEME_DIR:=$(shell ./utils/find_chez_scheme_dir.sh)

OBJS=container.o eval.o io.o lineedit.o parse.o posix.o repl.o shell.o signal.o

LIBS=-lkernel -lz -llz4 -lncurses -ldl -lm -lpthread -luuid

all: schemesh schemesh_test

clean:
	rm -f *~ *.o *.so */*.so schemesh schemesh_test

container.o: container.c container.h eval.h
	$(CC) -o $@ -c $< $(CFLAGS) -I$(CHEZ_SCHEME_DIR)

eval.o: eval.c eval.h
	$(CC) -o $@ -c $< $(CFLAGS) -I$(CHEZ_SCHEME_DIR)

lineedit.o: lineedit.c lineedit.h eval.h posix.h
	$(CC) -o $@ -c $< $(CFLAGS) -I$(CHEZ_SCHEME_DIR)

main.o: main.c main.h eval.h shell.h
	$(CC) -o $@ -c $< $(CFLAGS) -I$(CHEZ_SCHEME_DIR)

io.o: io.c io.h eval.h
	$(CC) -o $@ -c $< $(CFLAGS) -I$(CHEZ_SCHEME_DIR)

parse.o: parse.c parse.h eval.h
	$(CC) -o $@ -c $< $(CFLAGS) -I$(CHEZ_SCHEME_DIR)

posix.o: posix.c posix.h eval.h signal.h
	$(CC) -o $@ -c $< $(CFLAGS) -I$(CHEZ_SCHEME_DIR)

repl.o: repl.c repl.h eval.h
	$(CC) -o $@ -c $< $(CFLAGS) -I$(CHEZ_SCHEME_DIR)

signal.o: signal.c signal.h eval.h posix.h
	$(CC) -o $@ -c $< $(CFLAGS) -I$(CHEZ_SCHEME_DIR)

shell.o: shell.c shell.h container.h eval.h io.h lineedit.h parse.h posix.h signal.h
	$(CC) -o $@ -c $< $(CFLAGS) -I$(CHEZ_SCHEME_DIR) -DCHEZ_SCHEME_DIR="$(CHEZ_SCHEME_DIR)"

test.o: test.c test.h shell.h
	$(CC) -o $@ -c $< $(CFLAGS) -I$(CHEZ_SCHEME_DIR)


schemesh: main.o $(OBJS)
	$(CC) -o $@ $^ -L$(CHEZ_SCHEME_DIR) $(LIBS)

schemesh_test: test.o $(OBJS)
	$(CC) -o $@ $^ -L$(CHEZ_SCHEME_DIR) $(LIBS)
