
CFLAGS=-g -Wall -W

CHEZ_SCHEME_DIR=/usr/local/lib/csv9.5.8/ta6le

all: schemesh schemesh_test

clean:
	rm -f *~ *.o schemesh schemesh_test

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


schemesh: main.o container.o eval.o io.o lineedit.o parse.o posix.o repl.o shell.o signal.o
	$(CC) -o $@ $^ -L$(CHEZ_SCHEME_DIR) -lkernel -lz -llz4 -lm -lncurses -luuid

schemesh_test: test.o container.o eval.o io.o lineedit.o parse.o posix.o repl.o shell.o signal.o
	$(CC) -o $@ $^ -L$(CHEZ_SCHEME_DIR) -lkernel -lz -llz4 -lm -lncurses -luuid
