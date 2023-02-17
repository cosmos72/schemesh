
CFLAGS=-g -Wall -W

CHEZ_SCHEME_DIR=/usr/local/lib/csv9.5.8/ta6le

all: schemesh schemesh_test

clean:
	rm -f *~ *.o schemesh schemesh_test

eval.o: eval.c eval.h
	$(CC) -c $< -o $@ $(CFLAGS) -I$(CHEZ_SCHEME_DIR) -DCHEZ_SCHEME_DIR="$(CHEZ_SCHEME_DIR)"

iterator.o: iterator.c iterator.h main.h
	$(CC) -c $< -o $@ $(CFLAGS) -I$(CHEZ_SCHEME_DIR)

main.o: main.c main.h eval.h iterator.h posix.h
	$(CC) -c $< -o $@ $(CFLAGS) -I$(CHEZ_SCHEME_DIR)

posix.o: posix.c posix.h main.h
	$(CC) -c $< -o $@ $(CFLAGS) -I$(CHEZ_SCHEME_DIR)

test.o: test.c test.h eval.h iterator.h posix.h
	$(CC) -c $< -o $@ $(CFLAGS) -I$(CHEZ_SCHEME_DIR)


schemesh: iterator.o posix.o eval.o main.o
	$(CC) $^ -o $@ -L$(CHEZ_SCHEME_DIR) -lkernel -lz -llz4 -lm -lncurses -luuid

schemesh_test: iterator.o posix.o eval.o test.o
	$(CC) $^ -o $@ -L$(CHEZ_SCHEME_DIR) -lkernel -lz -llz4 -lm -lncurses -luuid
