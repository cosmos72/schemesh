
CFLAGS=-g -Wall -W

CHEZ_SCHEME_DIR=/usr/local/lib/csv9.5.8/ta6le

all: schemesh

clean:
	rm -f *~ *.o schemesh

main.o: main.c posix.h
	$(CC) -c $< -o $@ $(CFLAGS) -I$(CHEZ_SCHEME_DIR) -DCHEZ_SCHEME_DIR="$(CHEZ_SCHEME_DIR)"

posix.o: posix.c
	$(CC) -c $< -o $@ $(CFLAGS) -I$(CHEZ_SCHEME_DIR)

hash_iterator.o: hash_iterator.c
	$(CC) -c $< -o $@ $(CFLAGS) -I$(CHEZ_SCHEME_DIR)

schemesh: hash_iterator.o posix.o main.o
	$(CC) $^ -o $@ -L$(CHEZ_SCHEME_DIR) -lkernel -lz -llz4 -lm -lncurses -luuid
