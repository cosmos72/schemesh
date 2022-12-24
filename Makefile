
CFLAGS=-g -Wall -W

CHEZ_SCHEME_DIR=/usr/local/lib/csv9.5.8/ta6le

all: schemesh

clean:
	rm -f main.o posix.o schemesh

main.o: main.c posix.h
	$(CC) -c $< -o $@ $(CFLAGS) -I$(CHEZ_SCHEME_DIR) -DCHEZ_SCHEME_DIR="$(CHEZ_SCHEME_DIR)"

posix.o: posix.c
	$(CC) -c $< -o $@ $(CFLAGS) -I$(CHEZ_SCHEME_DIR)

schemesh: main.o posix.o
	$(CC) $^ -o $@ -L$(CHEZ_SCHEME_DIR) -lkernel -lz -llz4 -lm -lncurses -luuid
