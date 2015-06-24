all: strabo

strabo: strabo.c
	$(CC) strabo.c -O2 -o strabo -Wall -W -lm

clean:
	rm -rf strabo
