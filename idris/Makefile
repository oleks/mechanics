.PHONY: all clean

IDR:=$(shell find -iname '*.idr')

all: Main.bin

%.bin: %.idr $(IDR) Makefile
	idris -o $@ $<

%.ibc: %.idr Makefile
	idris --check $<

clean:
	rm -f *.ibc
	rm -f *.bin
