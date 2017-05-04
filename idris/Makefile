.PHONY: all clean

PKGS:=-p contrib
IDR:=$(shell find -iname '*.idr')

all: Main.bin

%.bin: %.idr $(IDR) Makefile
	idris $(PKGS) -o $@ $<

%.ibc: %.idr $(IDR) Makefile
	idris $(PKGS) --check $<

clean:
	rm -f *.ibc
	rm -f *.bin
