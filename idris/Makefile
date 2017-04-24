.PHONY: all clean

IDR=$(shell find -iname '*.idr')

all: Main.bin

%.bin: %.idr $(IDR) Makefile
	idris -o $@ $<
