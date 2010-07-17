.PHONY: all
all: ldpc_decoder_802_3an.v

build_ldpc: *.hs ../Language/*.hs ../Language/TRS/*.hs
	ghc --make -W -i.. -o build_ldpc LDPC.hs

ldpc_decoder_802_3an.v: build_ldpc
	time ./build_ldpc | tee build_ldpc.log   # -xc

.PHONY: clean
clean:
	-rm build_ldpc*
	-rm ldpc_*
	-rm *.svg
	-rm *.tcl
	-rm *.o
	-rm *.hi
	-rm -rf .*
	-rm -rf INCA_libs
	-rm -rf *.log
	-rm -rf *.key
	-rm -rf SSout

