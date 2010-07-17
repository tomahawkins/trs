.PHONY: gcd
gcd: gcd.v

.PHONY: sorter
sorter: sorter.v

.PHONY: doc
doc:
	haddock -o doc --html *.hs Language/*.hs Language/TRS/*.hs

build_gcd: Examples/*.hs Language/*.hs Language/TRS/*.hs
	ghc --make -W -iExamples -o build_gcd Examples/GCDExample.hs

gcd.v gcd_stimulus.v: build_gcd
	time ./build_gcd +RTS -K32M -RTS | tee build_gcd.log   # -xc

build_sorter: Examples/*.hs Language/*.hs Language/TRS/*.hs
	ghc --make -W -iExamples -o build_sorter Examples/Sorter.hs

sorter.v sorter_stimulus.v: build_sorter
	time ./build_gcd +RTS -K32M -RTS | tee build_sorter.log   # -xc



#	runghc -W -iExamples Examples/GCDExample.hs

.PHONY: clean
clean:
	-rm build_gcd*
	-rm build_sorter*
	-rm doc/*.*
	-rm gcd.*
	-rm gcd_stimulus.*
	-rm sorter.*
	-rm sorter_stimulus.*
	-rm build_gcd.*
	-rm verilog.log
	-rm -rf *.o
	-rm -rf *.hi
	-rm -rf Language/*.o
	-rm -rf Language/*.hi
	-rm -rf Language/TRS/*.o
	-rm -rf Language/TRS/*.hi
	-rm -rf Examples/*.o
	-rm -rf Examples/*.hi
	-rm -rf .*
	-rm -rf INCA_libs
	-rm -rf *.log
	-rm -rf *.key
	-rm -rf SSout
	-cd LDPC && make clean

