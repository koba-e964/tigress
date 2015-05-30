TCFLAGS=

.PHONY : clean
%.ll : %.tiger
	cabal run tigress-emit -- $*.tiger -o $*.ll $(TCFLAGS)

clean :
	rm -f *.ll
