TCFLAGS =
LLI = x86_64-lli

.PHONY : clean
%.ll : %.tiger
	cabal run tigress-emit -- $*.tiger -o $*.ll $(TCFLAGS)
%.test : %.ll
	$(LLI) -force-interpreter $*.ll

clean :
	rm -f *.ll
