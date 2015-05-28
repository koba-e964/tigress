%.ll : %.tiger
	cabal run tigress-emit -- $*.tiger -o $*.ll

