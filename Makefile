Y = TigressLexer.x
YH = TigressLexer.hsx
YHS = TigressLexer.hs
OBJS = TigressLexer.o TigressToken.o
EXEC = mtigress

$(EXEC) : *.hs $(YHS)
	ghc -Wall -O2 -o $@ Main.hs

%.o : %.hs
	ghc -Wall -O2 -c $*.hs

%.hs : %.y
	happy $*.y -o $*.hs
	cp $*.hs $*.hsy
%.hs : %.x
	alex $*.x -o $*.hs
	cp $*.hs $*.hsx
clean :
	rm -rf *.o *.hi $(YH) $(YHS) $(EXEC) *~

