all:
	bnfc Func.cf
	alex -g LexFunc.x
	happy -gca ParFunc.y
	happy -i ParFunc.y
	ghc --make TestFunc.hs -o TestFunc
	ghc --make lab3.hs -o lab3
clean:
	-rm -f lab3
	-rm -f TestFunc
	-rm -f ParFunc.info
	-rm -f DocFunc.ps

distclean: clean
	-rm -f DocFunc.* LexFunc.* ParFunc.* LayoutFunc.* SkelFunc.* PrintFunc.* TestFunc.* AbsFunc.* TestFunc ErrM.* SharedString.* Func.dtd XMLFunc.* lab3 *.o *.hi 

