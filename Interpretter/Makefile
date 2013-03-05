all:
	bnfc Func.cf
	happy -gca ParFunc.y
	alex -g LexFunc.x
	ghc --make lab3.hs -o lab3
clean:
	-rm -f lab3
	-rm -f DocFunc.ps

distclean: clean
	-rm -f DocFunc.* LexFunc.* ParFunc.* LayoutFunc.* SkelFunc.* PrintFunc.* TestFunc.* AbsFunc.* TestFunc ErrM.* SharedString.* Func.dtd XMLFunc.*

