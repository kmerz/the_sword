all:
	ghc --make main.hs -o the_sword

clean:
	rm -rf main.hi the_sword main.o
