all:
	ghc --make main.hs s_world.hs s_utils.hs -o the_sword

clean:
	rm -rf *.hi the_sword *.o
