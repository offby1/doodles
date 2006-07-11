%.o : %.hs
	ghc -O -prof -auto-all -c $^ -o $@

prof: anagrams.prof

anagrams.prof: anagrams
	time ./$^ +RTS -p

check: anagrams
	time ./$^

anagrams: Bag.o anagrams.o
	ghc -prof -auto-all -o $@ $^

clean:
	-rm *.o *.hi anagrams anagrams.prof

.PHONY: clean check prof
