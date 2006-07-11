%.o : %.hs
	ghc -O -c $^ -o $@

check: anagrams
	time ./$^

anagrams: Bag.o anagrams.o
	ghc -o $@ $^

clean:
	-rm *.o *.hi anagrams

.PHONY: clean check
