%.o : %.hs
	ghc -c $^ -o $@

read: Bag.o read.o
	ghc -o $@ $^

clean:
	-rm *.o *.hi read

.PHONY: clean
