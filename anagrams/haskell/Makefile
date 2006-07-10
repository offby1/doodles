%.o : %.hs
	ghc -c $^ -o $@

check: read
	./$^

read: Bag.o read.o
	ghc -o $@ $^

clean:
	-rm *.o *.hi read

.PHONY: clean check
