all: $(patsubst %.l,%.fas,$(wildcard *.l))

dict.fas: bag.fas
anagrams.fas: bag.fas dict.fas

%.fas %.lib: %.l
	@(clisp --quiet -c $< && clisp --quiet $@) > /dev/null
	@echo $< tests passed.

clean:
	-rm -f *.fas *.lib

.PHONY: clean
