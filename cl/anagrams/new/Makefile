all: $(patsubst %.l,%.fas,$(wildcard *.l))

%.fas %.lib: %.l
	@(clisp --quiet -c $< && clisp --quiet $@) > /dev/null
	@echo $< tests passed.

clean:
	-rm -f *.fas *.lib

.PHONY: clean
