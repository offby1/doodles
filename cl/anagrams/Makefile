all: cmucl clisp

cmucl: $(patsubst %.l,%.x86f,$(wildcard *.l))
clisp: $(patsubst %.l,%.fas,$(wildcard *.l))

dict.% : bag.%

anagrams.% : bag.% dict.%

%.x86f: %.l
	@lisp -eval "(progn (compile-file \"$<\") (quit))"

%.fas %.lib: %.l
	@clisp -c $<

clean:
	-rm -f *.fas{,l} *.lib *.x86f

.PHONY: clean all cmucl clisp
