all: cmucl clisp

cmucl: $(patsubst %.l,%.x86f,$(wildcard *.l))
clisp: $(patsubst %.l,%.fas,$(wildcard *.l))

dict.fas : bag.fas
dict.x86f: bag.x86f

anagrams.fas : bag.fas dict.fas
anagrams.x86f: bag.x86f dict.x86f

%.x86f: %.l
	@lisp -eval "(progn (compile-file \"$<\") (quit))"

%.fas %.lib: %.l
	@clisp -c $<

clean:
	-rm -f *.fas *.lib *.x86f

.PHONY: clean all cmucl clisp
