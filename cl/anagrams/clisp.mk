#  -i ~/.clisprc.lisp

# Without this, clisp 2.31 breaks when reading the dictionary, as soon
# as it hits a non-ASCII character.
encoding_weirdness	:= -Efile iso-latin-1

anagrams:
	clisp $(encoding_weirdness) -norc -i $@.system -x "(make:operate-on-system :$@ :compile)"

clean:
	-rm *.fas *.lib

.PHONY: clean anagrams
