#  -i ~/.clisprc.lisp

# Without this, clisp 2.31 breaks when reading the dictionary, as soon
# as it hits a non-ASCII character.  I should probably tell clisp to
# use latin1 in the source code, not this makefile, so that the source
# code will work in any context.  But I don't yet know how to do that
# ...
encoding_weirdness	:= -Efile latin1

anagrams:
	clisp $(encoding_weirdness) -norc -i $@.system -x "(make:operate-on-system :$@ :compile)"

clean:
	-rm *.fas *.lib

.PHONY: clean anagrams
