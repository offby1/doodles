#encoding_weirdness	:= -Efile latin1

wc:
	clisp -q $(encoding_weirdness) -norc -i $@.system -x "(make:operate-on-system :$@ :compile)"

clean:
	-rm *.fas *.lib

.PHONY: clean wc
