dict := /usr/share/dict/words

check: bag-example
	./bag-example |  sed 's/:.*$$//' > hmm
	diff $(dict) hmm

bag-example: bag.scm bag-example.scm
	gsc $^
	gcc -o $@ bag.c bag-example.c bag-example_.c -lgambc -lm -lutil -ldl 

clean:
	rm -f *.c bag-example hmm

.PHONY: clean check
