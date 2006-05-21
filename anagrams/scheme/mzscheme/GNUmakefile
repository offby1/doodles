check: words
	time ./anagrams.scm  "Hemingway"

words: /usr/share/dict/words
	ln -sv $< $@

profile:
	time ./profile.ss  "Ernest Hemingway" > $@

.PHONY: check
