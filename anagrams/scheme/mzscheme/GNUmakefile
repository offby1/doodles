check: words
	time ./anagrams.scm  "Ernest Hemingway" > /dev/null

words: /usr/share/dict/words
	ln -sv $< $@

profile:
	time ./profile.ss  "Ernest Hemingway" > $@

.PHONY: check
