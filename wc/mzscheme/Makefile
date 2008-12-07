.PHONY: check
check: dist
	./dist/bin/wc foo bar

dist: wc
	mzc --exe-dir $@ $^

wc: wc.ss
	mzc --exe $@ $^

.PHONY: clean
clean:
	-rm -rf dist wc compiled *.dot
