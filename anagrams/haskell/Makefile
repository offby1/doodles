check: hello
	./$^

hello: hello.hs
	ghc $^ -o $@

clean:
	-rm hello

.PHONY: clean check
