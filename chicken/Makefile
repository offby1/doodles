hello: hello.scm
	csc -profile $^

%.c: %.scm
	chicken $^

check: hello
	./$^

clean:
	rm -f hello PROFILE

.PHONY: check
