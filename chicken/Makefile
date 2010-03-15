hello: hello.scm
	csc -profile $^

%.c: %.scm
	chicken $^

check: hello
	time ./$^

clean:
	rm -f hello PROFILE

.PHONY: check
