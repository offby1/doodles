sources := packages.scm boink.scm

image := boink.image

check:
	(echo ,config ,load packages.scm; echo  ,open boink; echo '(boink)') | scheme48 -a batch

$(image): build-image.scm $(sources)
	echo ,exec ,load $< | scheme48 -a batch

check2: $(image)
	scheme48 -h 3516784 -i $< -a foo bar baz

clean:
	-rm -f $(image)

.PHONY: check clean
