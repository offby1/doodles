sources := packages.scm boink.scm

image := boink.image

$(image): build-image.scm $(sources)
	echo ,exec ,load $< | scheme48 -a batch
	[ -r $(image) ]

check: $(image)
	scheme48 -h 3516784 -i $< -a foo bar baz

clean:
	-rm -f $(image)

.PHONY: check clean
