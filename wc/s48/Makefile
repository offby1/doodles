s48 := scheme48 -h 7000000
sources := $(wildcard *.scm)
image := boink.image

#unfortunatley scheme48 will not return a failure code if it fails to
#build the image, so we need to check by hand.
$(image): build-image.scm $(sources)
	-rm -f $(image)
	echo ,exec ,load $< | $(s48) -a batch
	[ -r $(image) ]		

check: $(image)
	time $(s48) -i $< -a giant raven

clean:
	-rm -f $(image)

.PHONY: check clean
