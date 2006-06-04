#We're going to make a fairly large image, so we need to tell scheme48
#to reserve a fairly large amount of memory for its heap.
s48 := scheme48 -h 7000000

sources := $(wildcard *.scm)
image := wc.image

#unfortunately scheme48 will not return a failure code if it fails to
#build the image, so we need to check by hand.
$(image): build-image.exec $(sources)
	-rm -f $(image)
	echo ,exec ,load $< | $(s48) -a batch
	[ -r $(image) ]		

check: $(image)
	time -p $(s48) -i $< -a giant raven

clean:
	-rm -f $(image)

.PHONY: check clean
