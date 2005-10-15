define last-element
$(word $(words $1),$1)
endef

this_makefile := $(call last-element,$(MAKEFILE_LIST))
this_directory := $(notdir $(shell (cd $$(dirname $(this_makefile)); pwd)))
PLT_ARCHIVE_NAME := $(this_directory).plt

$(PLT_ARCHIVE_NAME): $(wildcard *.ss)
	planet --create-archive .

install: $(PLT_ARCHIVE_NAME)
	planet --file $(PLT_ARCHIVE_NAME) offby1 1 0

uninstall:
	-planet --remove offby1 $(PLT_ARCHIVE_NAME)  1 0

clean:
	-rm -f $(PLT_ARCHIVE_NAME)

check: uninstall install
	@mzscheme -qmve '(require (planet "howdy.ss" ("offby1" "my-plt-collects.plt"))))'  \
		 | egrep 'Yes!  Module .howdy. got loaded.'

.PHONY: install uninstall clean check
