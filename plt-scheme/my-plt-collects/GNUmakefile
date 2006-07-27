define last-element
$(word $(words $1),$1)
endef

this_makefile := $(call last-element,$(MAKEFILE_LIST))

# Can't use 'notdir', because our argument might contain a space :-|
this_directory := $(shell cd $$(dirname $(this_makefile)); pwd | sed 's,.*/\(.*\),\1,')

sources := $(wildcard *.ss)
PLT_ARCHIVE_NAME := $(this_directory).plt
tmpdir := tmp

#I originally just ran "planet" in the current directory.  But I
#noticed that it descended into the .svn directory, which is bad, and
#it also freaked out on an empty file.
$(PLT_ARCHIVE_NAME): $(sources) $(tmpdir)
	cp -v $(sources) $(tmpdir)
	(cd $(tmpdir) && planet --create-archive .)
	mv $(tmpdir)/*.plt "$@"
	rm -rf $(tmpdir)

install: $(PLT_ARCHIVE_NAME)
	planet --file "$(PLT_ARCHIVE_NAME)" offby1 1 0

uninstall:
	-planet --remove offby1 "$(PLT_ARCHIVE_NAME)"  1 0

clean:
	-rm -f "$(PLT_ARCHIVE_NAME)"

check: uninstall install
	@mzscheme -qmve '(require (planet "howdy.ss" ("offby1" "my-plt-collects.plt"))))'  \
		 | egrep 'Yes!  Module .howdy. got loaded.'

$(tmpdir):
	mkdir -vp $@

.PHONY: install uninstall clean check
