.PHONY: all
all: message.html

 %.html: %.scrbl
	mzscheme -t "$^" > "$@"

.DELETE_ON_ERROR:
