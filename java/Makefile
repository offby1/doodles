%.class: %.java
	javac $<

%.h: %.class
	javah $(basename $< .class)

lib%.so: %.o
	$(CC) $(LDFLAGS) $^ -shared -o $@

CLASS	:= HelloWorld
LIB	:= Hello

CPPFLAGS:= -I/usr/local/include -I/usr/local/include/linux
LDFLAGS	:= -shared

all: $(CLASS).class lib$(LIB).so

$(LIB).o : $(LIB).c $(CLASS).h

check: all
	test "`LD_LIBRARY_PATH=. java $(CLASS)`" = "Hello world!"
	@echo All tests passed.

clean:
	rm -f *.class *.so *.exp *.lib *.o
	-find . -name '*.h' -type f -print0 | xargs -0 egrep -l 'machine generated' | xargs --no-run-if-empty rm -v

.PHONY: all check clean

