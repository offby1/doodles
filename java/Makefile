JAVAC := javac
JAVAH := javah

%.class: %.java
	$(JAVAC) $<

%.h: %.class
	$(JAVAH) $(basename $< .class)

lib%.so: %.o
	$(CC) $(LDFLAGS) $^ -shared -o $@

CLASSES	:= HelloWorld CallBack
LIB	:= Hello

CPPFLAGS:= -I/usr/local/include -I/usr/local/include/linux
LDFLAGS	:= -shared

all: $(patsubst %,%.class,$(CLASSES)) lib$(LIB).so

$(LIB).o : $(LIB).c $(patsubst %,%.h,$(CLASSES))

check: all
	LD_LIBRARY_PATH=. java HelloWorld
	@echo All tests passed.

clean:
	rm -f *.class *.so *.exp *.lib *.o
	-find . -name '*.h' -type f -print0 | xargs -0 egrep -l 'machine generated' | xargs --no-run-if-empty rm -v

.PHONY: all check clean

