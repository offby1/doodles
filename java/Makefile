USE_GCJ := yes

ifdef USE_GCJ
JAVA  := gij
JAVAC := /usr/bin/javac
JAVAH := gcjh -jni
else
JAVA  := java
JAVAC := javac
JAVAH := javah
endif

CLASSES	:= HelloWorld CallBack
LIB	:= Hello

ifdef USE_GCJ
# Unfortunately if I simply say `gcj HelloWorld.java' it complains
# about `Cannot find class "CallBack" [JLS 8]'
$(word 1,$(CLASSES)).class: $(patsubst %,%.java,$(CLASSES))
	$(JAVAC) $^
else
%.class: %.java
	$(JAVAC) $<
endif

%.h: %.class
	$(JAVAH) $(basename $< .class)

lib%.so: %.o
	$(CC) $(LDFLAGS) $^ -shared -o $@

CPPFLAGS:= -I/usr/local/include -I/usr/local/include/linux
LDFLAGS	:= -shared

all: $(patsubst %,%.class,$(CLASSES)) lib$(LIB).so

$(LIB).o : $(LIB).c $(patsubst %,%.h,$(CLASSES))

check: all
	LD_LIBRARY_PATH=. $(JAVA) HelloWorld
	@echo All tests passed.

clean:
	rm -f *.class *.so *.exp *.lib *.o
	-find . -maxdepth 1 -name '*.h' -type f -print0 | xargs -0 egrep -l 'machine generated' | xargs --no-run-if-empty rm -v

.PHONY: all check clean

