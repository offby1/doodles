%.class: %.java
	javac $<

%.h: %.class
	javah $(basename $< .class)

C := HelloWorld
DLL := Hello

ifeq ($(OS),Windows_NT)
J2 := c:/j2sdk1.4.2_05
MSVC := d:/devstudio/vc98
CPPFLAGS := -I$(J2)/include -I$(J2)/include/win32 -I$(MSVC)/include

%.obj: %.c 
	cl -c $(CPPFLAGS) $<

%.dll: %.obj
	LIB=$(MSVC)/lib cl -LD $< -Fe$*.dll

endif

all: $(C).class $(DLL).dll

$(DLL).obj : $(DLL).c $(C).h

check: all
	test "`java $(C)`" = "Hello world!"

clean:
	rm -f *.class *.dll *.exp *.lib *.obj
	-find . -name '*.h' -type f -print0 | xargs -0 egrep -l 'machine generated' | xargs --no-run-if-empty rm -v

.PHONY: all check clean

