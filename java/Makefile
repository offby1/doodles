%.class: %.java
	javac $<

%.h: %.class
	javah $(basename $< .class)

C := HelloWorld
DLL := Hello

ifeq ($(OS),Windows_NT)
O  	 := .obj
SO 	 := .dll
J2 	 := c:/j2sdk1.4.2_05
MSVC 	 := d:/devstudio/vc98
CPPFLAGS := -I$(J2)/include -I$(J2)/include/win32 -I$(MSVC)/include

%$(O): %.c 
	cl -c $(CPPFLAGS) $<

%$(SO): %$(O)
	LIB=$(MSVC)/lib cl -LD $< -Fe$*$(SO)

else
CPPFLAGS := -I/usr/local/include -I/usr/local/include/linux
CC	:= gcc
LDFLAGS	:= -shared
SO	:= .so
O	:= .o

%$(SO): %$(O)
	$(CC) $(LDFLAGS) $^ -shared -o lib$@
endif

all: $(C).class $(DLL)$(SO)

$(DLL)$(O) : $(DLL).c $(C).h

check: all
	test "`LD_LIBRARY_PATH=. java $(C)`" = "Hello world!"
	@echo All tests passed.

clean:
	rm -f *.class *$(SO) *.exp *.lib *$(O)
	-find . -name '*.h' -type f -print0 | xargs -0 egrep -l 'machine generated' | xargs --no-run-if-empty rm -v

.PHONY: all check clean

