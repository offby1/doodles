C := Factorial

%.class: %.java
	javac $<

all: $(C).class

check: all
	test `java $(C) 10` = 3628800

clean:
	rm -f *.class

.PHONY: all check clean

