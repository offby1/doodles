String asList := method (result := list clone; self foreach (i, c, result add (c asCharacter)); result)
String sortChars := method ("" join (self asList sort))
"testing:"           print; "\n" print
"testing:" sortChars print; "\n" print

f := File setPath ("/usr/share/dict/words") openForReading

while(line := f readLine,
  line asString sortChars print
  "\n" print
)
