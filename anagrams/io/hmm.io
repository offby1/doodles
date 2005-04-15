String asList := method (result := list; self foreach (i, c, result add (c asCharacter)); result)
String sortChars := method ("" join (self asList sort))
"testing:"           print; "\n" print
"testing:" sortChars print; "\n" print

f := File setPath ("/usr/share/dict/words") openForReading

count := 0

while(line := f readLine,
      count := count + 1;

      line asString sortChars print;
      "\n" print;

      if (count == 5, break);
      );

count print;
" total words\n" print;
