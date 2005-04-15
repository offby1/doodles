String asList := method (result := list; self foreach (i, c, result add (c asCharacter)); result)
String sortChars := method ("" join (self asList sort))
"testing:"           print; "\n" print
"testing:" sortChars print; "\n" print

f := File setPath ("/usr/share/dict/words") openForReading

# work around a bug
evil_words_to_skip := list ("activate", "clone", "debug", "delegate", "forward", "parent", "return", "self", "sender", "target")

while(line := f readLine,

  if (evil_words_to_skip contains (line), 
    Nil,
    line print
    ": " print
    line asString sortChars print
    "\n" print
  )
)
