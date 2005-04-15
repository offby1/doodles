# keys are characters; values are the number of times that character
# appeared in the original string.

String asMap  := method (
  result := Map clone
  self foreach(i, c, 
    ch := c asCharacter
    if (result hasKey (ch),
      result atPut (ch, 1 + result at (ch)),
      result atPut (ch, 1)) 
    result))

Map asString := method (
  result := String clone
  self foreach (char, count, 
    for (n, 0, count -1,
      result := result append (char)))
  result)

Nil asString := method ("Nil!")

f := File setPath ("/usr/share/dict/words") openForReading

evil_words_to_skip := list ("activate", "clone", "debug", "delegate", "forward", "parent", "return", "self", "sender", "target")

while(line := f readLine,
  if (evil_words_to_skip contains (line) == Nil, 
    line print
    ": " print
    line asString asMap asString linePrint))
