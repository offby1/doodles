import std.stream;
import bag;

class dictionary
{
  // this sucks.  I originally defined the map to have _bags_ as keys,
  // not strings.  But when I did that, the `in' expression always
  // yielded false.  Apparently associative arrays don't work as
  // expected with my bag class ... but since strings work, I store
  // them instead.
  char [][] [char[]] string_map;
  char [][][bag] bag_map;

  this (Stream s)
  {
    while (!s.eof ())
      {
        char []this_line = s.readLine ();
        bag b = new bag (this_line);
        char[] bag_string = b.toString ();

        if (b in bag_map)
          {
            bag_map[b] ~= this_line;
          }
        else
          {
            char [][] new_wordlist;
            new_wordlist ~= this_line;
            bag_map[b] = new_wordlist;
          }
        
        if (bag_string in string_map)
          {
            string_map[bag_string] ~= this_line;
          }
        else
          {
            char [][] new_wordlist;
            new_wordlist ~= this_line;
            string_map[bag_string] = new_wordlist;
          }
      }
  }

  int size ()
  {
    //return bag_map.length;
    return string_map.length;
  }
  
  unittest
  {
    printf ("Snarfing a test dictionary; patience! ... \n");
    alias char[] strings;
    strings test_lines;

    test_lines ~= "Hey\n";
    test_lines ~= "You\n";
    test_lines ~= "cuz\n";
    test_lines ~= "what\n";
    test_lines ~= "up\n";
    test_lines ~= "zuc\n";
    test_lines ~= "cuz\n";
    test_lines ~= "what\n";

    TArrayStream! (char[]) s = new TArrayStream! (char[]) (test_lines);
    dictionary words = new dictionary (s);
    printf ("done\n");
    assert (5 == words.size ());

    foreach (char [] key; words.string_map.keys)
      {
        printf ("Key `%.*s' => ", key );
        char [][] words = words.string_map[key];
        foreach (int i, char [] word; words)
          {
            if (i > 0)
              {
                printf (", ");
              }
            printf ("`%.*s'", word);
          }
        printf ("\n");
      }
    printf ("Dictionary unit tests passed\n");
  }
}
