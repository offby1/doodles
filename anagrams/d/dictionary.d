import std.stream;
import bag;

class dictionary
{
  int [char []][bag] hoo_map;

  this (Stream s)
  {
    while (!s.eof ())
      {
        char []this_line = s.readLine ();
        bag b = new bag (this_line);
        hoo_map[b][this_line]++;
      }
  }

  int size ()
  {
    return hoo_map.length;
  }
  
  char[][] lookup (bag b)
  {
    return hoo_map[b].keys;
  }
  bag[] keys ()
  {
    return hoo_map.keys;
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
    {
      char[][] say_what = words.lookup (new bag ("what"));
      char [] rendered;
      foreach (int i, char [] s; say_what)
        {
          if (i > 0)
            rendered ~= ";";
          rendered ~= s;
        }
      printf ("Word `what' hath %d anagrams: %.*s\n", say_what.length, rendered);
      foreach (int i, bag b; words.hoo_map.keys)
        {
          foreach (char []s; words.hoo_map[b].keys)
            {
              printf ("%.*s\n", s);
            }
        }
      assert (1 == say_what.length);
    }
    foreach (bag key; words.keys ())
      {
        printf ("Key `%.*s' => ", key.toString ());
        char [][] words = words.lookup(key);
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
