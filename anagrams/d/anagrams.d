import bag;
import dictionary;
import std.stream;

dictionary the_dictionary;

int main()
{
  BufferedFile s = new BufferedFile (
                                     "/usr/share/dict/words"
                                     //"one_percent_dict"
                                     );
  printf ("Snarfing big dictionary ...\n");
  the_dictionary = new dictionary (s, "Ernest");
  printf ("done\n");

  return 0;
}
