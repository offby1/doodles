#include <vector>
#include <string>
#include <hash_map>
#include <iostream>
#include <fstream>
#include <list>
#include <cstring>
#include <set>

using namespace std;

namespace {
typedef vector<const string *> strvec;

enum states { unknown, pending };

struct eqstr
{
  bool operator()(const char* s1, const char* s2) const
  {
    return strcmp(s1, s2) == 0;
  }
};

ostream & operator<< (ostream &o, const strvec &sv)
{
  for (strvec::const_iterator i = sv.begin ();
       i != sv.end ();
       i++)
    {
      o << *i;
    }
  return o;
}

typedef hash_map<const char*, int, hash<const char*>, eqstr> wordbag;

wordbag dictionary;

void make_pending (const string *word)
{
  dictionary [word->c_str ()] = pending;
}

void make_unknown (const string *word)
{
  dictionary [word->c_str ()] = unknown;
}

const char *finish;
size_t finish_length;

bool is_word (const string &thing)
{
  bool rv = (dictionary.find (thing.c_str ())
             !=
             dictionary.end ());
  cerr << "`"
       << thing
       << "' "
       << (rv? "is" : "is not")
       << " a word"
       << endl;
  return rv;
}

void snarf_dictionary (const string &file_name)
{
  ifstream inf (file_name.c_str ());
  if (!inf) { cerr << "Can't open " << file_name << endl; exit(1); }
  while (!inf.eof ())
    {
      string s;
      inf >> s;
      if (s.size () == finish_length)
        {
          //cerr << "Inserting `" << s << "'" << endl;
          char *tmp (new char [s.size ()]);
          strcpy (tmp, s.c_str ());
          dictionary [tmp] = unknown;
        }
    }

}

string variant_of_one_letter (const string &word,
                              unsigned position,
                              unsigned offset)
{
  string rv (word);
  char ch = rv [position];
  ch -= 'a';
  ch += offset;
  ch %= 26;
  ch += 'a';
  rv [position] = ch;
  //cerr << word << " " << position << " " << offset << ": " << rv << endl;
  return rv;
}

strvec all_variants_of_position (const string &word,
                                 unsigned position)
{
  strvec return_value;
  for (unsigned chars_tried = 0;
       chars_tried < 25;
       chars_tried++)
    {
      string *one_variant (new string (variant_of_one_letter (word,
                                                              position,
                                                              chars_tried + 1)));
      return_value.push_back (one_variant);
    }

  return return_value;
}

strvec generate_neighbor_strings (const string &word)
{
  strvec return_value;
  for (unsigned chars_to_twiddle = word.size ();
       chars_to_twiddle;
       chars_to_twiddle--)
    {
      strvec variants (all_variants_of_position (word,
                                                 chars_to_twiddle - 1));
      return_value.insert (return_value.end (),
                           variants.begin (),
                           variants.end ());
    }
  return return_value;
}

strvec find_neighbors (const string &word)
{
  strvec return_value (generate_neighbor_strings (word));

  for (strvec::iterator i = return_value.begin ();
       i != return_value.end ();
       i++)
    {
      if (dictionary.find ((*i)->c_str ()) == dictionary.end ())
	{
	  delete *i;
          return_value.erase (i);
	}
    }

  return return_value;
}

void internal_word_chain (const strvec &path_to_here)
{

  strvec return_value;

  if (0 == strcmp (path_to_here[0]->c_str (), finish))
    {
      cout << path_to_here << endl;
      return;
    }
  
  if (
      (dictionary.find ((path_to_here[0])->c_str ()))->second 
      == unknown)
    {
      make_pending (path_to_here[0]);
      strvec neighbors (find_neighbors (*(path_to_here[0])));

      for (strvec::const_iterator neighbor = neighbors.begin ();
           neighbor != neighbors.end ();
           neighbor++)
        {
          strvec new_path (path_to_here);
          new_path.insert (new_path.begin (),
                           *neighbor);

          internal_word_chain (new_path);
        }
      
      for (strvec::const_iterator neighbor = neighbors.begin ();
           neighbor != neighbors.end ();
           neighbor++)
        {
          delete *neighbor;
        }
      
      make_unknown (path_to_here[0]);
    }
}

void word_chain (const char *start)
{
  if (strlen (start) != strlen (finish))
    {
      cerr << start
           << " and "
           << finish
           << " aren't the same length"
           << endl;
      exit (1);
    }

  strvec short_path;
  short_path.push_back ( new string (start));
  internal_word_chain (short_path);
}

};

int
main (int argc, char *argv[])
{

  if (argc != 3)
    {
      cerr << "Usage: "
           << argv[0]
           << " start finish"
           << endl;
      exit (1);
    }

  snarf_dictionary (
                    //"/tmp/x"
                    "/usr/share/dict/words"
                    );

  finish = argv[2];
  finish_length = strlen (finish);

  dictionary [argv[1]] = unknown;
  dictionary [finish] = unknown;

  word_chain (argv [1]);
}

// Local variables:
// compile-command: "make CXXFLAGS=\"-g -Wall\" word-chain"
// end:
