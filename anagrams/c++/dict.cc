#include <stdexcept>
#include <iostream>
#include <fstream>
#include <sstream>
#include <regexx.hh>
#include <map>
#include <cstring>

#include "dict.h"

bool
entry::operator < (const entry &other) const
 {
    bool rv = ((this->second.at(0).size () > other.second.at(0).size ())
               ||
               (this->second.at(0).size () == other.second.at(0).size ()
                &&
                this->second.at(0) < other.second.at(0)));
    return rv;
  }

namespace {
  regexx::Regexx has_a_vowel;
  regexx::Regexx is_long_enough;
  regexx::Regexx contains_non_ascii;

  bool
  acceptable (const bag&filter,
              const bag &candidate)
  {
    has_a_vowel.str (candidate);
    is_long_enough.str (candidate);
    bag *difference = filter.subtract_bag (candidate);
    bool rv = (has_a_vowel.exec ()
               &&
               is_long_enough.exec ()
               &&
               !contains_non_ascii.exec ()
               &&
               difference
               );
    delete difference;
    return rv;
  }
}

typedef std::map<bag, wordlist> hash_t;

void
init (const bag &filter)
{
  has_a_vowel.expr        ("[aeiou]");
  is_long_enough.expr     ("^(i|a)$|^..");
  contains_non_ascii.expr ("[^[:alpha:]]");

  hash_t  hash;
  
  const std::string dict_file_name ("/usr/share/dict/words");
  std::ifstream words (dict_file_name.c_str ());
  if (!words)
    {
      std::ostringstream whine;
      whine << "Can't open dictionary `";
      whine << dict_file_name;
      whine << "' because ";
      whine << strerror (errno);
      throw std::runtime_error (whine.str ());
    }
  while (words)
    {
      std::string one_string;
      words >> one_string;

      for (int i = 0; i < one_string.size (); i++)
        {one_string.at (i) = tolower (one_string.at (i));}

      bag one_bag (one_string);
      if (acceptable (filter,
                      one_bag))
        {
          hash_t::iterator existing = hash.find(one_bag);
          if (existing == hash.end ())
            {
              wordlist singleton;
              singleton.push_back (one_string);

              hash.insert (entry (one_bag, singleton));
            }
          else
            {
              bool duplicate = false;
              for (wordlist::const_iterator i = existing->second.begin ();
                   !duplicate && i != existing->second.end ();
                   i++)
                {
                  if (*i == one_string)
                    {
                      duplicate = true;
                    }
                }
              if (!duplicate)
                {
                  existing->second.push_back (one_string);
                  hash.insert (entry (existing->first, existing->second));
                }
            }
        }
    }

  // now convert the hash to a list.
  for (hash_t::const_iterator i = hash.begin ();
       i != hash.end ();
       i++)
    {
      the_dictionary.push_back (entry (i->first, i->second));
    }

  sort (the_dictionary.begin (),
        the_dictionary.end ());
  
  std::cout << "hash           hath " << hash          .size () << " elements" << std::endl;
  std::cout << "the_dictionary hath " << the_dictionary.size () << " elements" << std::endl;
}

#if 0
int
main ()
{
  bag b ("hey");
  init (b);
}
#endif
