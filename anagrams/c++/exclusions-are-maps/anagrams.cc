#include <iostream>
#include <sstream>
#include <cassert>
#include <map>
#include "numeric-bag.h"
#include "dict.h"

std::vector<entry > the_dictionary;

// TODO -- figure out what to use other than a map -- perhaps there's
// a set.
typedef std::map<bag, bool>  excls;

namespace {
  std::ostream &
  operator <<(std::ostream &o, const wordlist &w)
  {
    for (wordlist::const_iterator i = w.begin ();
         i != w.end ();
         i++)
      {
        o << *i;
        if (i + 1 != w.end ())
          o << " ";
      }
    return o;
  }

  std::ostream &
  operator <<(std::ostream &o, const std::vector<wordlist> &anagrams)
  {
    o << "(";
    for (std::vector<wordlist>::const_iterator i = anagrams.begin ();
         i != anagrams.end ();
         i++)
      {
        o << *i;
        if (i + 1 != anagrams.end ())
          o << " ";
      }
    o << ")";
    return o;
  }


  inline bool
  excluded (const bag &b, const excls &e)
  {
    return  (e.find(b) != e.end ());
  }

  excls
  copy_exclusions (const excls e)
  {
    excls rv;
    for (excls::const_iterator i = e.begin ();
         i != e.end ();
         i++)
      {
        rv.insert (std::pair<bag, bool>(i->first, i->second));
      }

    return rv;
  }

  std::string
  wordlist_to_string (const wordlist &wl)
  {
    std::ostringstream o;
    o << wl;
    return o.str ();
  }

  std::vector<wordlist>
  prepend_words_to_anagrams (const wordlist &these_words,
                             const std::vector<wordlist> &some_anagrams)
  {
    assert (these_words.size ());
    assert (some_anagrams.size ());

    std::vector<wordlist> rv;
    for (wordlist::const_iterator word = these_words.begin ();
         word != these_words.end ();
         word++)
      {
        for (std::vector<wordlist>::const_iterator an = some_anagrams.begin ();
             an != some_anagrams.end ();
             an++)
          {
            wordlist one_new_anagram;
            one_new_anagram.push_back (*word);
            for (wordlist::const_iterator i = an->begin ();
                 i != an->end ();
                 i++)
              one_new_anagram.push_back (*i);
            rv.push_back (one_new_anagram);
          }
      }

    return rv;
  }
}

std::ostream &
operator <<(std::ostream &o, const entry &e)
{
  o << "["
    << e.first
    << " => "
    << e.second
    << "]";
  return o;
}


std::vector<wordlist>
anagrams_internal (const bag &b,
                   const excls &given_exclusions,
                   unsigned int level)
{
  std::vector<wordlist> rv;
  excls exclusions (copy_exclusions (given_exclusions));
  bag *smaller_bag = 0;
  for (std::vector<entry>::const_iterator i = the_dictionary.begin ();
       i != the_dictionary.end ();
       i++)
    {
      delete smaller_bag; smaller_bag = 0;

      const bag &key_bag (i->first);
      const wordlist &these_words (i->second);

      if (excluded (key_bag, exclusions))
        {
          continue;
        }

      smaller_bag = (b.subtract_bag (key_bag));
      if (!smaller_bag)
        continue;

      if (smaller_bag->is_empty ())
        {
          for (wordlist::const_iterator wd = these_words.begin ();
               wd != these_words.end ();
               wd++)
            {
              wordlist result;
              result.push_back (*wd);
              if (false && !level)
                std::cerr << result << std::endl;
              rv.push_back (result);
            }
        }
      else
        {
          std::vector<wordlist> from_smaller_bag (anagrams_internal (*smaller_bag, exclusions, level + 1));
          if (!from_smaller_bag.size ())
            continue;

          std::vector<wordlist> more (prepend_words_to_anagrams (these_words, from_smaller_bag));
          for (std::vector<wordlist>::const_iterator i = more.begin ();
               i != more.end ();
               i++)
            {
              if (false && !level)
                std::cerr << *i << std::endl;
              rv.push_back (*i);
            }
        }
      exclusions.insert (std::pair<bag, bool>(*smaller_bag, true));
    }

  return rv;
}

std::vector<wordlist>
all_anagrams (const bag &b)
{
  std::cerr << "Snarfing the dictionary and whatnot ... ";
  init (b);
  std::cerr << the_dictionary.size ()
            << " items left in dictionary"
            << std::endl;
  return anagrams_internal (b, excls (), 0);
}

#if 1
int
main (int argc, char *argv[])
{
  try
    {
      if (argc > 1)
        {
          argc--;
          argv++;

          const std::string input = argv[0];
          const bag b (input);

          std::vector<wordlist> ans (all_anagrams (b));
          std::cerr << ans.size ()
                    << " anagrams of "
                    << input
                    << std::endl;
          for (std::vector<wordlist>::const_iterator i = ans.begin ();
               i != ans.end ();
               i++)
            {
              std::cout << *i << std::endl;
            }
        }
    }
  catch (std::exception &e)
    {
      std::cerr << e.what () << std::endl;
    }
}
#endif

