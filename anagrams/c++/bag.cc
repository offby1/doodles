#include <cassert>
#include <string>
#include <iostream>
#include <algorithm>

#include "bag.h"

bag::bag (const std::string &s)
{
  for (int i = 0;
       i < s.size ();
       i++)
    {
      if (isalpha (s.at (i)))
        _letters += (tolower (s.at(i)));
    }
  sort (_letters.begin (), _letters.end ());
}

bag*
bag::subtract_bag (const bag &subtrahend) const
{
  std::string b1(this->_letters);
  std::string b2 (subtrahend._letters);

  std::string difference;
  for (;;
       b1 = b1.substr (1), b2 = b2.substr (1)) 
    {
      if (!b2.size ())
        return new bag (difference + b1);

      if (!b1.size ())
        return 0;

      if (b1.at (0) != b2.at (0))
        {
          char c1 = b1.at(0);
          char c2 = b2.at(0);

          while ((c1 < c2))
            {
              if (!b1.size ())
                return 0;

              difference += c1;
              
              b1 = b1.substr (1);
              if (!b1.size ())
                return 0;

              c1 = b1.at (0);
            }

          if (c1 > c2)
            return 0;
        }  
    }
  bag *rv = new bag (difference);
  assert (rv->size () == this->size () - subtrahend.size ());
  return rv;
}
#if 0
int
main ()
{
#if 0
  assert (bag ("HEY") == bag ("hey"));

  assert (bag ("").is_empty ());


  assert (!bag ("a").is_empty ());

  assert (bag ("abc") == bag ("cba")) ;


  assert (!(bag ("abc") == bag ("bc")));
#endif
  assert (!(bag ("abc") == bag ("a")));

  {
    bag *oughta_be_empty (bag ("a").subtract_bag (bag ("a")));
    assert (oughta_be_empty);
    assert (oughta_be_empty->is_empty ());
  }

  bag *b = 0;

  // oh how I miss garbage collection.
  b = bag("ab").subtract_bag (bag ("b"));
  assert (bag ("a") == *b) ;
  delete b; b = 0;

  assert (!bag ("a").subtract_bag (bag ("b")));
  assert (!bag ("a").subtract_bag (bag ("aa")));


  const std::string silly_long_string = "When first I was a wee, wee lad Eating on my horse I had to take a farting duck Much to my remorse. Oh Sally can't you hear my plea When Roe V Wade is nigh And candles slide the misty morn With dimples on your tie.";

  const std::string ever_so_slightly_longer_string (silly_long_string + "x");

  b = bag (ever_so_slightly_longer_string).subtract_bag (bag (silly_long_string));
  assert (*b == bag ("x"));
  delete b; b = 0;

  std::cout << "We cool!\n";
}
#endif
