// -*-c++-*-
#ifndef BAG_H
#define BAG_H

#include <vector>
#include <string>
#include <ostream>

class bag: public std::vector<std::string> {
public:
  bag (const std::string &s);
  bool is_empty () const { return (0 == _letters.size ()); };
  bool operator == (const bag &subtrahend) const
  {
    bag *b = this->subtract_bag (subtrahend);
    bool rv = b && b->is_empty ();
    delete b;
    return rv;
  }
  bool operator < (const bag &other) const
  {
    return (this->_letters < other._letters);
  }

  // returns 0 if b2 cannot be subtracted from b1
  bag*  subtract_bag (const bag &b2) const;

  operator const std::string () const { return "Bag `" + _letters + "'"; }
  const char * c_str () const { return _letters.c_str (); }

  inline friend std::ostream &
  operator <<(std::ostream &o, const bag &b)
  {
    o << static_cast<std::string>(b);
    return o;
  }

private:
  std::string _letters;
};

#endif
