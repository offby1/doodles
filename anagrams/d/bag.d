import mpz;
import std.string;
import std.ctype;

int primes[26] = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101];

class bag
{
  mpz product;
  int opEquals (int i)
  {
    return this.product == i;
  }
  this (char[] s)
  {
    product = new mpz (1);

    printf ("New bag: input was `%.*s'; ", s);

    while (s.length)
      {
        char c = s[0];
        if (isalpha (c))
          {
            product *= primes[std.ctype.tolower (c) - 'a'];
          }
        s = s[1 .. s.length];
      }

    printf ("product is %.*s\n", product.toString ());
  }
  bool is_empty ()
  {
    return (1 == product ? true : false);
  }
  unittest
  {
    assert (2 == new bag ("a"));
    assert (6 == new bag ("ab"));
    assert (6 == new bag ("ba"));
  }
}

int main()
{
  int i = 0;
  mpz sam = new mpz (2);

  for (i = 0;
       i < 7;
       i++)
    {
      printf ("%.*s\n", sam.toString ());
      sam *= sam;
    }

  bag ted = new bag ("Ernest Hemingway");
  
  return 0;
}
