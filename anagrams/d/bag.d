import mpz;

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

  return 0;
}
 
