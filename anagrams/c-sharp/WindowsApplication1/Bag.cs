using System;
using System.Collections.Generic;
using System.Text;

namespace Anagrams
{

    class Bag
    {
        public Char[] letters;
        public Bag(string s)
        {
            Char [] chars = s.ToLower(). ToCharArray();
            Array.Sort(chars);
            letters = Array.FindAll<char>(chars, Char.IsLetter);
        }

        public static void test ()
        {
            Console.WriteLine ("Pretend the bag tests all passed.");
        }

        public string toString()
        {
            System.Text.StringBuilder sb = new System.Text.StringBuilder();
            sb.Insert(0, letters);
            return sb.ToString();
        }
    }
}
