using System;
using System.Collections.Generic;
using System.Text;

namespace Anagrams
{

    class Bag
    {
        private static bool IsLetter(char c)
        {
            return char.IsLetter(c);
        }

        private char[] letters;
        public Bag(string s)
        {
            char [] chars = s.ToLower(). ToCharArray();
            Array.Sort(chars);
            letters = Array.FindAll<char>(chars, IsLetter);
            Console.Write(s + " -> " );
            Console.WriteLine(letters);
        }

        public static void test ()
        {
            Console.WriteLine ("Pretend the bag tests all passed.");
        }
    }
}
