using System;
using System.Collections.Generic;
using System.Text;

namespace Anagrams
{

    class Bag
    {
        private string guts;
        public Bag(string s)
        {
            Char [] chars = s.ToLower(). ToCharArray();
            Array.Sort(chars);
            Char [] letters = Array.FindAll<char>(chars, Char.IsLetter);
            System.Text.StringBuilder sb = new System.Text.StringBuilder();
            sb.Insert(0, letters);
            guts = sb.ToString();
        }

        public static void test ()
        {
            Console.WriteLine ("Pretend the bag tests all passed.");
        }

        public string toString()
        {
            return guts;
        }
        public override int GetHashCode()
        {
            return guts.GetHashCode();
        }
        public override bool Equals(object obj)
        {
            return guts.Equals(obj);
        }
    }
}
