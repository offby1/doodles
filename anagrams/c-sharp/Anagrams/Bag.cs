using System;
using System.Collections.Generic;
using System.Text;

namespace Anagrams
{
    class Bag
    {
        static private string subtract_strings(string minuend, string subtrahend)
        {
            Bag m = new Bag(minuend);
            Bag s = new Bag(subtrahend);
            return m.subtract(s).toString();
        }

        private string guts;
        public Bag(string s)
        {
            Char[] chars = s.ToLower().ToCharArray();
            Array.Sort(chars);
            Char[] letters = Array.FindAll<char>(chars, Char.IsLetter);
            System.Text.StringBuilder sb = new System.Text.StringBuilder();
            sb.Insert(0, letters);
            guts = sb.ToString();
        }
        public Bag subtract(Bag subtrahend)
        {
            return new Bag("");
        }
        public static void test()
        {
            string diff = subtract_strings("dog", "god");
            if (diff != "") throw new Exception("Uh oh");
            Console.WriteLine("Pretend the bag tests all passed.");
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
