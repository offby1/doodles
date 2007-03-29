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
            return m.subtract(s).AsString();
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
        private static void test_subtraction(string minuend, string subtrahend, string expected_difference)
        {
            string actual_difference = subtract_strings("dog", "god");
            if (actual_difference != expected_difference)
                throw new Exception("Test failure: "
                    + "Subtracting `" + subtrahend 
                    + "' from `" + minuend 
                    + "' yielded `" + actual_difference
                    + "', but should have yielded `" + expected_difference + "'.");
        }
        public static void test()
        {
            test_subtraction("dog", "god", "");
            test_subtraction("ddog", "god", "d");
            Console.WriteLine("Pretend the bag tests all passed.");
        }

        public string AsString()
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
