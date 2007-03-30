using System;
using System.Collections.Generic;
using System.Text;

namespace Anagrams
{
    class Anagrams
    {
        public static List<List<string>> anagrams(Bag bag, List<bag_and_anagrams> dictionary)
        {
            Console.WriteLine("Pretend I'm generating some anagrams for {0}", bag.AsString());
            List<List<string>> rv = new List<List<string>>();
            List<string> one = new List<string>();
            one.Add("Hey you.");
            rv.Add(one);
            return rv;
        }
    }
}
