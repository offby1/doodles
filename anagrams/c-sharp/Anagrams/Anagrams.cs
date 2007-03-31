using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Forms;

namespace Anagrams
{
    class Anagrams
    {
        private static List<List<string>> combine(List<string> ws, List<List<string>> ans)
        {
            List<List<string>> rv = new List<List<string>>();
            List<List<string>> anagrams = new List<List<string>>();
            anagrams.InsertRange(0, ans);
            List<string> words = new List<string>();
            words.InsertRange(0, ws);
            foreach (List<string> a in anagrams)
            {
                List<string> anagram = new List<string>();
                anagram.InsertRange(0, a);
                foreach (string word in words)
                {
                    anagram.Add(word);
                }
                rv.Add(anagram);
            }
            return rv;
        }
        public static List<List<string>> anagrams(Bag bag,
            List<bag_and_anagrams> dictionary,
            int recursion_level,
            Form1.zero_arg_del usual_callback,
            Form1.one_arg_del success_callback)
        {
            List<List<string>> rv = new List<List<string>>();
            List<bag_and_anagrams> pruned = Form1.prune(bag, dictionary);
            while (pruned.Count > 0)
            {
                bag_and_anagrams entry = pruned[0];
                Bag this_bag = entry.b;
                Bag diff = bag.subtract(this_bag);
                if (diff != null)
                {
                    if (diff.empty())
                    {
                        rv.Add(entry.words);
                    }
                    else
                    {
                        List<List<string>> from_smaller = anagrams(diff, pruned, recursion_level + 1,
                            usual_callback, success_callback);
                        if (from_smaller.Count > 0)
                        {
                            rv.AddRange(combine(entry.words, from_smaller));
                        }
                    }
                }
                pruned.RemoveAt(0);
                Application.DoEvents();
            }
            if (recursion_level == 0)
            {
                usual_callback();
                foreach (List<string> anagram in rv)
                {
                    success_callback(anagram);
                }
            }
            return rv;
        }
    }
}
