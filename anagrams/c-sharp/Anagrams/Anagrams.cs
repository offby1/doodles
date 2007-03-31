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
            foreach (List<string> a in ans)
            {
                foreach (string word in ws)
                {
                    List<string> bigger_anagram = new List<string>();
                    bigger_anagram.InsertRange(0, a);
                    bigger_anagram.Add(word);
                    rv.Add(bigger_anagram);
                }
            }
            {
                int expected = ws.Count * ans.Count;

                if (rv.Count != expected) throw new Exception(String.Format(
                    "Expected {0} anagrams, but got {1}",
                    expected, rv.Count));
            }
            return rv;
        }
        public static List<List<string>> anagrams(Bag bag,
            List<bag_and_anagrams> dictionary,
            uint recursion_level,
            Form1.zero_arg_del prune_callback,
            Form1.one_arg_del success_callback)
        {
            List<List<string>> rv = new List<List<string>>();
            List<bag_and_anagrams> pruned = Form1.prune(bag,
                dictionary,
                prune_callback,
                recursion_level);
            while (pruned.Count > 0)
            {
                bag_and_anagrams entry = pruned[0];
                Bag this_bag = entry.b;
                Bag diff = bag.subtract(this_bag);
                if (diff != null)
                {
                    if (diff.empty())
                    {
                        foreach (string w in entry.words)
                        {
                            List<string> loner = new List<string>();
                            loner.Add(w);
                            rv.Add(loner);
                        }
                    }
                    else
                    {
                        List<List<string>> from_smaller = anagrams(diff, pruned, recursion_level + 1,
                            prune_callback, success_callback);
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

                foreach (List<string> anagram in rv)
                {
                    success_callback(anagram);
                }
            }
            return rv;
        }
    }
}
