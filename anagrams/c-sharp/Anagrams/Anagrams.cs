using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Forms;

namespace Anagrams
{
        public delegate void started_pruning(Bag filter, List<bag_and_anagrams> dict);
        public delegate void pruned_one();
        public delegate void done_pruning_callback();
        public delegate void found_anagram(List<string> words);

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

                System.Diagnostics.Trace.Assert(rv.Count == expected,
                    String.Format(
                    "Expected {0} anagrams, but got {1}",
                    expected, rv.Count));
            }
            return rv;
        }
        public static List<List<string>> anagrams(Bag bag,
            List<bag_and_anagrams> dictionary,
            uint recursion_level,
            started_pruning started_pruning_callback,
            pruned_one pruned_one_callback,
            done_pruning_callback done_pruning_callback,
            found_anagram success_callback)
        {
            started_pruning_callback(bag, dictionary);
            List<List<string>> rv = new List<List<string>>();
            List<bag_and_anagrams> pruned = Form1.prune(bag,
                dictionary,
                pruned_one_callback,
                done_pruning_callback,
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
                            started_pruning_callback,
                            pruned_one_callback,
                            done_pruning_callback,
                            success_callback);
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
