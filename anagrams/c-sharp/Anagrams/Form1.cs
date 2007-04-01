using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;

namespace Anagrams
{
    public partial class Form1 : Form
    {
        List<bag_and_anagrams> dictionary;

        public Form1()
        {
            InitializeComponent();
        }

        private void button1_Click(object sender, EventArgs e)
        {
        }

        private void Form1_FormClosed(object sender, FormClosedEventArgs e)
        {
            Environment.Exit(0);
        }

        private void Form1_Load(object sender, EventArgs e)
        {
            Bag.test();
        }

        // this is a filter for entires in the original word list.  It rejects words that have no vowels, and those that are too short.
        private bool acceptable(string s)
        {
            if (s.Length < 2)
            {
                if (s == "i" || s == "a")
                    return true;
                return false;
            }
            char[] vowels = { 'a', 'e', 'i', 'o', 'u', 'y' };
            if (s.IndexOfAny(vowels, 0) > -1)
                return true;
            return false;
        }

        private void Form1_Shown(object sender, EventArgs e)
        {
            System.IO.Stream wordlist_stream;
            System.Reflection.Assembly thisExe;
            thisExe = System.Reflection.Assembly.GetExecutingAssembly();
            wordlist_stream =
                thisExe.GetManifestResourceStream("Anagrams.words");
            System.Diagnostics.Trace.Assert(wordlist_stream != null,
                "Uh oh, can't find word list inside myself!");
            toolStripStatusLabel1.Text = "Compiling dictionary ...";
            toolStripProgressBar1.Value = 0;
            toolStripProgressBar1.Maximum = (int)wordlist_stream.Length;
            using (StreamReader sr = new StreamReader(wordlist_stream))
            {
                String line;
                // Read and display lines from the file until the end of 
                // the file is reached.
                int linesRead = 0;
                Hashtable stringlists_by_bag = new Hashtable();
                while ((line = sr.ReadLine()) != null)
                {
                    // TODO -- filter out nonletters.  Thus "god's"
                    // should become "gods".  And since both of those
                    // are likely to appear, we need to ensure that we
                    // only store one.
                    line = line.ToLower();
                    if (!acceptable(line)) continue;
                    Bag aBag = new Bag(line);
                    if (!stringlists_by_bag.ContainsKey(aBag))
                    {
                        List<string> l = new List<String>();
                        l.Add(line);
                        stringlists_by_bag.Add(aBag, l);
                    }
                    else
                    {
                        List<string> l = (List<string>)stringlists_by_bag[aBag];
                        if (!l.Contains(line)) l.Add(line);
                    }
                    linesRead++;
                    toolStripProgressBar1.Increment(line.Length + 1); // the +1 is for the line ending character, I'd guess.

#if DEBUG
                    //if (linesRead == 10000) break;
#endif
                    Application.DoEvents();
                }

                // Now convert the hash table, which isn't useful for
                // actually generating anagrams, into a list, which is.

                dictionary = new List<bag_and_anagrams>();
                foreach (DictionaryEntry de in stringlists_by_bag)
                {
                    dictionary.Add(new bag_and_anagrams((Bag)de.Key, (List<string>)de.Value));
                }
            }
            toolStripStatusLabel1.Text = "Compiling dictionary ... done.";
            listView1.Enabled = true;
            do_anagrams.Enabled = true;
            input.Enabled = true;
            input.Focus();
        }

        private void anagrams_Click(object sender, EventArgs e)
        {
            do_anagrams.Enabled = false;
            input.Enabled = false;
            Bag input_bag = new Bag(input.Text);
            listView1.Items.Clear();
            DateTime start = DateTime.Now;
            Anagrams.anagrams(input_bag, dictionary, 0,
                delegate(Bag filter, List<bag_and_anagrams> dict)
                {
                    toolStripStatusLabel1.Text = "Pruning for '" + filter.AsString() + "' ...";
                    toolStripProgressBar1.Value = 0;
                    toolStripProgressBar1.Maximum = dict.Count;
                },
                delegate()
                {
                    toolStripProgressBar1.PerformStep();
                    Application.DoEvents();
                },
                delegate()
                {
                    toolStripStatusLabel1.Text = "";
                },
                delegate(List<string> words)
                {
                    string display_me = "";
                    foreach (string s in words)
                    {
                        if (display_me.Length > 0) display_me += " ";
                        display_me += s;
                    }
                    listView1.Items.Add(display_me);
                    listView1.EnsureVisible(listView1.Items.Count - 1);
                    toolStripStatusLabel1.Text = listView1.Items.Count.ToString() + " anagrams so far";
                    Application.DoEvents();
                });
            DateTime stop = DateTime.Now;
            toolStripStatusLabel1.Text = String.Format("Done.  {0} anagrams; took {1}.",
                listView1.Items.Count,
                stop.Subtract(start));

            do_anagrams.Enabled = true;
            input.Enabled = true;
            input.Focus();
        }

        private void input_KeyPress(object sender, KeyPressEventArgs e)
        {
            if (e.KeyChar == (char)Keys.Enter)
                do_anagrams.PerformClick();

            // This smells.  I want to trap Control-A, so that I can
            // select all the text in the input box (control-A does
            // just that in other contexts, but not here, for some
            // reason).  But I don't know the politically-correct way
            // to spell Control-A, so I just use 1.
            if (e.KeyChar == (char)1)
                input.SelectAll();
        }

        private void listView1_Resize(object sender, EventArgs e)
        {
            // trial and error shows that we must make the column
            // header four pixels narrower than the containing
            // listview in order to avoid a scrollbar.
            listView1.Columns[0].Width = listView1.Width - 4;
        }

        private void listView1_SelectedIndexChanged(object sender, EventArgs e)
        {
            Clipboard.Clear();

            string selected_text = "";
            ListView me = (ListView)sender;
            foreach (ListViewItem it in me.SelectedItems)
            {
                if (selected_text.Length > 0)
                    selected_text += Environment.NewLine;
                selected_text += it.Text;
            }
            // Under some circumstances -- probably a bug in my code somewhere --
            // we can get blank lines in the listview.  And if you click on one, since it
            // has no text, selected_text will be blank; _and_, apparantly, calling
            // Clipboard.set_text with an empty string provokes an access violation ...
            // so avoid that AV.
            if (selected_text.Length > 0) Clipboard.SetText(selected_text);
        }
    }
    // each entry is a bag followed by words that can be made from that bag.

    public class bag_and_anagrams
    {
        public Bag b;
        public List<string> words;

        // *sigh* this is tediously verbose
        public bag_and_anagrams(Bag b, List<string> words)
        {
            this.b = b;
            this.words = words;
        }
    }
}
