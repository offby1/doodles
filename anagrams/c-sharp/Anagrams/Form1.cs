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

        // this is a kludge.  Without this variable, if I click the
        // "close" button (the little X in the upper-right of the
        // window), I get an exception to the effect that textBox2 has
        // been "disposed", and thus I shouldn't be appending text to
        // it.  I suspect there's a cleaner way to avoid that
        // exception than this, but what the hell.

        private bool ok_to_continue = true;

        public Form1()
        {
            InitializeComponent();
        }

        private void button1_Click(object sender, EventArgs e)
        {
        }

        private void Form1_FormClosed(object sender, FormClosedEventArgs e)
        {
            ok_to_continue = false;
        }

        private void Form1_Load(object sender, EventArgs e)
        {
            Bag.test();
        }

        private void update_counts(int bags, int linesRead)
        {
            toolStripStatusLabel_bags_read.Text = bags.ToString() + " bags";
            toolStripStatusLabel_strings_read.Text = linesRead.ToString() + " words";
        }

        private void update_last_line(string lastline, Bag aBag, int this_lines_index)
        {
            string display_me = lastline + " -> " + aBag.AsString();
            listView1.Items.Add(display_me);
            listView1.EnsureVisible(listView1.Items.Count - 1);
        }

        private void Form1_Shown(object sender, EventArgs e)
        {
            System.IO.Stream wordlist_stream;
            System.Reflection.Assembly thisExe;
            thisExe = System.Reflection.Assembly.GetExecutingAssembly();
            wordlist_stream =
                thisExe.GetManifestResourceStream("Anagrams.words");
            if (wordlist_stream == null)
            {
                throw new Exception("Uh oh, can't find word list inside myself!");
            }
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
                while (ok_to_continue &&
                    (line = sr.ReadLine()) != null)
                {
                    line = line.ToLower();
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
                    if (linesRead % 1000 == 0)
                    {
                        update_counts(stringlists_by_bag.Count, linesRead);
                        update_last_line(line, aBag, linesRead / 1000 - 1);
                    }

#if DEBUG
                    if (linesRead == 10000) break;
#endif
                    Application.DoEvents();
                }
                update_counts(stringlists_by_bag.Count, linesRead);

                // Now convert the hash table, which isn't useful for
                // actually generating anagrams, into a list, which is.

                dictionary = new List<bag_and_anagrams>();
                foreach (DictionaryEntry de in stringlists_by_bag)
                {
                    dictionary.Add(new bag_and_anagrams((Bag)de.Key, (List<string>)de.Value));
                }
            }
            toolStripStatusLabel1.Text = "Compiling dictionary ... done.";

            do_some_pruning.Enabled = true;
            input.Enabled = true;
            input.Focus();
        }

        private void do_some_pruning_Click(object sender, EventArgs e)
        {
            Bag input_bag = new Bag(input.Text);
            Anagrams.anagrams(input_bag, dictionary);
            listView1.Items.Clear();
            toolStripStatusLabel_bags_read.Text = "";
            toolStripStatusLabel_strings_read.Text = "";
            toolStripStatusLabel1.Text = "Pruning for '" + input.Text + "' ...";
            toolStripProgressBar1.Value = 0;
            toolStripProgressBar1.Maximum = dictionary.Count;
            foreach (bag_and_anagrams pair in dictionary)
            {
                Bag this_bag = pair.b;
                if (input_bag.subtract(this_bag) != null)
                {
                    string display_me = "";
                    foreach (string s in pair.words)
                    {
                        if (display_me.Length > 0) display_me += " ";
                        display_me += s;
                    }
                    listView1.Items.Add(display_me);
                    listView1.EnsureVisible(listView1.Items.Count - 1);

                }
                toolStripProgressBar1.PerformStep();
                Application.DoEvents();
            }
            toolStripStatusLabel1.Text += " done.";
        }

        private void input_KeyPress(object sender, KeyPressEventArgs e)
        {
            if (e.KeyChar == (char)Keys.Enter)
                do_some_pruning.PerformClick();
        }

    }
    // each entry is a bag followed by words that can be made from that bag.

    class bag_and_anagrams
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
