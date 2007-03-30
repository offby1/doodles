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

        private void update_last_line(string lastline, Bag aBag)
        {
            OutputArea.AppendText(lastline);
            OutputArea.AppendText(" -> ");
            OutputArea.AppendText(aBag.AsString());
            OutputArea.AppendText("\n");
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
            OutputArea.Clear();
            toolStripProgressBar1.Value = 0;
            toolStripProgressBar1.Maximum = (int)wordlist_stream.Length;
            using (StreamReader sr = new StreamReader(wordlist_stream))
            {
                String line;
                // Read and display lines from the file until the end of 
                // the file is reached.
                int linesRead = 0;
                int length_of_longest_list_so_far = 0;
                Hashtable stringlists_by_bag = new Hashtable();
                while (ok_to_continue &&
                    (line = sr.ReadLine()) != null)
                {
                    Bag aBag = new Bag(line);
                    if (!stringlists_by_bag.ContainsKey(aBag.AsString()))
                    {
                        List<string> l = new List<String>();
                        l.Add(line);
                        stringlists_by_bag.Add(aBag, l);
                    }
                    else
                    {
                        List<string> l = (List<string>)stringlists_by_bag[aBag.AsString()];
                        l.Add(line);
                        if (l.Count > length_of_longest_list_so_far)
                        {
                            length_of_longest_list_so_far = l.Count;
                            textBox1.Text = "";
                            foreach (string s in l)
                            {
                                textBox1.Text += s + " ";
                            }
                        }
                    }
                    linesRead++;
                    toolStripProgressBar1.Increment(line.Length + 1); // the +1 is for the line ending character, I'd guess.
                    if (linesRead % 1000 == 0)
                    {
                        update_counts(stringlists_by_bag.Count, linesRead);
                        update_last_line(line, aBag);
                    }
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
        }

        private void do_some_pruning_Click(object sender, EventArgs e)
        {
            Bag input_bag = new Bag(input.Text);
            OutputArea.Clear();
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
                    foreach (string s in pair.words)
                    {
                        OutputArea.AppendText(" ");
                        OutputArea.AppendText(s);
                    }
                    OutputArea.AppendText("\n");
                }
                toolStripProgressBar1.PerformStep();
                Application.DoEvents();
            }
            toolStripStatusLabel1.Text += " done.";
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
