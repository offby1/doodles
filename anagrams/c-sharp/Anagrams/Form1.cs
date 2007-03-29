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
            textBox2.Clear();
            progressBar1.Value = 0;
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
                    if (!stringlists_by_bag.ContainsKey(aBag.toString()))
                    {
                        List<string> l = new List<String>();
                        l.Add(line);
                        stringlists_by_bag.Add(aBag, l);
                    }
                    else
                    {
                        List<string> l = (List<string>)stringlists_by_bag[aBag.toString()];
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
                    progressBar1.PerformStep();
                    if (linesRead % 1000 == 0)
                    {
                        bags_textBox.Text = stringlists_by_bag.Count.ToString();
                        strings_textBox.Text = linesRead.ToString();
                        textBox2.AppendText(linesRead.ToString());
                        textBox2.AppendText(": ");
                        textBox2.AppendText(line);
                        textBox2.AppendText(" -> ");
                        textBox2.AppendText(aBag.toString());
                        textBox2.AppendText("\n");
                    }
                    Application.DoEvents();
                }
            }

        }
    }
}
