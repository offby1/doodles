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
        private string word_file_name;

        public Form1()
        {
            InitializeComponent();
        }

        private void button1_Click(object sender, EventArgs e)
        {
            if (word_file_name == null)
            {
                // pulling stuff outta the environment like this seems clunky, but it seems to work.
                openFileDialog1.InitialDirectory = Environment.GetEnvironmentVariable ("USERPROFILE") + "/doodles/anagrams";
                openFileDialog1.ShowDialog();
                word_file_name = openFileDialog1.FileName;
            }
            (sender as Button).Enabled = false;
            textBox2.Clear();
            progressBar1.Value = 0;
            using (StreamReader sr = new StreamReader(word_file_name))
            {
                String line;
                // Read and display lines from the file until the end of 
                // the file is reached.
                int linesRead = 0;
                Hashtable stringlists_by_bag = new Hashtable();
                while (ok_to_continue &&
                    (line = sr.ReadLine()) != null)
                {
                    Bag aBag =new Bag(line);
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
                (sender as Button).Enabled = true;
            }
        }

        private void textBox2_TextChanged(object sender, EventArgs e)
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

        private void label1_Click(object sender, EventArgs e)
        {

        }
    }
}
