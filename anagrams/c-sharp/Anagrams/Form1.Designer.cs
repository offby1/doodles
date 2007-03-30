namespace Anagrams
{
    partial class Form1
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.openFileDialog1 = new System.Windows.Forms.OpenFileDialog();
            this.statusStrip1 = new System.Windows.Forms.StatusStrip();
            this.toolStripProgressBar1 = new System.Windows.Forms.ToolStripProgressBar();
            this.toolStripStatusLabel1 = new System.Windows.Forms.ToolStripStatusLabel();
            this.toolStripStatusLabel_strings_read = new System.Windows.Forms.ToolStripStatusLabel();
            this.toolStripStatusLabel_bags_read = new System.Windows.Forms.ToolStripStatusLabel();
            this.do_some_pruning = new System.Windows.Forms.Button();
            this.input = new System.Windows.Forms.TextBox();
            this.listView1 = new System.Windows.Forms.ListView();
            this.columnHeader1 = new System.Windows.Forms.ColumnHeader();
            this.statusStrip1.SuspendLayout();
            this.SuspendLayout();
            // 
            // openFileDialog1
            // 
            this.openFileDialog1.FileName = "words";
            this.openFileDialog1.ReadOnlyChecked = true;
            this.openFileDialog1.Title = "Dictionary";
            // 
            // statusStrip1
            // 
            this.statusStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.toolStripProgressBar1,
            this.toolStripStatusLabel1,
            this.toolStripStatusLabel_strings_read,
            this.toolStripStatusLabel_bags_read});
            this.statusStrip1.Location = new System.Drawing.Point(0, 202);
            this.statusStrip1.Name = "statusStrip1";
            this.statusStrip1.Size = new System.Drawing.Size(463, 22);
            this.statusStrip1.TabIndex = 10;
            this.statusStrip1.Text = "statusStrip1";
            // 
            // toolStripProgressBar1
            // 
            this.toolStripProgressBar1.Name = "toolStripProgressBar1";
            this.toolStripProgressBar1.Size = new System.Drawing.Size(100, 16);
            this.toolStripProgressBar1.Step = 1;
            this.toolStripProgressBar1.Style = System.Windows.Forms.ProgressBarStyle.Continuous;
            // 
            // toolStripStatusLabel1
            // 
            this.toolStripStatusLabel1.Name = "toolStripStatusLabel1";
            this.toolStripStatusLabel1.Size = new System.Drawing.Size(109, 17);
            this.toolStripStatusLabel1.Text = "toolStripStatusLabel1";
            // 
            // toolStripStatusLabel_strings_read
            // 
            this.toolStripStatusLabel_strings_read.Name = "toolStripStatusLabel_strings_read";
            this.toolStripStatusLabel_strings_read.Size = new System.Drawing.Size(13, 17);
            this.toolStripStatusLabel_strings_read.Text = "0";
            // 
            // toolStripStatusLabel_bags_read
            // 
            this.toolStripStatusLabel_bags_read.Name = "toolStripStatusLabel_bags_read";
            this.toolStripStatusLabel_bags_read.Size = new System.Drawing.Size(13, 17);
            this.toolStripStatusLabel_bags_read.Text = "0";
            // 
            // do_some_pruning
            // 
            this.do_some_pruning.Enabled = false;
            this.do_some_pruning.Location = new System.Drawing.Point(379, 164);
            this.do_some_pruning.Name = "do_some_pruning";
            this.do_some_pruning.Size = new System.Drawing.Size(75, 23);
            this.do_some_pruning.TabIndex = 0;
            this.do_some_pruning.Text = "Prune!";
            this.do_some_pruning.UseVisualStyleBackColor = true;
            this.do_some_pruning.Click += new System.EventHandler(this.do_some_pruning_Click);
            // 
            // input
            // 
            this.input.Enabled = false;
            this.input.Location = new System.Drawing.Point(12, 164);
            this.input.Name = "input";
            this.input.Size = new System.Drawing.Size(361, 20);
            this.input.TabIndex = 1;
            this.input.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.input_KeyPress);
            // 
            // listView1
            // 
            this.listView1.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.columnHeader1});
            this.listView1.Location = new System.Drawing.Point(12, 13);
            this.listView1.Name = "listView1";
            this.listView1.Size = new System.Drawing.Size(436, 145);
            this.listView1.TabIndex = 11;
            this.listView1.UseCompatibleStateImageBehavior = false;
            this.listView1.View = System.Windows.Forms.View.Details;
            // 
            // columnHeader1
            // 
            this.columnHeader1.Text = "Stuff";
            this.columnHeader1.Width = 300;
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(463, 224);
            this.Controls.Add(this.listView1);
            this.Controls.Add(this.input);
            this.Controls.Add(this.do_some_pruning);
            this.Controls.Add(this.statusStrip1);
            this.Name = "Form1";
            this.Text = "Anagram Thingy";
            this.FormClosed += new System.Windows.Forms.FormClosedEventHandler(this.Form1_FormClosed);
            this.Shown += new System.EventHandler(this.Form1_Shown);
            this.Load += new System.EventHandler(this.Form1_Load);
            this.statusStrip1.ResumeLayout(false);
            this.statusStrip1.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.OpenFileDialog openFileDialog1;
        private System.Windows.Forms.StatusStrip statusStrip1;
        private System.Windows.Forms.ToolStripProgressBar toolStripProgressBar1;
        private System.Windows.Forms.ToolStripStatusLabel toolStripStatusLabel1;
        private System.Windows.Forms.ToolStripStatusLabel toolStripStatusLabel_strings_read;
        private System.Windows.Forms.ToolStripStatusLabel toolStripStatusLabel_bags_read;
        private System.Windows.Forms.Button do_some_pruning;
        private System.Windows.Forms.TextBox input;
        private System.Windows.Forms.ListView listView1;
        private System.Windows.Forms.ColumnHeader columnHeader1;
    }
}

