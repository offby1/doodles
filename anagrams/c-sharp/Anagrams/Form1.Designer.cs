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
            this.OutputArea = new System.Windows.Forms.TextBox();
            this.openFileDialog1 = new System.Windows.Forms.OpenFileDialog();
            this.textBox1 = new System.Windows.Forms.TextBox();
            this.label3 = new System.Windows.Forms.Label();
            this.statusStrip1 = new System.Windows.Forms.StatusStrip();
            this.toolStripProgressBar1 = new System.Windows.Forms.ToolStripProgressBar();
            this.toolStripStatusLabel1 = new System.Windows.Forms.ToolStripStatusLabel();
            this.toolStripStatusLabel_strings_read = new System.Windows.Forms.ToolStripStatusLabel();
            this.toolStripStatusLabel_bags_read = new System.Windows.Forms.ToolStripStatusLabel();
            this.do_some_pruning = new System.Windows.Forms.Button();
            this.input = new System.Windows.Forms.TextBox();
            this.statusStrip1.SuspendLayout();
            this.SuspendLayout();
            // 
            // OutputArea
            // 
            this.OutputArea.Location = new System.Drawing.Point(64, 76);
            this.OutputArea.Multiline = true;
            this.OutputArea.Name = "OutputArea";
            this.OutputArea.ReadOnly = true;
            this.OutputArea.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
            this.OutputArea.Size = new System.Drawing.Size(340, 94);
            this.OutputArea.TabIndex = 2;
            this.OutputArea.TabStop = false;
            // 
            // openFileDialog1
            // 
            this.openFileDialog1.FileName = "words";
            this.openFileDialog1.ReadOnlyChecked = true;
            this.openFileDialog1.Title = "Dictionary";
            // 
            // textBox1
            // 
            this.textBox1.Location = new System.Drawing.Point(15, 35);
            this.textBox1.Name = "textBox1";
            this.textBox1.ReadOnly = true;
            this.textBox1.ScrollBars = System.Windows.Forms.ScrollBars.Both;
            this.textBox1.Size = new System.Drawing.Size(439, 20);
            this.textBox1.TabIndex = 8;
            this.textBox1.TabStop = false;
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(12, 9);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(138, 13);
            this.label3.TabIndex = 9;
            this.label3.Text = "Most anagrams found so far";
            // 
            // statusStrip1
            // 
            this.statusStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.toolStripProgressBar1,
            this.toolStripStatusLabel1,
            this.toolStripStatusLabel_strings_read,
            this.toolStripStatusLabel_bags_read});
            this.statusStrip1.Location = new System.Drawing.Point(0, 304);
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
            this.do_some_pruning.Location = new System.Drawing.Point(382, 231);
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
            this.input.Location = new System.Drawing.Point(15, 231);
            this.input.Name = "input";
            this.input.Size = new System.Drawing.Size(361, 20);
            this.input.TabIndex = 1;
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(463, 326);
            this.Controls.Add(this.input);
            this.Controls.Add(this.do_some_pruning);
            this.Controls.Add(this.statusStrip1);
            this.Controls.Add(this.label3);
            this.Controls.Add(this.textBox1);
            this.Controls.Add(this.OutputArea);
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

        private System.Windows.Forms.TextBox OutputArea;
        private System.Windows.Forms.OpenFileDialog openFileDialog1;
        private System.Windows.Forms.TextBox textBox1;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.StatusStrip statusStrip1;
        private System.Windows.Forms.ToolStripProgressBar toolStripProgressBar1;
        private System.Windows.Forms.ToolStripStatusLabel toolStripStatusLabel1;
        private System.Windows.Forms.ToolStripStatusLabel toolStripStatusLabel_strings_read;
        private System.Windows.Forms.ToolStripStatusLabel toolStripStatusLabel_bags_read;
        private System.Windows.Forms.Button do_some_pruning;
        private System.Windows.Forms.TextBox input;
    }
}

