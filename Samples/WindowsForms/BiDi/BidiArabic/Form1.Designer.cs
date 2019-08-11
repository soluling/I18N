namespace BiDiArabic
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
      System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(Form1));
      this.checkBox1 = new System.Windows.Forms.CheckBox();
      this.browseButton = new System.Windows.Forms.Button();
      this.textBox = new System.Windows.Forms.TextBox();
      this.label1 = new System.Windows.Forms.Label();
      this.SuspendLayout();
      // 
      // checkBox1
      // 
      resources.ApplyResources(this.checkBox1, "checkBox1");
      this.checkBox1.Name = "checkBox1";
      this.checkBox1.UseVisualStyleBackColor = true;
      // 
      // browseButton
      // 
      resources.ApplyResources(this.browseButton, "browseButton");
      this.browseButton.Name = "browseButton";
      this.browseButton.UseVisualStyleBackColor = true;
      this.browseButton.Click += new System.EventHandler(this.browseButton_Click);
      // 
      // textBox
      // 
      resources.ApplyResources(this.textBox, "textBox");
      this.textBox.Name = "textBox";
      // 
      // label1
      // 
      resources.ApplyResources(this.label1, "label1");
      this.label1.Name = "label1";
      // 
      // Form1
      // 
      resources.ApplyResources(this, "$this");
      this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
      this.Controls.Add(this.checkBox1);
      this.Controls.Add(this.browseButton);
      this.Controls.Add(this.textBox);
      this.Controls.Add(this.label1);
      this.Name = "Form1";
      this.ResumeLayout(false);
      this.PerformLayout();

    }

    #endregion

    private System.Windows.Forms.CheckBox checkBox1;
    private System.Windows.Forms.Button browseButton;
    private System.Windows.Forms.TextBox textBox;
    private System.Windows.Forms.Label label1;
  }
}

