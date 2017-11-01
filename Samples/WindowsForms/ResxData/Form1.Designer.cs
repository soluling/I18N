namespace ResxData
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
      this.label1 = new System.Windows.Forms.Label();
      this.pictureBox = new System.Windows.Forms.PictureBox();
      this.playButton = new System.Windows.Forms.Button();
      this.stringXmlLabel = new System.Windows.Forms.Label();
      this.binaryXmlLabel = new System.Windows.Forms.Label();
      this.simpleXmlLabel = new System.Windows.Forms.Label();
      ((System.ComponentModel.ISupportInitialize)(this.pictureBox)).BeginInit();
      this.SuspendLayout();
      // 
      // label1
      // 
      resources.ApplyResources(this.label1, "label1");
      this.label1.Name = "label1";
      // 
      // pictureBox
      // 
      resources.ApplyResources(this.pictureBox, "pictureBox");
      this.pictureBox.Name = "pictureBox";
      this.pictureBox.TabStop = false;
      // 
      // playButton
      // 
      resources.ApplyResources(this.playButton, "playButton");
      this.playButton.Name = "playButton";
      this.playButton.UseVisualStyleBackColor = true;
      this.playButton.Click += new System.EventHandler(this.playButton_Click);
      // 
      // stringXmlLabel
      // 
      resources.ApplyResources(this.stringXmlLabel, "stringXmlLabel");
      this.stringXmlLabel.Name = "stringXmlLabel";
      // 
      // binaryXmlLabel
      // 
      resources.ApplyResources(this.binaryXmlLabel, "binaryXmlLabel");
      this.binaryXmlLabel.Name = "binaryXmlLabel";
      // 
      // simpleXmlLabel
      // 
      resources.ApplyResources(this.simpleXmlLabel, "simpleXmlLabel");
      this.simpleXmlLabel.Name = "simpleXmlLabel";
      // 
      // Form1
      // 
      resources.ApplyResources(this, "$this");
      this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
      this.Controls.Add(this.simpleXmlLabel);
      this.Controls.Add(this.binaryXmlLabel);
      this.Controls.Add(this.stringXmlLabel);
      this.Controls.Add(this.playButton);
      this.Controls.Add(this.pictureBox);
      this.Controls.Add(this.label1);
      this.Name = "Form1";
      this.Load += new System.EventHandler(this.Form1_Load);
      ((System.ComponentModel.ISupportInitialize)(this.pictureBox)).EndInit();
      this.ResumeLayout(false);
      this.PerformLayout();

    }

    #endregion

    private System.Windows.Forms.Label label1;
    private System.Windows.Forms.PictureBox pictureBox;
    private System.Windows.Forms.Button playButton;
    private System.Windows.Forms.Label stringXmlLabel;
    private System.Windows.Forms.Label binaryXmlLabel;
    private System.Windows.Forms.Label simpleXmlLabel;
  }
}

