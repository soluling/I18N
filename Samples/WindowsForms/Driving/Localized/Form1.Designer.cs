namespace Driving
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
      this.menuStrip1 = new System.Windows.Forms.MenuStrip();
      this.fileToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
      this.calculateToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
      this.toolStripMenuItem1 = new System.Windows.Forms.ToolStripSeparator();
      this.exitToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
      this.helpToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
      this.aboutToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
      this.distanceLabel = new System.Windows.Forms.Label();
      this.distanceTextBox = new System.Windows.Forms.TextBox();
      this.sizeLabel = new System.Windows.Forms.Label();
      this.speedTextBox = new System.Windows.Forms.TextBox();
      this.calculateButton = new System.Windows.Forms.Button();
      this.resultLabel = new System.Windows.Forms.Label();
      this.flagPictureBox = new System.Windows.Forms.PictureBox();
      this.carPictureBox = new System.Windows.Forms.PictureBox();
      this.menuStrip1.SuspendLayout();
      ((System.ComponentModel.ISupportInitialize)(this.flagPictureBox)).BeginInit();
      ((System.ComponentModel.ISupportInitialize)(this.carPictureBox)).BeginInit();
      this.SuspendLayout();
      // 
      // menuStrip1
      // 
      this.menuStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.fileToolStripMenuItem,
            this.helpToolStripMenuItem});
      resources.ApplyResources(this.menuStrip1, "menuStrip1");
      this.menuStrip1.Name = "menuStrip1";
      // 
      // fileToolStripMenuItem
      // 
      this.fileToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.calculateToolStripMenuItem,
            this.toolStripMenuItem1,
            this.exitToolStripMenuItem});
      this.fileToolStripMenuItem.Name = "fileToolStripMenuItem";
      resources.ApplyResources(this.fileToolStripMenuItem, "fileToolStripMenuItem");
      // 
      // calculateToolStripMenuItem
      // 
      this.calculateToolStripMenuItem.Name = "calculateToolStripMenuItem";
      resources.ApplyResources(this.calculateToolStripMenuItem, "calculateToolStripMenuItem");
      this.calculateToolStripMenuItem.Click += new System.EventHandler(this.calculateButton_Click);
      // 
      // toolStripMenuItem1
      // 
      this.toolStripMenuItem1.Name = "toolStripMenuItem1";
      resources.ApplyResources(this.toolStripMenuItem1, "toolStripMenuItem1");
      // 
      // exitToolStripMenuItem
      // 
      this.exitToolStripMenuItem.Name = "exitToolStripMenuItem";
      resources.ApplyResources(this.exitToolStripMenuItem, "exitToolStripMenuItem");
      this.exitToolStripMenuItem.Click += new System.EventHandler(this.exitToolStripMenuItem_Click);
      // 
      // helpToolStripMenuItem
      // 
      this.helpToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.aboutToolStripMenuItem});
      this.helpToolStripMenuItem.Name = "helpToolStripMenuItem";
      resources.ApplyResources(this.helpToolStripMenuItem, "helpToolStripMenuItem");
      // 
      // aboutToolStripMenuItem
      // 
      this.aboutToolStripMenuItem.Name = "aboutToolStripMenuItem";
      resources.ApplyResources(this.aboutToolStripMenuItem, "aboutToolStripMenuItem");
      this.aboutToolStripMenuItem.Click += new System.EventHandler(this.aboutToolStripMenuItem_Click);
      // 
      // distanceLabel
      // 
      resources.ApplyResources(this.distanceLabel, "distanceLabel");
      this.distanceLabel.Name = "distanceLabel";
      // 
      // distanceTextBox
      // 
      resources.ApplyResources(this.distanceTextBox, "distanceTextBox");
      this.distanceTextBox.Name = "distanceTextBox";
      this.distanceTextBox.TextChanged += new System.EventHandler(this.textBox_TextChanged);
      // 
      // sizeLabel
      // 
      resources.ApplyResources(this.sizeLabel, "sizeLabel");
      this.sizeLabel.Name = "sizeLabel";
      // 
      // speedTextBox
      // 
      this.speedTextBox.AcceptsReturn = true;
      resources.ApplyResources(this.speedTextBox, "speedTextBox");
      this.speedTextBox.Name = "speedTextBox";
      this.speedTextBox.TextChanged += new System.EventHandler(this.textBox_TextChanged);
      // 
      // calculateButton
      // 
      resources.ApplyResources(this.calculateButton, "calculateButton");
      this.calculateButton.Name = "calculateButton";
      this.calculateButton.UseVisualStyleBackColor = true;
      this.calculateButton.Click += new System.EventHandler(this.calculateButton_Click);
      // 
      // resultLabel
      // 
      resources.ApplyResources(this.resultLabel, "resultLabel");
      this.resultLabel.Name = "resultLabel";
      // 
      // flagPictureBox
      // 
      this.flagPictureBox.Image = global::Driving.Properties.Resources.flag_great_britain;
      resources.ApplyResources(this.flagPictureBox, "flagPictureBox");
      this.flagPictureBox.Name = "flagPictureBox";
      this.flagPictureBox.TabStop = false;
      // 
      // carPictureBox
      // 
      this.carPictureBox.Image = global::Driving.Properties.Resources.car_sedan_blue;
      resources.ApplyResources(this.carPictureBox, "carPictureBox");
      this.carPictureBox.Name = "carPictureBox";
      this.carPictureBox.TabStop = false;
      // 
      // Form1
      // 
      resources.ApplyResources(this, "$this");
      this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
      this.Controls.Add(this.resultLabel);
      this.Controls.Add(this.flagPictureBox);
      this.Controls.Add(this.carPictureBox);
      this.Controls.Add(this.calculateButton);
      this.Controls.Add(this.speedTextBox);
      this.Controls.Add(this.sizeLabel);
      this.Controls.Add(this.distanceTextBox);
      this.Controls.Add(this.distanceLabel);
      this.Controls.Add(this.menuStrip1);
      this.MainMenuStrip = this.menuStrip1;
      this.Name = "Form1";
      this.Load += new System.EventHandler(this.Form1_Load);
      this.menuStrip1.ResumeLayout(false);
      this.menuStrip1.PerformLayout();
      ((System.ComponentModel.ISupportInitialize)(this.flagPictureBox)).EndInit();
      ((System.ComponentModel.ISupportInitialize)(this.carPictureBox)).EndInit();
      this.ResumeLayout(false);
      this.PerformLayout();

    }

    #endregion

    private System.Windows.Forms.MenuStrip menuStrip1;
    private System.Windows.Forms.ToolStripMenuItem fileToolStripMenuItem;
    private System.Windows.Forms.ToolStripMenuItem calculateToolStripMenuItem;
    private System.Windows.Forms.ToolStripSeparator toolStripMenuItem1;
    private System.Windows.Forms.ToolStripMenuItem exitToolStripMenuItem;
    private System.Windows.Forms.ToolStripMenuItem helpToolStripMenuItem;
    private System.Windows.Forms.ToolStripMenuItem aboutToolStripMenuItem;
    private System.Windows.Forms.Label distanceLabel;
    private System.Windows.Forms.TextBox distanceTextBox;
    private System.Windows.Forms.Label sizeLabel;
    private System.Windows.Forms.TextBox speedTextBox;
    private System.Windows.Forms.Button calculateButton;
    private System.Windows.Forms.PictureBox carPictureBox;
    private System.Windows.Forms.PictureBox flagPictureBox;
    private System.Windows.Forms.Label resultLabel;
  }
}

