namespace WindowsForms
{
  partial class MainForm
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
      System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(MainForm));
      this.simpleButton = new System.Windows.Forms.Button();
      this.baseButton = new System.Windows.Forms.Button();
      this.inheritedButton = new System.Windows.Forms.Button();
      this.moreButton = new System.Windows.Forms.Button();
      this.SuspendLayout();
      // 
      // simpleButton
      // 
      resources.ApplyResources(this.simpleButton, "simpleButton");
      this.simpleButton.Name = "simpleButton";
      this.simpleButton.UseVisualStyleBackColor = true;
      this.simpleButton.Click += new System.EventHandler(this.simpleButton_Click);
      // 
      // baseButton
      // 
      resources.ApplyResources(this.baseButton, "baseButton");
      this.baseButton.Name = "baseButton";
      this.baseButton.UseVisualStyleBackColor = true;
      this.baseButton.Click += new System.EventHandler(this.baseButton_Click);
      // 
      // inheritedButton
      // 
      resources.ApplyResources(this.inheritedButton, "inheritedButton");
      this.inheritedButton.Name = "inheritedButton";
      this.inheritedButton.UseVisualStyleBackColor = true;
      this.inheritedButton.Click += new System.EventHandler(this.inheritedButton_Click);
      // 
      // moreButton
      // 
      resources.ApplyResources(this.moreButton, "moreButton");
      this.moreButton.Name = "moreButton";
      this.moreButton.UseVisualStyleBackColor = true;
      this.moreButton.Click += new System.EventHandler(this.moreButton_Click);
      // 
      // MainForm
      // 
      resources.ApplyResources(this, "$this");
      this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
      this.Controls.Add(this.moreButton);
      this.Controls.Add(this.inheritedButton);
      this.Controls.Add(this.baseButton);
      this.Controls.Add(this.simpleButton);
      this.Name = "MainForm";
      this.ResumeLayout(false);

    }

    #endregion

    private System.Windows.Forms.Button simpleButton;
    private System.Windows.Forms.Button baseButton;
    private System.Windows.Forms.Button inheritedButton;
    private System.Windows.Forms.Button moreButton;
  }
}

