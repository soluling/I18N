namespace SimpleGender
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
      this.maleLabel = new System.Windows.Forms.Label();
      this.femaleLabel = new System.Windows.Forms.Label();
      this.neutralLabel = new System.Windows.Forms.Label();
      this.languageButton = new System.Windows.Forms.Button();
      this.neutralInfo = new System.Windows.Forms.Label();
      this.femaleInfo = new System.Windows.Forms.Label();
      this.maleInfo = new System.Windows.Forms.Label();
      this.SuspendLayout();
      // 
      // maleLabel
      // 
      resources.ApplyResources(this.maleLabel, "maleLabel");
      this.maleLabel.Name = "maleLabel";
      // 
      // femaleLabel
      // 
      resources.ApplyResources(this.femaleLabel, "femaleLabel");
      this.femaleLabel.Name = "femaleLabel";
      // 
      // neutralLabel
      // 
      resources.ApplyResources(this.neutralLabel, "neutralLabel");
      this.neutralLabel.Name = "neutralLabel";
      // 
      // languageButton
      // 
      resources.ApplyResources(this.languageButton, "languageButton");
      this.languageButton.Name = "languageButton";
      this.languageButton.UseVisualStyleBackColor = true;
      this.languageButton.Click += new System.EventHandler(this.languageButton_Click);
      // 
      // neutralInfo
      // 
      resources.ApplyResources(this.neutralInfo, "neutralInfo");
      this.neutralInfo.Name = "neutralInfo";
      // 
      // femaleInfo
      // 
      resources.ApplyResources(this.femaleInfo, "femaleInfo");
      this.femaleInfo.Name = "femaleInfo";
      // 
      // maleInfo
      // 
      resources.ApplyResources(this.maleInfo, "maleInfo");
      this.maleInfo.Name = "maleInfo";
      // 
      // Form1
      // 
      resources.ApplyResources(this, "$this");
      this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
      this.Controls.Add(this.neutralInfo);
      this.Controls.Add(this.femaleInfo);
      this.Controls.Add(this.maleInfo);
      this.Controls.Add(this.languageButton);
      this.Controls.Add(this.neutralLabel);
      this.Controls.Add(this.femaleLabel);
      this.Controls.Add(this.maleLabel);
      this.Name = "Form1";
      this.Load += new System.EventHandler(this.Form1_Load);
      this.ResumeLayout(false);
      this.PerformLayout();

    }

    #endregion

    private System.Windows.Forms.Label maleLabel;
    private System.Windows.Forms.Label femaleLabel;
    private System.Windows.Forms.Label neutralLabel;
    private System.Windows.Forms.Button languageButton;
    private System.Windows.Forms.Label neutralInfo;
    private System.Windows.Forms.Label femaleInfo;
    private System.Windows.Forms.Label maleInfo;
  }
}

