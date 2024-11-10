namespace Select
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
      this.soccerLabel = new System.Windows.Forms.Label();
      this.hockeyLabel = new System.Windows.Forms.Label();
      this.basketballLabel = new System.Windows.Forms.Label();
      this.languageButton = new System.Windows.Forms.Button();
      this.SuspendLayout();
      // 
      // soccerLabel
      // 
      resources.ApplyResources(this.soccerLabel, "soccerLabel");
      this.soccerLabel.Name = "soccerLabel";
      // 
      // hockeyLabel
      // 
      resources.ApplyResources(this.hockeyLabel, "hockeyLabel");
      this.hockeyLabel.Name = "hockeyLabel";
      // 
      // basketballLabel
      // 
      resources.ApplyResources(this.basketballLabel, "basketballLabel");
      this.basketballLabel.Name = "basketballLabel";
      // 
      // languageButton
      // 
      resources.ApplyResources(this.languageButton, "languageButton");
      this.languageButton.Name = "languageButton";
      this.languageButton.UseVisualStyleBackColor = true;
      this.languageButton.Click += new System.EventHandler(this.languageButton_Click);
      // 
      // Form1
      // 
      resources.ApplyResources(this, "$this");
      this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
      this.Controls.Add(this.languageButton);
      this.Controls.Add(this.basketballLabel);
      this.Controls.Add(this.hockeyLabel);
      this.Controls.Add(this.soccerLabel);
      this.Name = "Form1";
      this.Load += new System.EventHandler(this.Form1_Load);
      this.ResumeLayout(false);
      this.PerformLayout();

    }

    #endregion

    private System.Windows.Forms.Label soccerLabel;
    private System.Windows.Forms.Label hockeyLabel;
    private System.Windows.Forms.Label basketballLabel;
    private System.Windows.Forms.Button languageButton;
  }
}

