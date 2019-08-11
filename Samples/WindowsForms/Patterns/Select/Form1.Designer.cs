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
      this.soccerLabel = new System.Windows.Forms.Label();
      this.hockeyLabel = new System.Windows.Forms.Label();
      this.basketballLabel = new System.Windows.Forms.Label();
      this.languageButton = new System.Windows.Forms.Button();
      this.SuspendLayout();
      // 
      // soccerLabel
      // 
      this.soccerLabel.AutoSize = true;
      this.soccerLabel.Location = new System.Drawing.Point(8, 8);
      this.soccerLabel.Name = "soccerLabel";
      this.soccerLabel.Size = new System.Drawing.Size(40, 13);
      this.soccerLabel.TabIndex = 0;
      this.soccerLabel.Text = "dummy";
      // 
      // hockeyLabel
      // 
      this.hockeyLabel.AutoSize = true;
      this.hockeyLabel.Location = new System.Drawing.Point(8, 24);
      this.hockeyLabel.Name = "hockeyLabel";
      this.hockeyLabel.Size = new System.Drawing.Size(40, 13);
      this.hockeyLabel.TabIndex = 1;
      this.hockeyLabel.Text = "dummy";
      // 
      // basketballLabel
      // 
      this.basketballLabel.AutoSize = true;
      this.basketballLabel.Location = new System.Drawing.Point(8, 40);
      this.basketballLabel.Name = "basketballLabel";
      this.basketballLabel.Size = new System.Drawing.Size(40, 13);
      this.basketballLabel.TabIndex = 2;
      this.basketballLabel.Text = "dummy";
      // 
      // languageButton
      // 
      this.languageButton.ImeMode = System.Windows.Forms.ImeMode.NoControl;
      this.languageButton.Location = new System.Drawing.Point(320, 56);
      this.languageButton.Name = "languageButton";
      this.languageButton.Size = new System.Drawing.Size(91, 23);
      this.languageButton.TabIndex = 3;
      this.languageButton.Text = "&Language...";
      this.languageButton.UseVisualStyleBackColor = true;
      this.languageButton.Click += new System.EventHandler(this.languageButton_Click);
      // 
      // Form1
      // 
      this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
      this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
      this.ClientSize = new System.Drawing.Size(418, 88);
      this.Controls.Add(this.languageButton);
      this.Controls.Add(this.basketballLabel);
      this.Controls.Add(this.hockeyLabel);
      this.Controls.Add(this.soccerLabel);
      this.Name = "Form1";
      this.Text = "Select Sample";
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

