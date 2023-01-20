namespace DevExpress
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
      this.simpleButton1 = new DevExpress.XtraEditors.SimpleButton();
      this.ratingControl1 = new DevExpress.XtraEditors.RatingControl();
      ((System.ComponentModel.ISupportInitialize)(this.ratingControl1.Properties)).BeginInit();
      this.SuspendLayout();
      // 
      // simpleButton1
      // 
      resources.ApplyResources(this.simpleButton1, "simpleButton1");
      this.simpleButton1.Name = "simpleButton1";
      // 
      // ratingControl1
      // 
      resources.ApplyResources(this.ratingControl1, "ratingControl1");
      this.ratingControl1.Name = "ratingControl1";
      this.ratingControl1.Rating = new decimal(new int[] {
            0,
            0,
            0,
            0});
      // 
      // Form1
      // 
      resources.ApplyResources(this, "$this");
      this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
      this.Controls.Add(this.ratingControl1);
      this.Controls.Add(this.simpleButton1);
      this.Name = "Form1";
      ((System.ComponentModel.ISupportInitialize)(this.ratingControl1.Properties)).EndInit();
      this.ResumeLayout(false);
      this.PerformLayout();

    }

    #endregion

    private XtraEditors.SimpleButton simpleButton1;
    private XtraEditors.RatingControl ratingControl1;
  }
}

