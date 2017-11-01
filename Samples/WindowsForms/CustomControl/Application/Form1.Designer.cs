namespace SampleApplication
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
      this.sampleLabel1 = new SampleControl.SampleLabel();
      this.SuspendLayout();
      // 
      // sampleLabel1
      // 
      resources.ApplyResources(this.sampleLabel1, "sampleLabel1");
      this.sampleLabel1.Name = "sampleLabel1";
      // 
      // Form1
      // 
      resources.ApplyResources(this, "$this");
      this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
      this.Controls.Add(this.sampleLabel1);
      this.Name = "Form1";
      this.ResumeLayout(false);
      this.PerformLayout();

    }

    #endregion

    private SampleControl.SampleLabel sampleLabel1;

  }
}

