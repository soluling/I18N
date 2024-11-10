namespace HelloWorld
{
  partial class Form1
  {
    /// <summary>
    ///  Required designer variable.
    /// </summary>
    private System.ComponentModel.IContainer components = null;

    /// <summary>
    ///  Clean up any resources being used.
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
    ///  Required method for Designer support - do not modify
    ///  the contents of this method with the code editor.
    /// </summary>
    private void InitializeComponent()
    {
      System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(Form1));
      label1 = new Label();
      button1 = new Button();
      SuspendLayout();
      // 
      // label1
      // 
      resources.ApplyResources(label1, "label1");
      label1.Name = "label1";
      // 
      // button1
      // 
      resources.ApplyResources(button1, "button1");
      button1.Name = "button1";
      button1.UseVisualStyleBackColor = true;
      button1.Click += button1_Click;
      // 
      // Form1
      // 
      resources.ApplyResources(this, "$this");
      AutoScaleMode = AutoScaleMode.Font;
      Controls.Add(button1);
      Controls.Add(label1);
      Name = "Form1";
      Load += Form1_Load;
      ResumeLayout(false);
      PerformLayout();
    }

    #endregion

    private Label label1;
    private Button button1;
  }
}
