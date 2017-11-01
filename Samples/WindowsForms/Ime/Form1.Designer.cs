namespace Ime
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
      this.defaultLabel = new System.Windows.Forms.Label();
      this.defaultTextBox = new System.Windows.Forms.TextBox();
      this.onTextBox = new System.Windows.Forms.TextBox();
      this.onLabel = new System.Windows.Forms.Label();
      this.defaultDesciptionLabel = new System.Windows.Forms.Label();
      this.label1 = new System.Windows.Forms.Label();
      this.SuspendLayout();
      // 
      // defaultLabel
      // 
      this.defaultLabel.AccessibleRole = System.Windows.Forms.AccessibleRole.MenuBar;
      resources.ApplyResources(this.defaultLabel, "defaultLabel");
      this.defaultLabel.Name = "defaultLabel";
      // 
      // defaultTextBox
      // 
      resources.ApplyResources(this.defaultTextBox, "defaultTextBox");
      this.defaultTextBox.Name = "defaultTextBox";
      // 
      // onTextBox
      // 
      resources.ApplyResources(this.onTextBox, "onTextBox");
      this.onTextBox.Name = "onTextBox";
      // 
      // onLabel
      // 
      this.onLabel.AccessibleRole = System.Windows.Forms.AccessibleRole.MenuBar;
      resources.ApplyResources(this.onLabel, "onLabel");
      this.onLabel.Name = "onLabel";
      // 
      // defaultDesciptionLabel
      // 
      this.defaultDesciptionLabel.AccessibleRole = System.Windows.Forms.AccessibleRole.MenuBar;
      resources.ApplyResources(this.defaultDesciptionLabel, "defaultDesciptionLabel");
      this.defaultDesciptionLabel.Name = "defaultDesciptionLabel";
      // 
      // label1
      // 
      this.label1.AccessibleRole = System.Windows.Forms.AccessibleRole.MenuBar;
      resources.ApplyResources(this.label1, "label1");
      this.label1.Name = "label1";
      // 
      // Form1
      // 
      resources.ApplyResources(this, "$this");
      this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
      this.Controls.Add(this.label1);
      this.Controls.Add(this.defaultDesciptionLabel);
      this.Controls.Add(this.onTextBox);
      this.Controls.Add(this.onLabel);
      this.Controls.Add(this.defaultTextBox);
      this.Controls.Add(this.defaultLabel);
      this.Name = "Form1";
      this.ResumeLayout(false);
      this.PerformLayout();

    }

    #endregion

    private System.Windows.Forms.Label defaultLabel;
    private System.Windows.Forms.TextBox defaultTextBox;
    private System.Windows.Forms.TextBox onTextBox;
    private System.Windows.Forms.Label onLabel;
    private System.Windows.Forms.Label defaultDesciptionLabel;
    private System.Windows.Forms.Label label1;
  }
}

