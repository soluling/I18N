
namespace WindowsFormsApp1
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
      this.c1Combo1 = new C1.Win.C1List.C1Combo();
      this.c1MainMenu1 = new C1.Win.C1Command.C1MainMenu();
      this.c1CommandHolder1 = new C1.Win.C1Command.C1CommandHolder();
      this.c1CommandLink1 = new C1.Win.C1Command.C1CommandLink();
      this.c1CommandMenu1 = new C1.Win.C1Command.C1CommandMenu();
      this.c1CommandLink2 = new C1.Win.C1Command.C1CommandLink();
      this.c1Command1 = new C1.Win.C1Command.C1Command();
      ((System.ComponentModel.ISupportInitialize)(this.c1Combo1)).BeginInit();
      ((System.ComponentModel.ISupportInitialize)(this.c1CommandHolder1)).BeginInit();
      this.SuspendLayout();
      // 
      // c1Combo1
      // 
      this.c1Combo1.AllowColSelect = true;
      resources.ApplyResources(this.c1Combo1, "c1Combo1");
      this.c1Combo1.CharacterCasing = System.Windows.Forms.CharacterCasing.Normal;
      this.c1Combo1.DeadAreaBackColor = System.Drawing.Color.Empty;
      this.c1Combo1.Images.Add(((System.Drawing.Image)(resources.GetObject("c1Combo1.Images"))));
      this.c1Combo1.MatchEntryTimeout = ((long)(2000));
      this.c1Combo1.MaxDropDownItems = ((short)(5));
      this.c1Combo1.MaxLength = 32767;
      this.c1Combo1.MouseCursor = System.Windows.Forms.Cursors.Default;
      this.c1Combo1.Name = "c1Combo1";
      this.c1Combo1.RowSubDividerColor = System.Drawing.Color.DarkGray;
      this.c1Combo1.PropBag = resources.GetString("c1Combo1.PropBag");
      // 
      // c1MainMenu1
      // 
      resources.ApplyResources(this.c1MainMenu1, "c1MainMenu1");
      this.c1MainMenu1.CommandHolder = this.c1CommandHolder1;
      this.c1MainMenu1.CommandLinks.AddRange(new C1.Win.C1Command.C1CommandLink[] {
            this.c1CommandLink1});
      this.c1MainMenu1.Name = "c1MainMenu1";
      // 
      // c1CommandHolder1
      // 
      this.c1CommandHolder1.Commands.Add(this.c1CommandMenu1);
      this.c1CommandHolder1.Commands.Add(this.c1Command1);
      this.c1CommandHolder1.Owner = this;
      // 
      // c1CommandLink1
      // 
      this.c1CommandLink1.Command = this.c1CommandMenu1;
      // 
      // c1CommandMenu1
      // 
      this.c1CommandMenu1.CommandLinks.AddRange(new C1.Win.C1Command.C1CommandLink[] {
            this.c1CommandLink2});
      this.c1CommandMenu1.HideNonRecentLinks = false;
      this.c1CommandMenu1.Name = "c1CommandMenu1";
      resources.ApplyResources(this.c1CommandMenu1, "c1CommandMenu1");
      // 
      // c1CommandLink2
      // 
      this.c1CommandLink2.Command = this.c1Command1;
      // 
      // c1Command1
      // 
      this.c1Command1.Image = ((System.Drawing.Image)(resources.GetObject("c1Command1.Image")));
      this.c1Command1.Name = "c1Command1";
      resources.ApplyResources(this.c1Command1, "c1Command1");
      // 
      // Form1
      // 
      resources.ApplyResources(this, "$this");
      this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
      this.Controls.Add(this.c1Combo1);
      this.Controls.Add(this.c1MainMenu1);
      this.Name = "Form1";
      ((System.ComponentModel.ISupportInitialize)(this.c1Combo1)).EndInit();
      ((System.ComponentModel.ISupportInitialize)(this.c1CommandHolder1)).EndInit();
      this.ResumeLayout(false);
      this.PerformLayout();

    }

    #endregion

    private C1.Win.C1List.C1Combo c1Combo1;
    private C1.Win.C1Command.C1MainMenu c1MainMenu1;
    private C1.Win.C1Command.C1CommandHolder c1CommandHolder1;
    private C1.Win.C1Command.C1CommandMenu c1CommandMenu1;
    private C1.Win.C1Command.C1CommandLink c1CommandLink2;
    private C1.Win.C1Command.C1Command c1Command1;
    private C1.Win.C1Command.C1CommandLink c1CommandLink1;
  }
}

