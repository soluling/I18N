namespace Sport
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
      this.components = new System.ComponentModel.Container();
      System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(Form1));
      this.listView1 = new System.Windows.Forms.ListView();
      this.nameColumn = new System.Windows.Forms.ColumnHeader();
      this.playersColumn = new System.Windows.Forms.ColumnHeader();
      this.goalieColumn = new System.Windows.Forms.ColumnHeader();
      this.originColumn = new System.Windows.Forms.ColumnHeader();
      this.button1 = new System.Windows.Forms.Button();
      this.toolTip1 = new System.Windows.Forms.ToolTip(this.components);
      this.SuspendLayout();
      // 
      // listView1
      // 
      this.listView1.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.nameColumn,
            this.playersColumn,
            this.goalieColumn,
            this.originColumn});
      this.listView1.FullRowSelect = true;
      this.listView1.Items.AddRange(new System.Windows.Forms.ListViewItem[] {
            ((System.Windows.Forms.ListViewItem)(resources.GetObject("listView1.Items"))),
            ((System.Windows.Forms.ListViewItem)(resources.GetObject("listView1.Items1"))),
            ((System.Windows.Forms.ListViewItem)(resources.GetObject("listView1.Items2")))});
      resources.ApplyResources(this.listView1, "listView1");
      this.listView1.Name = "listView1";
      this.listView1.ShowItemToolTips = true;
      this.listView1.UseCompatibleStateImageBehavior = false;
      this.listView1.View = System.Windows.Forms.View.Details;
      // 
      // nameColumn
      // 
      this.nameColumn.Tag = "nameColumn";
      resources.ApplyResources(this.nameColumn, "nameColumn");
      // 
      // playersColumn
      // 
      this.playersColumn.Tag = "playersColumn";
      resources.ApplyResources(this.playersColumn, "playersColumn");
      // 
      // goalieColumn
      // 
      this.goalieColumn.Tag = "goalieColumn";
      resources.ApplyResources(this.goalieColumn, "goalieColumn");
      // 
      // originColumn
      // 
      this.originColumn.Tag = "originColumn";
      resources.ApplyResources(this.originColumn, "originColumn");
      // 
      // button1
      // 
      resources.ApplyResources(this.button1, "button1");
      this.button1.Name = "button1";
      this.toolTip1.SetToolTip(this.button1, resources.GetString("button1.ToolTip"));
      this.button1.UseVisualStyleBackColor = true;
      this.button1.Click += new System.EventHandler(this.button1_Click);
      // 
      // Form1
      // 
      resources.ApplyResources(this, "$this");
      this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
      this.Controls.Add(this.button1);
      this.Controls.Add(this.listView1);
      this.Name = "Form1";
      this.Load += new System.EventHandler(this.Form1_Load);
      this.ResumeLayout(false);

    }

    #endregion

    private System.Windows.Forms.ListView listView1;
    private System.Windows.Forms.Button button1;
    private System.Windows.Forms.ColumnHeader nameColumn;
    private System.Windows.Forms.ColumnHeader playersColumn;
    private System.Windows.Forms.ColumnHeader goalieColumn;
    private System.Windows.Forms.ColumnHeader originColumn;
    private System.Windows.Forms.ToolTip toolTip1;
  }
}

