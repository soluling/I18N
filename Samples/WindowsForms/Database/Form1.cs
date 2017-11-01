using System;
using System.Data;
using System.Data.OleDb;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using NewTool;
using NewTool.Forms;

namespace Database
{
  public partial class Form1 : Form
  {
    public Form1()
    {
      InitializeComponent();
    }

    // This procedure initializes the properties that are set on run time
    private void UpdateItems()
    {
      string databaseFile = "..\\..\\..\\..\\Access\\Sport\\Row\\Sport.accdb";

      // Localized EXE exist in a sub directory that is one level deeper
      if (!File.Exists(databaseFile))
        databaseFile = "..\\..\\..\\..\\..\\Access\\Sport\\Row\\Sport.accdb";

      // Open the database and assing Sport table to the grid view
      using (OleDbConnection connection = new OleDbConnection("Provider=Microsoft.ACE.OLEDB.12.0;Data Source=" + databaseFile))
      {
        // If you get here the following exception:
        // The 'Microsoft.ACE.OLEDB.12.0' provider is not registered on the local machine.
        // open project properties, select Build sheet and set platform target to match your computer (x84 vs x64). 
        // This applies when you have actually installed Access. If you have not please download Access redistrutable from Microsoft site.
        connection.Open();

        // Create a SQL statement that selects only those rows matching the current culture
        string sql = String.Format("SELECT Name, FieldPlayers, Goalie, Origin, Description FROM Sport WHERE LangId='{0}'", Resources.Language);
        OleDbDataAdapter adapter = new OleDbDataAdapter(sql, connection);
        DataSet dataset = new DataSet();

        adapter.Fill(dataset);

        dataGridView1.DataSource = dataset.Tables[0];

        // Set the column headers
        //dataGridView1.Columns[0].HeaderText = Properties.Resources.PictureColumn;
        dataGridView1.Columns[0].HeaderText = Properties.Resources.NameColumn;
        dataGridView1.Columns[1].HeaderText = Properties.Resources.FieldPlayersColumn;
        dataGridView1.Columns[2].HeaderText = Properties.Resources.GoalieColumn;
        dataGridView1.Columns[3].HeaderText = Properties.Resources.DescriptionColumn;
        dataGridView1.Columns[4].HeaderText = Properties.Resources.OriginColumn;
        //dataGridView1.Columns[6].HeaderText = Properties.Resources.FlagPictureColumn;
      }

      // Set the width of the view to show all columns.
      int width = dataGridView1.RowHeadersWidth + 2;

      foreach (DataGridViewColumn column in dataGridView1.Columns)
        width += column.Width;

      ClientSize = new Size(width, ClientSize.Height);
      dataGridView1.Width = width;
    }

    private void Form1_Load(object sender, EventArgs e)
    {
      UpdateItems();
    }

    private void button1_Click(object sender, EventArgs e)
    {
      // Show a language select dialog and turn on the selected language
      if (SelectLanguage.Select())
      {
        // Language has been changed.
        // Properties that were set on run time must be reset.
        UpdateItems();
      }
    }
  }
}
