using System;
using System.Data;
using System.Data.OleDb;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using Soluling;
using Soluling.Forms;

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
      using (OleDbConnection connection = new OleDbConnection("Provider=Microsoft.ACE.OLEDB.15.0;Data Source=" + databaseFile))
      {
        // If you get here the following exception:
        // "The 'Microsoft.ACE.OLEDB.15.0' provider is not registered on the local machine."
        // Make sure that Microsoft.ACE.OLEDB.15.0 is installed. If not install Microsoft Access Database Engine 2013 Redistributable.
        // If you have some other version installed such as Microsoft.ACE.OLEDB.12.0 or Microsoft.ACE.OLEDB.16.0 modify the above code.
        // If you have 64-bit machine but you have installed 32-bit Access open project properties, select Build sheet and set platform target to x84. 
        connection.Open();

        // Create a SQL statement that selects only those rows matching the current culture
        string sql = String.Format("SELECT Name, FieldPlayers, Goalie, Origin, Description FROM Sport WHERE LangId='{0}'", Language.Id);
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
