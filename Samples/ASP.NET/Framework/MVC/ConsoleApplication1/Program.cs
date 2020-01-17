using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Data.SqlClient;

namespace ConsoleApplication1
{
  class Program
  {
    static void Main(string[] args)
    {
      string connetionString = @"Server=JASKANPC\SQLEXPRESS;Database=SportImage;Integrated Security=SSPI;";
      SqlConnection sqlConnection1 = new SqlConnection(connetionString);

      SqlCommand cmd = new SqlCommand();
      cmd.CommandText = "SELECT * FROM Sport";
      cmd.Connection = sqlConnection1;

      sqlConnection1.Open();

      SqlDataReader reader = cmd.ExecuteReader();

      sqlConnection1.Close();
    }
  }
}
