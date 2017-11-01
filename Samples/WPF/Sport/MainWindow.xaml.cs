using System;
using System.Collections.Generic;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using System.Collections.ObjectModel;

namespace Sport
{
  /// <summary>
  /// Interaction logic for MainWindow.xaml
  /// </summary>
  public partial class MainWindow : Window
  {
    ObservableCollection<SportData> sportCollection = new ObservableCollection<SportData>();

    public MainWindow()
    {
      sportCollection.Add(new SportData
      {
        Name = Properties.Resources.SoccerName,
        FieldPlayers = 10,
        Goalie = true,
        Origin = Properties.Resources.SoccerOrigin,
        Description = Properties.Resources.SoccerDescription
      });

      sportCollection.Add(new SportData
      {
        Name = Properties.Resources.HockeyName,
        FieldPlayers = 5,
        Goalie = true,
        Origin = Properties.Resources.HockeyOrigin,
        Description = Properties.Resources.HockeyDescription
      });

      sportCollection.Add(new SportData
      {
        Name = Properties.Resources.BasketballName,
        FieldPlayers = 5,
        Goalie = false,
        Origin = Properties.Resources.BasketballOrigin,
        Description = Properties.Resources.BasketballDescription
      });

      InitializeComponent();
    }
 
    public ObservableCollection<SportData> SportCollection
    {
      get { return sportCollection; } 
    }
  }

  public class SportData
  {
    public string Name { get; set; }
    public int FieldPlayers { get; set; }
    public bool Goalie { get; set; }
    public string Origin { get; set; }
    public string Description { get; set; }
  }
}
