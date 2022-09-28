using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;

namespace Binding
{
  public class MainWindowViewModel: INotifyPropertyChanged
  {
    private DateTime date = DateTime.Now;
    private double amount = 199.95;
    private string productName = "Ski";

    public DateTime Date { get { return date; } set { date = value; DoPropertyChanged();} }
    public double Amount { get { return amount; } set { amount = value; DoPropertyChanged();} }
    public string ProductName { get { return productName; } set { productName = value; DoPropertyChanged(); } }

    public event PropertyChangedEventHandler PropertyChanged;

    private void DoPropertyChanged([CallerMemberName] string propertyName = "")
    {
      PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
    }
  }
}
