Imports System.Globalization
Imports System.Threading
Imports VBSample.My.Resources

Public Class Form1
  Public Sub New()
    ' Sets the default culture
    Thread.CurrentThread.CurrentUICulture = CultureInfo.CurrentCulture

    ' This call is required by the Windows Form Designer.
    InitializeComponent()
  End Sub

  Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
    Label2.Text = String1
    Label3.Text = String.Format(String2, "John")
  End Sub

  Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
    MessageBox.Show(
      AboutText,
      AboutCaption,
      MessageBoxButtons.OK,
      MessageBoxIcon.Information)
  End Sub
End Class
