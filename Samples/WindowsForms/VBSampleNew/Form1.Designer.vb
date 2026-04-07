<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class Form1
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()>
    Protected Overrides Sub Dispose(disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

  'NOTE: The following procedure is required by the Windows Form Designer
  'It can be modified using the Windows Form Designer.  
  'Do not modify it using the code editor.
  <System.Diagnostics.DebuggerStepThrough()>
  Private Sub InitializeComponent()
    Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(Form1))
    Label1 = New Label()
    Label2 = New Label()
    Label3 = New Label()
    Button1 = New Button()
    SuspendLayout()
    ' 
    ' Label1
    ' 
    resources.ApplyResources(Label1, "Label1")
    Label1.Name = "Label1"
    ' 
    ' Label2
    ' 
    resources.ApplyResources(Label2, "Label2")
    Label2.Name = "Label2"
    ' 
    ' Label3
    ' 
    resources.ApplyResources(Label3, "Label3")
    Label3.Name = "Label3"
    ' 
    ' Button1
    ' 
    resources.ApplyResources(Button1, "Button1")
    Button1.Name = "Button1"
    Button1.UseVisualStyleBackColor = True
    ' 
    ' Form1
    ' 
    resources.ApplyResources(Me, "$this")
    AutoScaleMode = AutoScaleMode.Font
    Controls.Add(Button1)
    Controls.Add(Label3)
    Controls.Add(Label2)
    Controls.Add(Label1)
    Name = "Form1"
    ResumeLayout(False)
    PerformLayout()
  End Sub

  Friend WithEvents Label1 As Label
  Friend WithEvents Label2 As Label
  Friend WithEvents Label3 As Label
  Friend WithEvents Button1 As Button

End Class
