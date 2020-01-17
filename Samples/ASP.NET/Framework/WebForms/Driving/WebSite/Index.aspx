<%@ Page Language="C#" AutoEventWireup="true" CodeFile="Index.aspx.cs" Inherits="Index" %>

<!DOCTYPE html>

<html xmlns="http://www.w3.org/1999/xhtml">
<head runat="server">
  <title>Driving Time Calculator</title>
</head>
<body>
  <h1><asp:Image ID="flagImage" runat="server" ImageUrl="Images/flag.png" ImageAlign="Middle" /> Driving Time</h1>

  <form id="form1" runat="server">
    <table border="0">
      <tr>
        <td>
          <asp:Label runat="server" Text="Driving distance:" />
          <br />
          <asp:TextBox ID="distanceTextBox" runat="server" />
        </td>
        <td>
          <asp:Label runat="server" Text="Speed:" />
          <br />
          <asp:TextBox ID="speedTextBox" runat="server" />
        </td>
        <td valign="bottom">
          <asp:Button ID="calculateButton" runat="server" Text="Calculate" OnClick="calculateButton_Click" />
        </td>
      </tr>
      <tr>
        <td>
          <img src="Images/car.png" />
        </td>
        <td>
          <asp:Label ID="resultLabel" runat="server" />
        </td>
        <td>
          <asp:Label ID="hoursLabel" runat="server" />
          <br />
          <asp:Label ID="minutesLabel" runat="server" />
        </td>
      </tr>
    </table>
  </form>
</body>
</html>
