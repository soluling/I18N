<%@ Page Language="C#" AutoEventWireup="true" CodeBehind="Index.aspx.cs" Inherits="Driving.Index" culture="auto" uiculture="auto" %>

<!DOCTYPE html>

<html xmlns="http://www.w3.org/1999/xhtml">
<head runat="server">
  <title><%=Driving.Resources.Title %></title>
</head>
<body>
  <h1><asp:Image ID="flagImage" runat="server" ImageUrl="Images/flag.png" ImageAlign="Middle" meta:resourcekey="flagImage" /> <%=Driving.Resources.Header %></h1>

  <form id="form1" runat="server">
    <table border="0">
      <tr>
        <td>
          <asp:Label runat="server" meta:resourcekey="distanceLabel" />
          <br />
          <asp:TextBox ID="distanceTextBox" runat="server" />
        </td>
        <td>
          <asp:Label runat="server" meta:resourcekey="speedLabel" />
          <br />
          <asp:TextBox ID="speedTextBox" runat="server" />
        </td>
        <td valign="bottom">
          <asp:Button ID="calculateButton" runat="server" OnClick="calculateButton_Click" meta:resourcekey="calculateButton" />
        </td>
      </tr>
      <tr>
        <td>
          <asp:Image ID="carImage" runat="server" ImageUrl="Images/car.png" meta:resourcekey="carImage" />
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
