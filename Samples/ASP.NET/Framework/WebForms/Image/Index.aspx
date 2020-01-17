<%@ Page Language="C#" AutoEventWireup="true" CodeBehind="Index.aspx.cs" Inherits="Image.WebForm1" meta:resourcekey="pageResource" culture="auto" uiculture="auto" %>

<!DOCTYPE html>

<html xmlns="http://www.w3.org/1999/xhtml">
<head runat="server">
</head>
<body>
  <form id="form1" runat="server">
  <div>
    <asp:Label ID="label1" runat="server" meta:resourcekey="label1"></asp:Label>
    <br />
    <asp:Image ID="image1" runat="server" ImageUrl="<%$resources:, flag %>" />
  </div>
  </form>
</body>
</html>
