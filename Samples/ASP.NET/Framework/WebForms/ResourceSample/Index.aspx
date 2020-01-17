<%@ Page Language="C#" AutoEventWireup="true" CodeBehind="Index.aspx.cs" Inherits="Resource.Index" UICulture="auto" Culture="auto" %>

<!DOCTYPE html>

<html xmlns="http://www.w3.org/1999/xhtml">
<head runat="server">
    <title></title>
</head>
<body>
    <h2>App_GlobalResources</h2>
    <p><%=Resources.Resource1.One %></p>
    <p><%=GetGlobalResourceObject("Resource1", "One") %></p>

    <h2>App_LocalResources</h2>
    <p><%=GetLocalResourceObject("One") %></p>

    <h2>Other resources</h2>
    <p><%=ResourceSample.Resource1.One %></p>

    <h2>Run time</h2>
    <p><asp:Localize runat="server" ID="localize1" /></p>
    <p><asp:Localize runat="server" ID="localize2" /></p>
    <p><asp:Localize runat="server" ID="localize3" /></p>
    <p><asp:Localize runat="server" ID="localize4" /></p>

    <h2>Form</h2>
    <form id="form1" runat="server">
    <div>
        <asp:Label runat="server" meta:resourcekey="label1" />
        <asp:Label runat="server" meta:resourcekey="label2" />
        <br />
        <asp:Label runat="server" Text="<%$resources:, One %>" />
        <asp:Label runat="server" Text="<%$resources:One %>" />
        <asp:Label runat="server" Text="<%$resources:Resource1, One %>" />
    </div>
    </form>
</body>
</html>
