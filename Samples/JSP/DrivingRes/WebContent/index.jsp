<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>

<%@ page import="java.util.ResourceBundle" %>

<%
  // Get parameters
  String distance = "";
  String speed = "";
  String clicked = "";

  if (request.getParameter("distance") != null) 
    distance = request.getParameter("distance");

  if (request.getParameter("speed") != null) 
    speed = request.getParameter("speed");

  if (request.getParameter("clicked") != null) 
    clicked = request.getParameter("clicked");
  
  ResourceBundle resourceBundle = ResourceBundle.getBundle("Resources", request.getLocale());
%>

<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
  <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
  <title><%=resourceBundle.getString("driving_time_calculator")%></title>
</head>

<body>
  <h1><%=resourceBundle.getString("driving_time")%></h1>

  <form action="index.jsp" method="get">
  <input type="hidden" name="clicked" value="yes">
  <table>
    <tr>
      <td>
        <%=resourceBundle.getString("distance")%><br/>
        <input type="text" name="distance" value="<%=distance%>">
      </td>
      <td>
        <%=resourceBundle.getString("speed")%><br/>
        <input type="text" name="speed" value="<%=speed%>">
      </td>
      <td valign="bottom">
        <input type="submit" value="<%=resourceBundle.getString("calculate")%>" />
      </td>
    </tr>
    <tr>
      <td>
        <img src="car.png" />
      </td>
      <td>
<%
  // Convert distance parameter into number
  double distanceValue;
    
  try
  {
    distanceValue = Double.parseDouble(distance);
  }
  catch (Exception e)
  {
   distanceValue = -1;
  }
    
  // Convert speed parameter into number
  double speedValue;
  
  try
  {
    speedValue = Double.parseDouble(speed);
  }
  catch (Exception e)
  {
    speedValue = -1;
  }
  
  if ((distanceValue >= 0) && (speedValue > 0))
  {
    // Calculate driving time
    double time = distanceValue/speedValue;
    int hours = (int)time;
    int minutes = (int)Math.round(60*(time - hours));
    
    out.println("<p>" + resourceBundle.getString("driving_time_is") + "</p>");
    // Message patterns for hours. Leave %d into the translated pattern.
    out.println("<p>" + String.format(resourceBundle.getString("hours"), hours) + "</p>"); 
    // Message patterns for minutes. Leave %d into the translated pattern.
    out.println("<p>" + String.format(resourceBundle.getString("minutes"), minutes) + "</p>");
  }
  else if (clicked.equals("yes"))
  {
    // Distance and/or speed was not given or were invalid
    out.println("<p>" + resourceBundle.getString("invalid_parameters") + "</p>");
  }
%>      
      </td>
      <td>
        <img src="flag.png" />
      </td>
    </tr>
  </table>
  </form>
</body>
</html>