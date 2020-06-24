<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>

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
%>

<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
  <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
  <title>Driving Time Calculator</title>
</head>

<body>
  <h1>Driving Time</h1>

  <form action="index.jsp" method="get">
  <input type="hidden" name="clicked" value="yes">
  <table>
    <tr>
      <td>
        Driving distance:<br/>
        <input type="text" name="distance" value="<%=distance%>">
      </td>
      <td>
        Speed:<br/>
        <input type="text" name="speed" value="<%=speed%>">
      </td>
      <td valign="bottom">
        <input type="submit" value="Calculate" />
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
  	
    out.println("<p>Driving time is</p>");
  	// Message patterns for hours. Leave %d into the translated pattern.
  	out.println("<p>" + String.format("%d hours", hours) + "</p>"); 
  	// Message patterns for minutes. Leave %d into the translated pattern.
  	out.println("<p>" + String.format("%d minutes", minutes) + "</p>");
  }
  else if (clicked.equals("yes"))
  {
	  // Distance and/or speed was not given or were invalid
    out.println("<p>You must enter a valid distance and a speed!</p>");
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