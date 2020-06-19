<!-- This sample shows how to localize a JavaScript enhanced PHP application with GetText -->
<!DOCTYPE html>

<?php 
  // Get the distance and speed parameters
  if (isSet($_GET["distance"])) 
    $distance = $_GET["distance"];
  else
    $distance = "";
  
  if (isSet($_GET["speed"])) 
    $speed = $_GET["speed"];
  else
    $speed = "";
?>

<html>
<head>
<meta charset="utf-8">
<title>Driving Time Calculator</title>

<script src="../sprintf.js"></script>

<script>
// This event is called when the calculate button is clicked
function calculate()
{
  var distance = document.getElementById("distanceTextBox").value;
  var speed = document.getElementById("speedTextBox").value;

  if ((distance >= 0) && (speed > 0))
  {
    var time = distance/speed;
    var hours = Math.floor(time);
    var minutes = Math.round(60*(time - hours));
	
    document.getElementById("hoursLabel").innerHTML = sprintf("%d hours", hours);
    document.getElementById("minutesLabel").innerHTML = sprintf("%d minute", minutes);
    // In many cases instead of above you use hard coded strings and append them together using . or +
    // Do not use it but use sprintf
    //document.getElementById("hoursLabel").innerHTML = hours." hours";
    //document.getElementById("minutesLabel").innerHTML = hours + " minutes";
  }
  else
  {
    document.getElementById("resultLabel").innerHTML = "You must enter a valid distance and a speed!";
  }

  document.getElementById("resultLabel").style.display = "block";
}

//This event is called when either the distance or speed edit has been changed
function valueChanged()
{
  var distance = document.getElementById("distanceTextBox").value;
  var speed = document.getElementById("speedTextBox").value;

  document.getElementById("calculateButton").disabled = (distance == "") || (speed == "");
}

function languageChanged()
{
  var locale = document.getElementById("languageSelect").value;
  var url = "http://127.0.0.1/DrivingJavaScript/" + locale;
  window.location.href = url;
}
</script>

</head>

<body onload="valueChanged()">
  <!-- Runtime language switch. Populate the list box and set event for list selection change. -->
  Select language:
  <select id="languageSelect" onchange="languageChanged()">
<?php
  $languages = array
  (
    "en" => "English",
    "fi" => "suomi",
    "ja" => "日本語"
  );

  foreach ($languages as $id => $name) 
  {
    if ($id == $locale)
      $selected = " selected";
    else 
      $selected = "";

    echo "      <option value=\"".$id."\"".$selected.">".$name."</option>\r\n";
  }
?>    </select>     
  
  <!-- User interface -->
  <h1>Driving Time</h1>

  <table>
    <tr>
      <td>
        Driving distance:<br/>
        <input type="text" id="distanceTextBox" onkeypress="valueChanged()" onchange="valueChanged()">
      </td>
      <td>
        Speed:<br/>
        <input type="text" id="speedTextBox" onkeypress="valueChanged()" onchange="valueChanged()">
      </td>
      <td valign="bottom">
        <button id="calculateButton" onclick="calculate()">Calculate</button>
      </td>
    </tr>
    <tr>
      <td>
        <img src="../Common/car.png" />
      </td>
      <td>
        <p id="resultLabel" style="display: none">Driving time is</p>
        <p id="hoursLabel"></p>
        <p id="minutesLabel"></p>
      </td>
      <td>
        <img src="../Common/flag.png" />
      </td>
    </tr>
  </table>
</body>
</html>