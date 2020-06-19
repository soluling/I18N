<!-- This sample shows how to localize a JavaScript enhanced PHP application with GetText -->
<!DOCTYPE html>

<?php
  // Get the language parameter. If no languge parameter use English.
  if (isSet($_GET["locale"])) 
    $locale = $_GET["locale"];
  else
    $locale = "en";

  // Initialize GetText using the given language
  putenv("LC_ALL=$locale");
  setlocale(LC_ALL, $locale);
  textdomain("messages");
  bindtextdomain("messages", "locale");
?>

<html>
<head>
<meta charset="utf-8">
<title><?=_("Driving Time Calculator")?></title>

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
	
    document.getElementById("hoursLabel").innerHTML = sprintf("<?=/* Message pattern for hours. Leave %d into the translated pattern.*/ _("%d hours")?>", hours);
    document.getElementById("minutesLabel").innerHTML = sprintf("<?=/* Message pattern for minutes. Leave %d into the translated pattern.*/ _("%d minute")?>", minutes);
    // In many cases instead of above you use hard coded strings and append them together using . or +
    // Do not use it but use sprintf and wrap the template string into _(...) to use GetText.
    //document.getElementById("hoursLabel").innerHTML = hours." hours";
    //document.getElementById("minutesLabel").innerHTML = hours + " minutes";
  }
  else
  {
    document.getElementById("resultLabel").innerHTML = <?=_("You must enter a valid distance and a speed!")?>;
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
  var url = document.URL.split('?')[0] + "?locale=" + locale;
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
  <h1><?=_("Driving Time")?></h1>

  <table>
    <tr>
      <td>
        <?=_("Driving distance:")?><br/>
        <input type="text" id="distanceTextBox" onkeypress="valueChanged()" onchange="valueChanged()">
      </td>
      <td>
        <?=_("Speed:")?><br/>
        <input type="text" id="speedTextBox" onkeypress="valueChanged()" onchange="valueChanged()">
      </td>
      <td valign="bottom">
        <button id="calculateButton" onclick="calculate()"><?=_("Calculate")?></button>
      </td>
    </tr>
    <tr>
      <td>
        <img src="../Common/<?=/* File name for car image. If you need to localize the image enter here the name of the localized image. */ _("car.png")?>" />
      </td>
      <td>
        <p id="resultLabel" style="display: none"><?=_("Driving time is")?></p>
        <p id="hoursLabel"></p>
        <p id="minutesLabel"></p>
      </td>
      <td>
        <img src="../Common/<?=/* File name for language flag. Enter here the name of the localized image. */ _("flag.png")?>" />
      </td>
    </tr>
  </table>
</body>
</html>