<!-- This sample shows how to localize a PHP application without GetText -->
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
  
  if (isSet($_GET["clicked"])) 
    $clicked = $_GET["clicked"];
  else
    $clicked = "";
?>
    
<html>
<head>
<meta charset="utf-8">
<title>Driving Time Calculator</title>

</head>

<body>
  <h1>Driving Time</h1>

  <form action="index.php" method="get">
  <input type="hidden" name="clicked" value="yes">
  <table>
    <tr>
      <td>
        Driving distance:<br/>
        <input type="text" name="distance" value="<?=$distance?>">
      </td>
      <td>
        Speed:<br/>
        <input type="text" name="speed" value="<?=$speed?>">
      </td>
      <td valign="bottom">
        <input type="submit" value="Calculate" />
      </td>
    </tr>
    <tr>
      <td>
        <img src="../Common/car.png" />
      </td>
      <td>
<?php 
  if (($distance >= 0) && ($speed > 0))
  {
    $time = $distance/$speed;
    $hours = floor($time);
    $minutes = round(60*($time - $hours));
  	
    echo "<p>Driving time is</p>";
  	// Message patterns for hours. Leave %d into the translated pattern.
  	echo "<p>"; printf("%d hours", $hours); echo "</p>"; 
  	// Message patterns for minutes. Leave %d into the translated pattern.
  	echo "<p>"; printf("%d minutes", $minutes); echo "</p>";
  }
  else if ($clicked == "yes")
  {
    echo "<p>You must enter a valid distance and a speed!</p>";
  }
?>      
        </td>
      <td>
        <img src="../Common/flag.png" />
      </td>
    </tr>
  </table>
  </form>
</body>
</html>