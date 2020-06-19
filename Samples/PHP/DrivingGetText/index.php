<!-- This sample shows how to localize a PHP application with gettext -->
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

  // Get the language parameter. If no languge parameter use English.
  if (isSet($_GET["locale"]))
    $locale = $_GET["locale"];
  else
    $locale = "en";

  // Initialize GetText using the given language
  putenv("LC_ALL=$locale");
  setlocale(LC_ALL, $locale);
  bindtextdomain("messages", "locale");
  textdomain("messages");
?>

<html>
<head>
<meta charset="utf-8">
<title><?=_("Driving Time Calculator")?></title>

</head>

<body>
  <h1><?=_("Driving Time")?></h1>

  <form action="index.php" method="get">
  <input type="hidden" name="clicked" value="yes">
  <table>
    <tr>
      <td>
        <?=_("Driving distance:")?><br/>
        <input type="text" name="distance" value="<?=$distance?>">
      </td>
      <td>
        <?=_("Speed:")?><br/>
        <input type="text" name="speed" value="<?=$speed?>">
      </td>
      <td valign="bottom">
        <input type="submit" value="<?=_("Calculate")?>" />
      </td>
    </tr>
    <tr>
      <td>
        <img src="../Common/<?=/* File name for car image. If you need to localize the image enter here the name of the localized image. */ _("car.png")?>" />
      </td>
      <td>
<?php
  if (($distance >= 0) && ($speed > 0))
  {
    $time = $distance/$speed;
    $hours = floor($time);
    $minutes = round(60*($time - $hours));

    echo "<p>"._("Driving time is")."</p>";
    // Message patterns for hours. Leave %d into the translated pattern.
    echo "<p>"; printf(ngettext("%d hour", "%d hours", $hours), $hours); echo "</p>";
    // Message patterns for minutes. Leave %d into the translated pattern.
    echo "<p>"; printf(ngettext("%d minute", "%d minutes", $minutes), $minutes); echo "</p>";
  }
  else if ($clicked == "yes")
  {
    echo "<p>"._("You must enter a valid distance and a speed!")."</p>";
  }
  ?>
        </td>
      <td>
        <img src="../Common/<?=/* File name for language flag. Enter here the name of the localized image. */ _("flag.png")?>" />
      </td>
    </tr>
  </table>
  </form>
</body>
</html>