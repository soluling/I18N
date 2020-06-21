void Sample()
{
  char* str;

  // This s a normal string. Is has not been tagged. By default NewTool localizes it.
  str = "This is a sample";

  // You can disable localization of a string by adding a comment that begings with noloc
  str = "SELECT * FROM Sample"; // noloc

  // You can attach a comment to the string by adding a comment that begings with loc following comment text
  str = "This is another sample"; // loc This is a comment

  // You can give the maximing string length in characters using the MaxChars attribute in the tag comment
  str = "One"; // loc MaxChars=10

  // You can give the maximing string length in pixels using the MaxPixels attribute in the tag comment
  str = "Two"; // loc MaxPixels=100

  // You can use expression to break string into parts by using the Expression attribute in the tag comment
  // The following string will be broken into three separate rows: Soccer, Ice Hockey and Basketball
  str = "Soccer;Ice Hockey;Basketball"; // loc Expression="#;#;#"

  // You can combine attributes and comments
  str = "This is another sample"; // loc MaxChars=20 This is a comment

  // You can exclude several string by adding block exlude comments around them
  // beginnoloc
  str = "This is not localized";
  str = "This is not localized, too";
  // endnoloc
  str = "This is localized";

  // You can use regular expression to describe what must the format of a string be.
  str = "CAPITAL_AND_NUMBERS"; // loc RegEx="[A-Z0-9_]*"
}