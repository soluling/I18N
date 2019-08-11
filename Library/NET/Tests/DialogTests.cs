using Microsoft.VisualStudio.TestTools.UnitTesting;
using Soluling;

namespace Tests
{
  [TestClass]
  public class DialogTests
  {
    [TestMethod]
    [TestCategory("DialogFilter")]
    public void DialogFilter()
    {
      // Typical
      string s = new DialogFilter()
        .AddAll("All files")
        .AddSupported("Supported files")
        .Add("XML files", "*.xml")
        .Add("Text files", "*.txt")
        .ToString();

      Assert.IsTrue(s == "All files (*.*)|*.*|Supported files (*.xml;*.txt)|*.xml;*.txt|XML files (*.xml)|*.xml|Text files (*.txt)|*.txt");


      // No all items or supported items
      s = new DialogFilter()
        .Add("XML files", "*.xml")
        .Add("Text files", "*.txt")
        .ToString();

      Assert.IsTrue(s == "XML files (*.xml)|*.xml|Text files (*.txt)|*.txt");


      // Specific item and all items
      s = new DialogFilter()
        .Add("XML files", "*.xml")
        .AddAll("All files")
        .ToString();

      Assert.IsTrue(s == "XML files (*.xml)|*.xml|All files (*.*)|*.*");


      // Supported items first and all items last
      s = new DialogFilter()
        .AddSupported("Supported files")
        .Add("XML files", "*.xml")
        .Add("Text files", "*.txt")
        .AddAll("All files")
        .ToString();

      Assert.IsTrue(s == "Supported files (*.xml;*.txt)|*.xml;*.txt|XML files (*.xml)|*.xml|Text files (*.txt)|*.txt|All files (*.*)|*.*");


      // Only all items
      s = new DialogFilter()
        .AddAll("All files")
        .ToString();

      Assert.IsTrue(s == "All files (*.*)|*.*");


      // No items
      s = new DialogFilter()
        .ToString();

      Assert.IsTrue(s == "");

      
      // No placeholder
      s = new DialogFilter()
        .Add("XML files", "*.xml", false)
        .ToString();

      Assert.IsTrue(s == "XML files|*.xml");

      
      // Custom placeholder
      s = new DialogFilter()
        .Add("XML files [{0}]", "*.xml")
        .ToString();

      Assert.IsTrue(s == "XML files [*.xml]|*.xml");

      
      // Another custom pattern
      s = new DialogFilter()
        .Add("{0} files", "*.xml")
        .ToString();

      Assert.IsTrue(s == "*.xml files|*.xml");
    }
  }
}
