using Microsoft.VisualStudio.TestTools.UnitTesting;
using Soluling;
using System.Collections.Generic;

namespace Tests
{
  [TestClass]
  public class HiddenTests
  {
    private HiddenId HiddenId = new HiddenId();

    public HiddenTests()
    {
      // Uncomment this if you want to use a custom value
/*
      var value = new Dictionary<ZeroWidthCharacter, object>();
      value.Add(ZeroWidthCharacter.NoBreakSpace, true);
      value.Add(ZeroWidthCharacter.LeftToRightMark, true);
      value.Add(ZeroWidthCharacter.LeftToRightOverwrite, true);
      value.Add(ZeroWidthCharacter.InvisibleSeparator, true);

      HiddenId.Value = value;
*/
    }

    private void Encode(int value)
    {
      var encoded = HiddenId.Encode(value);
      var decoded = HiddenId.Decode(encoded);
      Assert.AreEqual(decoded, value);
    }

    [TestCategory("Hidden")]
    [TestMethod]
    public void EncodeZero()
    {
      Encode(0);
    }

    [TestCategory("Hidden")]
    [TestMethod]
    public void EncodeVeryShort()
    {
      Encode(1);
    }

    [TestCategory("Hidden")]
    [TestMethod]
    public void EncodeShort()
    {
      Encode(28);
    }

    [TestCategory("Hidden")]
    [TestMethod]
    public void EncodeMedium()
    {
      Encode(4151);
    }

    [TestCategory("Hidden")]
    [TestMethod]
    public void EncodeLong()
    {
      Encode(90123);
    }

    [TestCategory("Hidden")]
    [TestMethod]
    public void EncodeVeryLong()
    {
      Encode(650123);
    }

    [TestCategory("Hidden")]
    [TestMethod]
    public void Parse()
    {
      const string VALUE = "Hello";
      const int ID = 123;

      var encoded = HiddenId.Inject(VALUE, ID);
      var part = HiddenId.ParseFirst(encoded);

      Assert.AreEqual(part.Value, VALUE);
      Assert.AreEqual(part.Id, ID);
    }

    [TestCategory("Hidden")]
    [TestMethod]
    public void ParseZero()
    {
      const string VALUE = "Hello";
      const int ID = 0;

      var encoded = HiddenId.Inject(VALUE, ID);
      var part = HiddenId.ParseFirst(encoded);

      Assert.AreEqual(part.Value, VALUE);
      Assert.AreEqual(part.Id, ID);
    }

    [TestCategory("Hidden")]
    [TestMethod]
    public void ParseEmpty()
    {
      const string VALUE = "Hello";

      var part = HiddenId.ParseFirst(VALUE);

      Assert.AreEqual(part.Value, VALUE);
      Assert.AreEqual(part.Id, -1);
    }

    [TestCategory("Hidden")]
    [TestMethod]
    public void ParseTriple()
    {
      const string VALUE1 = "First part";
      const string VALUE2 = "Second part";
      const string VALUE3 = "Third part";
      const int ID1 = 123;
      const int ID2 = 234;
      const int ID3 = 3456;

      var encoded = HiddenId.Inject(VALUE1, ID1) + HiddenId.Inject(VALUE2, ID2) + HiddenId.Inject(VALUE3, ID3);
      var parts = HiddenId.Parse(encoded);

      Assert.AreEqual(parts.Length, 3);

      Assert.AreEqual(parts[0].Value, VALUE1);
      Assert.AreEqual(parts[0].Id, ID1);

      Assert.AreEqual(parts[1].Value, VALUE2);
      Assert.AreEqual(parts[1].Id, ID2);

      Assert.AreEqual(parts[2].Value, VALUE3);
      Assert.AreEqual(parts[2].Id, ID3);
    }
  }
}
