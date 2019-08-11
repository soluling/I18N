using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Windows.Forms;

namespace SampleControl
{
  public class SampleLabel : Label
  {
    private byte[] customBinary = new byte[4];
      
    public SampleLabel()
    {
      CustomExpression = @"One;Two;Three";
      CustomXml = @"<?xml version=""1.0"" encoding=""utf-8""?><simple><product>Skim milk</product><price>1.15</price></simple>";
      CustomJson = @"{""product"": ""Skim milk"", ""price"": 1.15, ""onsale"": false}";
      CustomHtml = @"<html><head><title>Test</title></head><body><li>One</li><li>Two</li></body></html>";
      CustomC1 = @"4,1,0,0,0,95,Columns:1{Caption:""One"";}	2{Caption:""Two"";}	3{Caption:""Three"";}	";

      customBinary[0] = 1;
      customBinary[1] = 2;
      customBinary[2] = 3;
      customBinary[3] = 4;
    }

    [Localizable(true)]
    public string CustomString { get; set; }

    [Localizable(true)]
    public char CustomChar { get; set; }

    [Localizable(true)]
    public bool CustomBool { get; set; }

    [Localizable(true)]
    public byte CustomByte { get; set; }

    [Localizable(true)]
    public sbyte CustomSByte { get; set; }

    [Localizable(true)]
    public decimal CustomDecimal { get; set; }

    [Localizable(true)]
    public float CustomFloat { get; set; }

    [Localizable(true)]
    public double CustomDouble { get; set; }

    [Localizable(true)]
    public short CustomShort { get; set; }

    [Localizable(true)]
    public ushort CustomUShort { get; set; }

    [Localizable(true)]
    public int CustomInt { get; set; }

    [Localizable(true)]
    public uint CustomUInt { get; set; }

    [Localizable(true)]
    public long CustomLong { get; set; }

    [Localizable(true)]
    public ulong CustomULong { get; set; }

    [Localizable(true)]
    public DateTime CustomDate { get; set; }

    [Localizable(true)]
    public string CustomExpression { get; set; }

    [Localizable(true)]
    public string CustomXml { get; set; }

    [Localizable(true)]
    public string CustomJson { get; set; }

    [Localizable(true)]
    public string CustomHtml { get; set; }

    [Localizable(true)]
    public string CustomC1 { get; set; }

    [Localizable(true)]
    public Image CustomImage { get; set; }

    [Localizable(true)]
    public MemoryStream CustomAudio { get; set; }

    [Localizable(true)]
    public byte[] CustomBinary 
    {
      get { return customBinary; }
      set { customBinary = value; } 
    }
  }
}
