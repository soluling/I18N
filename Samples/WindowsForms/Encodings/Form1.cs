using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace Encodings
{
  public partial class Form1 : Form
  {
    public Form1()
    {
      InitializeComponent();
    }

    private void Form1_Load(object sender, EventArgs e)
    {
      ansiLabel.Text = Ansi.String1;
      isoLabel.Text = Iso.String1;
      utf8Label.Text = Utf8.String1;
      utf8NoBomLabel.Text = Utf8NoBom.String1;
      utf16LeLabel.Text = Utf16LE.String1;
      utf16BeLabel.Text = Utf16BE.String1;
      utf16LeNoBomLabel.Text = Utf16LENoBom.String1;
      utf16BeNoBomLabel.Text = Utf16BENoBom.String1;
      utf32LeLabel.Text = Utf32LE.String1;
      utf32BeLabel.Text = Utf32BE.String1;
      utf32LeNoBomLabel.Text = Utf32LENoBom.String1;
      utf32BeNoBomLabel.Text = Utf32BENoBom.String1;
      gb18030NoBomLabel.Text = GB18030NoBom.String1;
    }
  }
}
