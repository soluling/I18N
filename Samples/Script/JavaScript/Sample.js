  function loaded()
  {
    document.getElementById("p1").innerHTML = "This is a 'sample'";
    document.getElementById("p2").innerHTML = 'This is a \'sample\'';
    document.getElementById("p3").innerHTML = 'This is a "sample"';
    document.getElementById("p4").innerHTML = "This is a \"sample\"";

    var str = "Is this all there is?";
    var pattern = /is/gi;
    var result = str.match(pattern);
    document.getElementById("p10").innerHTML = result;

    str = "Is this 3 all there is?";
    pattern = /3 a/;
    result = str.match(pattern);
    document.getElementById("p11").innerHTML = result;

    str = "Is this /// all there is?";
    pattern = /\/\/\//g;
    result = str.match(pattern);
    document.getElementById("p12").innerHTML = result;

    str = "Is this all there is/a?";
    pattern = /[!"#$%&'()*+=<>\:\-./;¨?_(at)_[\]_^Çç`{|}~]/g;
    result = str.match(pattern);
    document.getElementById("p13").innerHTML = result;

    str = "Visit Microsoft!";
    result = str.replace(/microsoft/i, "W3Schools"); 
    document.getElementById("p14").innerHTML = result;
  }
