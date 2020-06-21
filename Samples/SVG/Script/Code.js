    function message_load(evt) 
    {
      var message = document.getElementById('message');
      var circle = document.getElementById('circle');
      var currentRadius = circle.getAttribute("r");

      if (currentRadius  == 100)
        message.textContent = "Click circle to make it larger";  // loc This message is shown when circle is small can it can be made larger
      else
        message.textContent = "Click circle to make it smaller";  // loc This message is shown when circle is large can it can be made smaller
    }

    function circle_click(evt) 
    {
      var circle = evt.target;
      var currentRadius = circle.getAttribute("r");

      if (currentRadius  == 100)
        circle.setAttribute("r", currentRadius*2);
      else
        circle.setAttribute("r", currentRadius*0.5);

      message_load(evt);
    }