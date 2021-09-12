click_anywhere <- "
    function(el, x, inputName){
      var id = el.getAttribute('id');
      var gd = document.getElementById(id);
      var d3 = Plotly.d3;
      Plotly.plot(id).then(attach);
      function attach() {
      var coordinates = [null, null]
  
      gd.addEventListener('click', function(evt) {
        var bb = evt.target.getBoundingClientRect();
        var x = gd._fullLayout.xaxis.p2d(evt.clientX - bb.left);
        var y = gd._fullLayout.yaxis.p2d(evt.clientY - bb.top);
        var coordinates = [x, y];
        Shiny.setInputValue(inputName, coordinates);
      });
    };
  }
  "