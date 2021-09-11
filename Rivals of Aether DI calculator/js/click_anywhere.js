click_anywhere <- "
    function(el, x, inputName){
      var id = el.getAttribute('id');
      var gd = document.getElementById(id);
      var d3 = Plotly.d3;
      Plotly.plot(id).then(attach);
        function attach() {
          var xaxis = gd._fullLayout.xaxis;
          var yaxis = gd._fullLayout.yaxis;
          var l = gd._fullLayout.margin.l;
          var t = gd._fullLayout.margin.t;
          var coordinates = [null, null]
    
          gd.addEventListener('click', function(evt) {
            var coordinates = [xaxis.p2c(evt.x), yaxis.p2c(evt.y)];
            Shiny.setInputValue(inputName, coordinates);
          });
        };
  }
  "