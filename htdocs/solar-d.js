$( document ).ready(function() {
  
  $("#tempclick").click(function( event ) {
    $.getJSON( "srv/temperature", function(data) {
      console.log(data);
      $("#temperaturetext").html(_.escape(data.temperature))
    });
  })
});
