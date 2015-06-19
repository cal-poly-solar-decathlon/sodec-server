// There must be some standard date format for json....

// how do we do modules, now?
solar_d = function () {

function append_all_strings(strings) {
  return _.foldl(strings,function(a,b){return a+b;},"");
}

  // json -> html
function temps_to_html(temps){
  // there must be some nicer way to quote this...
  var rows = _.map(temps.slice(-10),temp_to_table_row)
  return append_all_strings(rows);
}

// json status -> row
function temp_to_table_row(temp){
  return "<tr><td>"+_.escape(temp.timestamp)+"</td><td>"+_.escape(temp.reading)
    +"</td></tr>";
}
// _.escape

$( document ).ready(function() {
  $("#tempclick").click(function( event ) {
    $.getJSON( "srv/temperature", function(data) {
      console.log(data);
      $("#temptable").html(temps_to_html(data))
    });
    $.getJSON( "srv/timestamp", function(data) {
      console.log(data);
      $("#timetext").html(_.escape(data.timestamp));
    })
  })
});

}();
