var db = require('../modules/dbConnect.js');
var http = require('http');
var parseString = require('xml2js').parseString;

var insertReading = function(deviceName, deviceReading) {
   if (deviceOutput === 'Grid') {
   } else if (deviceOutput === 'Solar') {
      db.addSensorEvent('s-elec-gen-top-array', deviceReading);
   } else if (deviceOutput === 'Dryer/Washer') {
      db.addSensorEvent('s-elec-used-laundry', deviceReading);
   } else if (deviceOutput === 'Dishwasher') {
      db.addSensorEvent('s-elec-used-dishwasher', deviceReading);
   } else if (deviceOutput === 'Refrigerator') {
      db.addSensorEvent('s-elec-used-refrigerator', deviceReading);
   } else if (deviceOutput === 'Induction Stove') {
      db.addSensorEvent('s-elec-used-induction-stove', deviceReading);
   } else if (deviceOutput === 'Water Heater') {
      db.addSensorEvent('s-elec-used-ewh-solar-water-heater', deviceReading);
   } else if (deviceOutput === 'Kitchen Receps1') {
      db.addSensorEvent('s-elec-used-kitchen-receps-1', deviceReading);
   } else if (deviceOutput === 'Kitchen Receps2') {
      db.addSensorEvent('s-elec-used-kitchen-receps-2', deviceReading);
   } else if (deviceOutput === 'Mechanical Room') {
      db.addSensorEvent('s-elec-used-mechanical-receps', deviceReading);
   } else if (deviceOutput === 'Entry Receps') {
      db.addSensorEvent('s-elec-used-entry-receps', deviceReading);
   } else if (deviceOutput === 'Exterior Receps') {
      db.addSensorEvent('s-elec-used-exterior-receps', deviceReading);
   } else if (deviceOutput === 'Water Pump') {
      db.addSensorEvent('s-elec-used-water-supply-pump-recep', deviceReading);
   } else {
      console.log(output[i]['v']);
   }
};

setInterval(function() {
   console.log("Polling egauge");
   var options = {
      host: '192.168.2.5',
      port: 80,
      path: '/cgi-bin/egauge',
      method: 'GET'
   };
   http.get(options, function(response) {
      if (response.statusCode === 200) {
         response.on('data', function(chunk) {
            var xml = chunk.toString();
            parseString(xml, function(err, result) {
               if (err) {
                  console.log("error getting s-elec data from egauge");
                  callback(err, null);
               }

               var output = result['data']['r'];
               for (var i = 0; i < output.length; i++) {
                  var deviceOutput = output[i]['$']['n'];
                  insertReading(deviceOut, output[i]['v']);
               };
            });
         });
      } else {
         console.log("bad request");
      }
   }).on('error', function(e) {
      console.log("error: " + e.message);
   });
}, 15 * 1000);

/*
setInterval(function() {
    var random = randomNum(50.1, 70.9);
    console.log("Adding value " + random * 1000);
    db.addSensorEvent('s-temp-lr', random * 1000);
}, 6 * 1000);


// inserting random data into egauge table for testing on vps
setInterval(function() {
    var random = (Math.floor (Math.random() * Math.pow(2,12)) + (Math.floor (Math.random() * Math.pow(2,2)) / Math.pow(10,2)))
    // console.log(random);
      db.addEgaugeEvent(parseInt(random * 1000), parseInt(random / random));
}, 60 * 1000);

function randomNum(min, max)
{
   return (Math.random() * ( min - max) + max).toFixed(3);
}
*/