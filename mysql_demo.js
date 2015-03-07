
var db         = require('./dbConnect.js');
var connection = db.initConnect();


// Just some test data
for (var i = 0; i < 10; i++)
    db.addSensorEvent(connection, 'c-light-bath', i);

db.getLastSensor(connection, function(err, result) {
    if (err) {
            // error handling code goes here
            console.log("ERROR : ",err);            
        } else {            
            // code to execute on data retrieval
            console.log("result from last sensor is : ", result);   
        }
});

connection.end();