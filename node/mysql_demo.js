
var db         = require('./dbConnect.js');


// Just some test data
for (var i = 0; i < 10; i++)
    console.log(db.addSensorEvent('c-light-bath', NaN));

// db.getLastSensor(connection, function(err, result) {
//     if (err) {
//             // error handling code goes here
//             console.log("ERROR : ",err);            
//         } else {            
//             // code to execute on data retrieval
//             console.log("result from last sensor is : ", result);   
//         }
// });
