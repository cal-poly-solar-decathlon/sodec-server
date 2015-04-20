var mysql     = require('mysql');



// Establishes connections.
var express   = require("express");
var app       = express();

var connection = mysql.createConnection({
  host     : 'localhost',
  user     : 'root',
  password : 'sodec',
  database : 'sodec'
});

connection.connect((function(err){
    if(!err) {
        console.log("Database is connected ... \n\n");
    } else {
        console.log("Error connecting database ... \n\n");
    }
}));

exports.endConnection = function() {
    connection.end();
}

// insert device
exports.addNewDevice = function(device) {
    var query = connection.query("INSERT INTO devices " +
                     "VALUES (?)", device, function(err, result) {
            if(err) {
              console.log("error with this query: " + query.sql);
              console.log(err);  
            }
        });
};

// insert control event results
exports.addEventResults = function(id, code, details) {
    var query = connection.query("INSERT INTO controleventresults " +
                     "VALUES (?, ?, ?)", [id, code, details], function(err, result) {
            if(err) {
              console.log("error with this query: " + query.sql);
              console.log(err);  
            }
        });
};

// insert control event result codes
exports.addResultCode = function(resultCode) {
    var query = connection.query("INSERT INTO controleventresultcodes" +
                     " VALUES (?)", resultCode, function(err, result) {

            if(err) {
              console.log("error with this query: " + query.sql);
              console.log(err);
            }
        });
};

// insert control events
exports.addControlEvent = function(device, setting) {
    var query = connection.query("INSERT INTO controlevents" +
                     " (device, setting)" +
                     " VALUES (?,?)", [device, setting], function(err, result) {

            if(err) {
                console.log("error adding control event " + device + " with " + setting);
                console.log("error with this query: " + query.sql);
                console.log(err);
            }
        });
};

// insert sensor events
exports.addSensorEvent = function(device, reading) {
    connection.query("INSERT INTO sensorevents" +
                 " (device, reading)" +
                 " VALUES (?, ?)", [device, reading], function(err, result) {

        if(err) {
            console.log("error adding sensor event " + device + " with " + reading);
            console.log(err);
            throw err;
        }
    });
}

// get last event
exports.getLastSensorEvent = function(device, callback) {
     if (device == "egauge") {
        var query = connection.query("SELECT timestamp, `usage`, `generation` FROM egauge " +
                         " ORDER BY id DESC LIMIT 1;", function(err, result) {
            if (err) {
                console.log("error with this query: " + query.sql);
                callback(err, null);
            }
            else {
                callback(null, result);
            }
        });
    } else {
        var query = connection.query("SELECT device as 'device-id', reading as status, timestamp FROM sensorevents " +
                         "WHERE device = ?" + 
                         " ORDER BY id DESC LIMIT 1", device, function(err, result) {
            if (err) {
                console.log("error with this query: " + query.sql);
                callback(err, null);
            }
            else {
                callback(null, result);
            }
        });
    }
}

// Fetch all sensor events in the given range.
// More specifically, those with timestamps n such that start <= n < end.
exports.getSensorEventRange = function(device, start, end, callback) {
    if (device == "egauge") {
        var query = connection.query("SELECT timestamp, `usage`, `generation` FROM egauge" +
                     " WHERE timestamp >= ? AND " +
                     " timestamp < ?", [start, end], function(err, result) {
            if (err) {
                console.log("error with this query: " + query.sql);
                callback(err, null);
            }
            else {
                callback(null, result);
            }
        });
    }
    else {
        var query = connection.query("SELECT device, reading as status, timestamp FROM sensorevents" +
                         " WHERE timestamp >= ? AND " +
                         " timestamp < ? AND " +
                         " device = ?", [start, end, device], function(err, result) {
            if (err) {
                console.log("error with this query: " + query.sql);
                callback(err, null);
            }
            else {
                callback(null, result);
            }
        });
    }
};

// Fetch all control events in the given range.
// More specifically, those with timestamps n such that start <= n < end.
exports.getControlRange = function(device, start, end, callback) {
   var query = connection.query("SELECT * FROM controlevents" +
                     " WHERE timestamp >= ? AND " +
                     " timestamp < ? AND " +
                     " device = ?", [start, end, device], function(err, result) {
        if (err) {
            console.log("error with this query: " + query.sql);
            callback(err, null);
        }
        else {
            callback(null, result);
        }
    });
};

exports.findDevice = function(device, callback) {
    var query = connection.query("SELECT * FROM devices " +
                     "WHERE name = ?", device, function(err, result) {
        if (err) {
            console.log("error with this query: " + query.sql);
            callback(err, null);
        }
        else {
            callback(null, result);
        }
    });
};

exports.addEgaugeEvent = function(usage, generation) {
    var query = connection.query("INSERT INTO egauge" +
                     " (`usage`, `generation`)" +
                     " VALUES (?, ?)", [usage, generation], function(err, result) {

            if(err) {
                console.log("error with this query: " + query.sql);
                console.log(err);
                throw err;
            }
        });
};

exports.findAllDevices = function(callback) {
  var query = connection.query("SELECT name as id, " +
                                "description FROM devices", function(err, result) {
        if (err) {
            console.log("error with this query: " + query.sql);
            callback(err, null);
        }
        else {
            console.log(result);
            callback(null, result);
        }
    });
}

