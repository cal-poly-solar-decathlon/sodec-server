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
    connection.query("INSERT INTO devices " +
                     "VALUES ('" + device + "');", function(err, result) {
            if(err) {
                console.log(err);
            }
        });
};

// insert control event results
exports.addEventResults = function(id, code, details) {
    connection.query("INSERT INTO controleventresults " +
                     "VALUES (" + id + "," + code + ",'" + details + "');", function(err, result) {
            if(err) {
                console.log(err);
            }
        });
};

// insert control event result codes
exports.addResultCode = function(resultCode) {
    connection.query("INSERT INTO controleventresultcodes" +
                     " VALUES (" + resultCode + ");", function(err, result) {

            if(err) {
                console.log(err);
            }
        });
};

// insert control events
exports.addControlEvent = function(device, setting) {
    connection.query("INSERT INTO controlevents" +
                     " (device, setting)" +
                     " VALUES ('" + device + "'," + setting + ");", function(err, result) {

            if(err) {
                console.log("error adding control event " + device + " with " + setting);
                console.log(err);
            }
        });
};

// insert sensor events
exports.addSensorEvent = function(device, reading) {
    connection.query("INSERT INTO sensorevents" +
                 " (device, reading)" +
                 " VALUES ('" + device + "'," + reading + ");", function(err, result) {

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
        connection.query("SELECT * FROM egauge " +
                         " ORDER BY id DESC LIMIT 1;", function(err, result) {
            if (err) {
                callback(err, null);
            }
            else {
                callback(null, result);
            }
        });
    } else {
        connection.query("SELECT * FROM sensorevents " +
                         "WHERE device = '" + device + "'" + 
                         " ORDER BY id DESC LIMIT 1;", function(err, result) {
            if (err) {
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
        connection.query("SELECT * FROM egauge" +
                     " WHERE timestamp >= '" + start  +"' AND " +
                     " timestamp < '" + end + "';", function(err, result) {
            if (err) {
                callback(err, null);
            }
            else {
                console.log(start);
                console.log(end);
                callback(null, result);
            }
        });
    }
    else {
        connection.query("SELECT * FROM sensorevents" +
                         " WHERE timestamp >= '" + start + "' AND " +
                         " timestamp < '" + end + "' AND " +
                         " device = '" + device + "';", function(err, result) {
            if (err) {
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
   connection.query("SELECT * FROM controlevents" +
                     " WHERE timestamp >= '" + start  +"' AND " +
                     " timestamp < '" + end + "' AND " +
                     " device = '" + device + "';", function(err, result) {
        if (err) {
            callback(err, null);
        }
        else {
            callback(null, result);
        }
    });
};

exports.findDevice = function(device, callback) {
    connection.query("SELECT * FROM devices " +
                     "WHERE name = '" + device +"';", function(err, result) {
        if (err) {
            callback(err, null);
        }
        else {
            callback(null, result);
        }
    });
};

exports.addEgaugeEvent = function(usage, generation) {
    connection.query("INSERT INTO egauge" +
                     " (`usage`, `generation`)" +
                     " VALUES (" + usage + "," + generation + ");", function(err, result) {

            if(err) {
                console.log("error adding sensor event " + usage + " with " + generation);
                console.log(err);
                throw err;
            }
        });
};

