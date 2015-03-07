var mysql     = require('mysql');



// Establishes connections. Returns connection
exports.initConnect = function() {
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
    return connection;
};

// insert device
exports.addNewDevice = function(connection, device) {
    connection.query("INSERT INTO devices" +
                     " VALUES (" + device + ");", function(err, result) {
            if(err) {
                console.log(err);
            }
        });
};

// insert control event results
exports.addEventResults = function(connection, code, details) {
    connection.query("INSERT INTO controleventresults" +
                     " VALUES (" + code + "," + details + ");", function(err, result) {
            if(err) {
                console.log(err);
            }
        });
};

// insert control event result codes
exports.addResultCode = function(connection, resultCode) {
    connection.query("INSERT INTO controleventresultcodes" +
                     " VALUES (" + resultCode + ");", function(err, result) {

            if(err) {
                console.log(err);
            }
        });
};

// insert control events
exports.addControlEvent = function(connection, device, setting) {
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
exports.addSensorEvent = function(connection, device, reading) {
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
exports.getLastSensor = function(connection, callback) {
    connection.query("SELECT * FROM sensorevents " +
                     " ORDER BY id DESC LIMIT 1;", function(err, result) {
        if (err) {
            callback(err, null);
        }
        else {
            callback(null, result);
        }
    });
}

// Fetch all sensor events in the given range.
// More specifically, those with timestamps n such that start <= n < end.
exports.getSensorEventRange = function(connection, device, from, to, callback) {
    connection.query("SELECT * FROM sensorevents" +
                     " WHERE timestamp >= '" + from  +"' AND " +
                     " timestamp < '" + to + "';", function(err, result) {
        if (err) {
            callback(err, null);
        }
        else {
            callback(null, result);
        }
    });
};

// Fetch all control events in the given range.
// More specifically, those with timestamps n such that start <= n < end.
exports.getControlEventRange = function(connection, device, from, to, callback) {
    connection.query("SELECT * FROM controlevents" +
                     " WHERE timestamp >= '" + from  +"' AND " +
                     " timestamp < '" + to + "';", function(err, result) {
        if (err) {
            callback(err, null);
        }
        else {
            callback(null, result);
        }
    });
};

// Get last event for egauge
exports.getLastEgauge = function(connection, callback) {
    connection.query("SELECT * FROM egauge " +
                     " ORDER BY id DESC LIMIT 1;", function(err, result) {
        if (err) {
            callback(err, null);
        }
        else {
            callback(null, result);
        }
    });
};

// insert egauge events
exports.addEgaugeEvent = function(connection, usage, generation) {
    connection.query("INSERT INTO egauge" +
                     " (`usage`, `generation`)" +
                     " VALUES (" + usage + "," + generation + ");", function(err, result) {

            if(err) {
                console.log("error adding sensor event " + usage + " with " + generation);
                console.log(err);
                throw err;
            }
        });
}

