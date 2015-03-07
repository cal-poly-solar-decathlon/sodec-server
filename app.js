var express = require('express');
var path = require('path');
var favicon = require('serve-favicon');
var logger = require('morgan');
var cookieParser = require('cookie-parser');
var bodyParser = require('body-parser');
var webSocketServer = require('./webSockets/webSocketServer');
//var mySqlDemo = require('./mysql_demo');

var routes = require('./routes/index');
var users = require('./routes/users');
var arduino = require('./routes/arduino');
var power = require('./routes/power');
var latestEvent = require('./routes/latest-event');
var eventsInRange = require('./routes/events-in-range');

var request = require('request');
var jquery = require('jquery');

var app = express();

var db         = require('./dbConnect.js');
var connection = db.initConnect();

// view engine setup
app.set('views', path.join(__dirname, 'views'));
app.set('view engine', 'jade');

// uncomment after placing your favicon in /public
//app.use(favicon(__dirname + '/public/favicon.ico'));
app.use(logger('dev'));
app.use(bodyParser.json());
app.use(bodyParser.urlencoded({ extended: false }));
app.use(cookieParser());
app.use(express.static(path.join(__dirname, 'public')));

app.use('/srv/', routes);
app.use('/srv/', users);
app.use('/srv/', arduino);
app.use('/srv/power', power);
app.use('/srv/latest-event', latestEvent);
app.use('/srv/events-in-range', eventsInRange);


// catch 404 and forward to error handler
app.use(function(req, res, next) {
    var err = new Error('Not Found');
    err.status = 404;
    next(err);
});

// error handlers

// development error handler
// will print stacktrace
if (app.get('env') === 'development') {
    app.use(function(err, req, res, next) {
        res.status(err.status || 500);
        res.render('error', {
            message: err.message,
            error: err
        });
    });
}

// production error handler
// no stacktraces leaked to user
app.use(function(err, req, res, next) {
    res.status(err.status || 500);
    res.render('error', {
        message: err.message,
        error: {}
    });
});


module.exports = app;

// polls egauge for data every 1000ms
setInterval(function() {
    console.log("Polling egauge");
    request('http://egauge15668/cgi-bin/egauge-show?c', function (error, response, body) {
        if (!error && response.statusCode === 200) {
            // console.log(body);
            var text = body.split('\n');
            for (var i = 1; i < 2; i++) {
                elements = text[i].split(',');
                console.log(elements);
                // for (var j = 0; j < elements.length; j++) {
                //     console.log(elements[j]);
                //     // db.addSensorEvent(connection, device, elements[j]);
                // }
            }
        } else {
            console.log(error);
        }
    });
}, 3 * 1000);


// inserting random data into egauge table for testing on vps
// setInterval(function() {
//     var random = (Math.floor (Math.random() * Math.pow(2,12)) + (Math.floor (Math.random() * Math.pow(2,2)) / Math.pow(10,2)))
//     console.log(random);
// }, 30 * 1000);