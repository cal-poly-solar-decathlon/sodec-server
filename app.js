var express = require('express');
var path = require('path');
var favicon = require('serve-favicon');
var logger = require('morgan');
var cookieParser = require('cookie-parser');
var bodyParser = require('body-parser');
var webSocketServer = require('./webSockets/webSocketServer');
var cors = require('cors');
//var mySqlDemo = require('./mysql_demo');

var routes = require('./routes/index');
var users = require('./routes/users');
var latestEvent = require('./routes/latest-event');
var eventsInRange = require('./routes/events-in-range');
var recordReading = require('./routes/record-reading');
var timestamp = require('./routes/timestamp');
var ping = require('./routes/ping');
var listDevices = require('./routes/list-devices');

var db  = require('./modules/dbConnect.js');
var egauge = require('./modules/egauge.js');

var app = express();

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


// Enables CORS support on all routes
app.use(cors());

// Changes all content types to json for requests

app.use(function(req, res, next) {
  res.contentType('application/json');
  next();
});


app.use('/srv/', routes);
app.use('/srv/users', users);
app.use('/srv/latest-event', latestEvent);
app.use('/srv/events-in-range', eventsInRange);
app.use('/srv/record-reading', recordReading);
app.use('/srv/timestamp', timestamp);
app.use('/srv/ping', ping);
app.use('/srv/list-devices', listDevices);


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