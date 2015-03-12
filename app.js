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

var db  = require('./dbConnect.js');
var http = require('http');

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
    var options = {
      // url: 'http://google.com'
      host: '192.168.2.2',
      port: 80,
      path: '/cgi-bin/egauge-show?c',
      method: 'GET'
      // host: 'egauge15668',
      // path: '/cgi-bin/egauge-show?c'
    };
    // request('http://egauge15668/cgi-bin/egauge-show?c', function (error, response, body) {
      http.get(options, function(response) {
         // console.log("response.statusCode: " + response.statusCode);

         if (response.statusCode === 200) {
            response.on('data', function(chunk) {
               // console.log("BODY: " + chunk);
               var text = chunk.toString().split('\n');
               // console.log("Text: " + text);
               elements = text[1];
               console.log("ELEMENTS: " + elements);
               db.addEgaugeEvent(parseInt(elements[1]), parseInt(elements[2]));
               // for (var i = 1; i < 2; i++) {
               //    elements = text[i].toString().split(',');
               //    console.log("log: " + elements[0]);
               //    db.addEgaugeEvent(elements[1], elements[2]);
               // }
            });
        } else {
            console.log("ERROR: ");
            console.log(response.statusCode);
        }
   }).on('error',function(e){
      console.log("Error: " + e.message);
      console.log( e.stack );
   });
}, 1 * 1000);

setInterval(function() {
    var random =randomNum(50.1, 70.9);
    console.log("Adding value " + random);
    db.addSensorEvent('s-temp-lr', random);
}, 5000);


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
