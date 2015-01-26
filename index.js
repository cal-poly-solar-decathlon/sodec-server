// var app = require('./app');
var app = require('express')();
var http = require('http').Server(app);
var io = require('socket.io')(http);

var port = 3001;

app.get('/', function (req, res) {
  'use strict';
  res.sendFile(__dirname + '/index.html');
});

io.on('connection', function (socket) {
  'use strict';
  console.log('Connection Successful!');

  socket.on('disconnect', function () {
    console.log('Client Disconnected');
  });
    
  socket.on('connect_failed', function (data) {
    console.log(data || 'connect_failed');
  });

  socket.emit('test', { receivers: 'everyone'});

});

http.listen(port, function () {
  'use strict';
  console.log('listening on *:' + port);
});


module.exports = function (str) {
  console.log(str || 'Rainbow');
};
