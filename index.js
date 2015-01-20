// var app = require('./app');
var app = require('express')();
var http = require('http').Server(app);
var io = require('socket.io')(http);

var port = 3001;

app.get('/', function(req, res){
  res.sendFile(__dirname + '/index.html');
});

io.on('connection', function(socket){
  console.log('Connection Successful!');

  socket.emit('test', { receivers: 'everyone'});

  socket.on('disconnect', function() {
    console.log('Client Disconnected');
  });
});

http.listen(port, function(){
  console.log('listening on *:' + port);
});


module.exports = function (str) {
  console.log(str || 'Rainbow');
};


