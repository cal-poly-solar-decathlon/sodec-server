// var app = require('./app');
var http = require('http');
var WebSocketServer = require('websocket').server;
// var io = require('socket.io')(http);

var port = 8080;


// Create a web-server to host the websocket.
var server = http.createServer(function(req, res) {
  console.log((new Date()) + ' Request received for ' + req.url);
  res.writeHead(404);
  res.end();
});


// Begin listening on port 8080 for connections
server.listen(port, function() {
  console.log((new Date()) + ' Server Listening on port ' + port);
});


// Create a new WebSocket server, and filter out connections
var wsServer = new WebSocketServer({
  httpServer: server,

  autoAcceptConnections: false
});



// Function that deterines which clients can connect to the WebSocket service.
function originIsAllowed(origin) {
  return true;
}

// Method that handles WebSocket requests
wsServer.on('request', function(req) {

  // Checks if the client is authorized to connect
  if(!originIsAllowed(req.origin)) {
    req.reject();
    console.log((new Date()) + ' Connection from origin ' + req.origin + ' rejected');
    return;
  }

  // Accepts all connections on the main protocol, echoing these messages back to the client
  var connection = req.accept(null, req.origin);
  connection.on('message', function(message) {
    // Handles utf8 message formats.
    if(message.type === 'utf8') {
      console.log('Received message: ' + message.utf8Data);
      connection.sendUTF(message.utf8Data);
    }
    // handles binary message types as well.
    else if(message.type === 'binary') {
      console.log('Receieved binary message of ' + message.binaryData.length + ' bytes' );
      connection.sendBytes(message.binaryData);
    }
  });

  // Show a message when clients disconnect from the websocket.
  connection.on('close', function(reasonCode, description) {
    console.log((new Date()) + ' Peer ' + connection.remoteAddress + ' disconnected');
  });
});
