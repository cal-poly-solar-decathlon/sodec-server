> Basic NodeJS web-server for the Solar Decathlon Project

## Before Attempting to Run:

Install node.js: http://nodejs.org/
Make sure that Node and npm are in your path (This should be an option in the installer)

## Install
To install all dependencies run:

npm install


## Usage

To run the server, use the following command:

node index

This will create a websocket server on port 8080 of the host machine. You can then connect to it using your client of choice.

To test the connection, run the following command in the main directory:
  node testClient

It should echo the time between the server and the client.
