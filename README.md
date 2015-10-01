This repository contains ... well, it actually contains two complete servers.
One is written in JavaScript. One is written in Racket. We're currently using
the one written in Racket. Because Racket is more awesome.

Also, this server depends on InfluxDB, which has some rough edges but seems
to be vastly more responsive than MySQL for the kinds of queries that we want
to be running.

# Architecture

I'm going to imagine myself explaining the system to a Racket expert. This is
kind of the "if I get run over by a bus" document.

The house includes a Raspberry Pi 2, running Raspbian. This raspberry pi
interacts with the world strictly through ethernet, no interesting hardware
connections. The router knows to assign it address 192.168.1.3.

The remote temperature and humidity data come from custom arduino minis that
communicate to an arduino mega that sits right next to the pi and sends it
HTTP POST requests to register humidity and temperature data about every
45 seconds.

The electricity data comes from two eGauges mounted next to the electrical
panel. The Pi uses the eGauge's XML interface to query instantaneous data
every 15 seconds.

The server that handles the POST requests and queries the eGauges is written
in Racket, and it's that code that lives in this repository. It's daemonized
after a fashion using djb's 'daemontools' architecture, so there's a 'run'
script that lives in /etc/service/sodec-server. If it dies, it will be
automatically restarted.

To communicate with the outside world, this server also responds to a set of
GET requests, detailed in the apiary.apib file.

The server uses InfluxDB to store its data.

Hope I didn't suffer too much when that bus hit me.

-- John Clements, 2015-10-01
