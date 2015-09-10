This repository contains ... well, it actually contains two complete servers.
One is written in JavaScript. One is written in Racket. We're currently using
the one written in Racket. Because Racket is more awesome.

Also, this server depends on InfluxDB, which has some rough edges but seems
to be vastly more responsive than MySQL for the kinds of queries that we want
to be running.

Big Picture:

This server runs on a Raspberry Pi. It listens on port 3000. POST requests are
issued by the temperature and humidity sensors to record new readings. GET
requests are issued by the web page in order to gather and display information.
The API is documented in apiary.apib.
