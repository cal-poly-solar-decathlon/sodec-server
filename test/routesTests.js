var assert = require('assert');
var app =  require('../bin/www');
var request = require('supertest');
var serverString = 'localhost:3000/srv';

describe('/latest-event route tests', function() {
	describe('Get latest sensor event for s-temp-lr', function() {
		it('should return a sensor reading', function(done){
			request(serverString)
				.get('/latest-event?device=s-temp-lr')
				.expect(200)
				.end(function(err, res){
					if (err) return done(err);
					done();
				});
		});
	});
});


describe('/events-in-range route tests', function() {
	describe('Get sensor events from the last day', function() {
		it('should return an array of events', function(done) {
			var today = Date.now();
			var yesterday = today - 86400000;

			var routeText = '/events-in-range?device=s-temp-lr&start=' + (new Date(yesterday).toDateString()) + '&end=' + (new Date(today).toDateString());

			request(serverString)
				.get(routeText)
				.expect(200, done);

		});
	})

	describe('Improper date/time format for events-in-range', function(){
		it('should return a 400 error', function(done) {
			var invalidDate = null;
			var routeText = '/events-in-range?device=s-temp-lr&start=' + invalidDate + '&end=' + invalidDate;

			request(serverString)
				.get(routeText)
				.expect(400, done);
		});

		it('should return invalid date and 400 error', function(done) {
			var today = Date.now();
			var yesterday = today - 86400000;

			var routeText = '/events-in-range?device=s-temp-lr&start=' + (new Date(today).toDateString()) + '&end=' + (new Date(yesterday).toDateString());

			request(serverString)
				.get(routeText)
				.expect(200, done);

		});
	});
});


describe('Record Reading Route', function() {
	describe('Sends sample POST, for the s-temp-lr device.', function(){
		it('Should post a value of 555 for the s-temp-lr device', function(done)
		{
			var sensorReadout = {
				"status" : 555,
				"secret" : "somethingRandom"
			};

			request(serverString)
				.post('/record-reading?device=s-temp-lr')
				.send(sensorReadout)
				.expect(200, done);
		});

	});
});
