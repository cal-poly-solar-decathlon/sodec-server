var assert = require('assert');
var app =  require('../bin/www');
var request = require('supertest');
var serverString = 'localhost:3000/srv';

describe('Devices Route', function() {
	describe('simpleGet', function() {
		it('should return a simple device', function(){
			request(serverString)
				.get('/latest-event?device=s-temp-lr')
				.expect('Content-Type', /json/)
				.expect(200)
				.end(function(err, res){
					if (err) return done(err);
					done();
				});
		});
	});
});


describe('Record Reading Route', function() {
	describe('Sends sample POST, for the s-temp-lr device.', function(){
		it('Should post a value of 555 for the s-temp-lr device', function()
		{
			var sensorReadout = {
				"status" : 555,
				"secret" : "somethingRandom"
			};

			request(serverString)
				.post('/record-reading?device=s-temp-lr')
				.send(sensorReadout)
				.expect(200)
				.end(function(err, res){
					if(err) return done(err);
					done();
				})
		});

	});
});
