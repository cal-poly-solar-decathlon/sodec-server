var express = require('express');
var router = express.Router();
var db = require('../dbConnect');

router.post('/', function(req, res, next) {
	var dev = req.query.device;
	var value = req.body.status;
	var okStatus = JSON.stringify('okay');


	if(value)
	{
		db.addSensorEvent(dev, value);
		res.status(200).send(okStatus);
	}
	else
	{
		console.log('Failed for device ' + dev);	
		res.status(400).send({error: 'No value was defined for the device'});
	}
	
});

module.exports = router;
