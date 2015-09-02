var express = require('express');
var router = express.Router();
var db = require('../modules/dbConnect');

router.post('/record-reading/', function(req, res, next) {
	var dev = req.query.device;
	var value = parseInt(req.body.status);
	var okStatus = JSON.stringify('okay');

	db.findDevice(dev, function(err, result) {
    	if (result !== 0) {
        	if(!isNaN(value))
			{
				db.addSensorEvent(dev, value);
				res.status(200).send(okStatus);
			}
			else
			{
				console.log('Failed for device ' + dev);
				res.status(400).send({error: 'No value was defined for the device'});
			}
		}
		else {
			res.status(400).send({error: 'No device found'});
		}
	});
});

module.exports = router;