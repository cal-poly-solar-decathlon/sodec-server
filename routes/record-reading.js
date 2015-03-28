var express = require('express');
var router = express.Router();
var db = require('../dbConnect');

router.post('/', function(req, res, next) {
	var dev = req.query.device;
	var value = req.body.status;

	db.addSensorEvent(dev, value);
});

module.exports = router;