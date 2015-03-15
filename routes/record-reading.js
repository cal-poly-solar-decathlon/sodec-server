var express = require('express');
var router = express.Router();

router.post('/', function(req, res, next) {
	var dev = req.query.device;
	var value = req.body.status;

	db.addSensorEvent(dev, value);
});

module.exports = router;