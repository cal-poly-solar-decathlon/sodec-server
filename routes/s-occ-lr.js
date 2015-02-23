var express = require('express');
var router = express.Router();


router.get('/', function(req, res, next) {
   var occupancy = randomOccupancy();
   res.send({'occupancy': occupancy});
});

function randomOccupancy()
{
   return Math.floor((Math.random() * 5));
}

module.exports = router;
