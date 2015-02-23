var express = require('express');
var router = express.Router();

/* GET temperature of the living room (random for now). */
router.get('/', function(req, res, next) {
  var isTemp = getRandomTemp();
  res.send({temp: isTemp});
});

function getRandomTemp()
{
   return (Math.random() * ( 45.0 - 120.0) + 120.0).toFixed(3);
}

module.exports = router;
