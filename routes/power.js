var express = require('express');
var router = express.Router();


router.get('/', function(req, res, next) {
   var power = randomPower();
   res.send({'power-consumption': power});
});

function randomPower()
{
   return (Math.random() * ( 90.0 - 100.0) + 100.0).toFixed(3);
}

module.exports = router;
