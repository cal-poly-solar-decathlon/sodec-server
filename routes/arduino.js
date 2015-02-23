var express = require('express');
var router = express.Router();

/* GET temperature of the living room (random for now). */
router.post('/', function(req, res, next) {
   var temp = req.body.temperature;
   var humidity = req.body.humidity;
   res.json({
      message :'Posted Successfully!!!!\n Got temp ' + temp
   });

   console.log('Temperature received of ' + temp);
   console.log('Hudmidity received of ' + humidity);
});

module.exports = router;
