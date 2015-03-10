var express = require('express');
var router  = express.Router();
var db      = require('../dbConnect.js');


router.get('/', function(req, res, next) {
   var power = randomPower();
   // res.send({'power-consumption': power});

   db.getLastEgauge(function(err, result) {
    if (err) {
            // error handling code goes here
            console.log("ERROR : ",err);            
        } else {            
            // code to execute on data retrieval
            console.log("result from last sensor is : ", result);   
            var json = JSON.stringify(result);
            // console.log(json);
            res.send(json);
        }
	});
});

function randomPower()
{
   return (Math.random() * ( 90.0 - 100.0) + 100.0).toFixed(3);
}

module.exports = router;
