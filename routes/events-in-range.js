var express = require('express');
var router  = express.Router();
var db      = require('../dbConnect.js');

// Format of time is given as ISO 8601 format: YYYY-MM-DDTHH:MM:SS
// Example: 2015-03-07T12:00:12
// Can search by day also YYYY-MM-DD 

router.get('/', function(req, res, next) {
   var dev = req.query.device;
   var startDate = new Date(Date.parse(req.query.start));
   var endDate = new Date(Date.parse(req.query.end));

   if(dev === undefined || startDate === undefined || endDate === undefined)
   {
      res.status(400).send({error: 'Expects device, start and end'});
   }
   else if(isNaN(startDate.getTime()) || isNaN(endDate.getTime()))
   {
      res.status(400).send({error: 'Invalid start or end date'})
   }
   else
   {
      startDate = startDate.toISOString();
      endDate = endDate.toISOString();
      // res.send({
      //    device: dev,
      //    start: startDate,
      //    end: endDate
      // })
      db.findDevice(dev, function(err, result) {
            if (err) {
                console.log("ERROR finding device: ", err);
            }
            else {
                if (result != 0) {
                    db.getSensorEventRange(dev, startDate, endDate, function(err, result) {
                        if (err) {
                            // error handling code goes here
                            console.log("ERROR : ",err);            
                        } 
                        else {            
                            // code to execute on data retrieval
                            // console.log("result from range sensor is : ", result);   
                            
                            function convertTime(key, value) {
                                if (key == "timestamp") {
                                    return (new Date(value).getTime());
                                }
                                return value;
                            }

                            var json = JSON.stringify(result, convertTime);
                            res.status(200).send(json);
                        }
                    });
                }
                else {
                    res.status(400).send("no device of name: " + dev);
                }
            }
        });
   }
});

module.exports = router;
