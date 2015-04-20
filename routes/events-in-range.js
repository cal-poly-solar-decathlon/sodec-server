var express = require('express');
var router  = express.Router();
var db      = require('../dbConnect.js');

// Format of time is given as ISO 8601 format: YYYY-MM-DDTHH:MM:SS
// Example: 2015-03-07T12:00:12
// Can search by day also YYYY-MM-DD

router.get('/', function(req, res, next) {
   var dev = req.query.device;

   var startSeconds = parseInt(req.query.start, 10) * 1000;
   var endSeconds = parseInt(req.query.end, 10) * 1000;

   var startDate = new Date();
   var endDate = new Date();

   startDate.setTime(startSeconds);
   endDate.setTime(endSeconds);


   console.log(startDate.toISOString());
   console.log(endDate.toISOString());

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

      console.log('The timestamp is ' + startDate + ", " + endDate);
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

                            if(result.length <= 0)
                            {
                              result = 'no events';
                            }
                            else {
                              if (dev == 'egauge')
                                result = deltaEgauge(result);
                              else
                                result = deltaSequence(result);
                            }

                            console.log(result);

                            json = JSON.stringify(result);

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


function deltaSequence(result) {
  var time = (new Date(result[0]['timestamp']).getTime() / 1000);
  var status = result[0]['status'];

  var json = {"baseTimestamp": time,
              "baseStatus": status,
              "seriesData": [] };

  for (var i = 1; i < result.length; i++) {
    console.log(result[i]['timestamp']);
    var newTime = (new Date(result[i]['timestamp']).getTime() / 1000)
    var timeDelta =  newTime - time;
    time = newTime;

    var newStatus = result[i]['status'];
    var statusDelta = newStatus - status;
    status = newStatus;

    var data = [timeDelta, statusDelta];
    json["seriesData"].push(data);
  }

  return json;
}

function deltaEgauge(result) {
  var time = (new Date(result[0]['timestamp']).getTime() / 1000);
  var status = [result[0]['usage'], result[0]['generation']];



  var json = {"baseTimestamp": time,
              "baseStatus": status,
              "seriesData": []};

  for (var i = 1; i < result.length; i++) {
    console.log(result[i]['timestamp']);
    var newTime = (new Date(result[i]['timestamp']).getTime() / 1000)
    var timeDelta =  newTime - time;
    time = newTime;

    var newStatus = [result[i]['usage'], result[i]['generation']];

    var statusDelta = [newStatus[0] - status[0],
                       newStatus[1] = status[1]];
    status = newStatus;

    var data = [timeDelta, statusDelta];
    json["seriesData"].push(data);
  }

  return json;
};

module.exports = router;
