var express = require('express');
var router = express.Router();

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
      console.log('Start date is: ' + startDate);
      res.send({
         device: dev,
         start: startDate,
         end: endDate
      })
   }
});

module.exports = router;
