var express = require('express');
var router = express.Router();

router.get('/', function(req, res, next)
{
   var dev = req.query.device;
   if(dev === undefined)
   {
      res.status(400).send({error: 'No device specified'});
   }
   else
   {
      res.send({device : dev});
   }
});

module.exports = router;
