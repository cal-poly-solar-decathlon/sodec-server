var express = require('express');
var router = express.Router();

router.get('/timestamp/', function(req, res, next) {
  var curTime = Math.floor(new Date().getTime() / 1000);

  res.send({"timestamp" : curTime});
});

module.exports = router;
