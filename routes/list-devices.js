var express = require('express');
var router  = express.Router();
var db      = require('../modules/dbConnect.js');


router.get('/', function(req, res, next)
{
    console.log("list devices");

    db.findAllDevices(function(err, result) {
        if (err) {
            res.status(400).send({error: 'error getting all devices'});
        }
        else {
            console.log(result);
            var json = JSON.stringify(result);
            console.log(json);
            res.status(200).send(json);

        }
    });
});

module.exports = router;
