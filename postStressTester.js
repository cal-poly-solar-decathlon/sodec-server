var request = require('superagent');
var serverString = 'localhost:3000/srv';

console.log('much doge, wow');

setInterval(function() {
   var postObj = {
      status : 444,
      secret : "derpDerpDERPDERP"
   };

   request
        .post(serverString + '/record-reading?device=s-temp-testing-blackhole')
        .send(postObj)
        .end(function(err, res) {
            if(res.ok) {
               console.log(JSON.stringify(res.body));
            }
            else {
               console.log('Error: ' + res.text);
            }
        });
}, 100);
