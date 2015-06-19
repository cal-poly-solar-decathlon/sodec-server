#!/usr/bin/env node
'use strict';
var meow = require('meow');
var soldecserver = require('./');

var cli = meow({
  help: [
    'Usage',
    '  soldecserver <input>',
    '',
    'Example',
    '  soldecserver Unicorn'
  ].join('\n')
});

soldecserver(cli.input[0]);
