/*global describe, it */
'use strict';
var assert = require('assert');
var soldecserver = require('../');

describe('soldecserver node module', function () {
  it('must have at least one test', function () {
    soldecserver();
    assert(true, 'I was too lazy to write any tests. Shame on me.');
  });
});
