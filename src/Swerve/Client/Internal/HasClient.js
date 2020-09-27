"use strict";

const { Readable } = require("stream");

exports.toStream = (input) => () => { 
  var stream = new Readable();
  stream.push(input);
  stream.push(null); 
  return stream; 
}