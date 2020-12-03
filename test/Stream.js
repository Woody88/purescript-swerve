"use strict";

exports.newStream = str => () => {
  const stream = require('stream').Readable();
  stream.push(str);
  stream.push(null);

  return stream;
}