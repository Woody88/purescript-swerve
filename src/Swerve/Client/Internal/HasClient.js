const Stream = require('stream')


exports.toReadableStream = (str) => () => {
  const readableStream = new Stream.Readable({
    read() {}
  })

  readableStream.push(str)
  return readableStream;
}