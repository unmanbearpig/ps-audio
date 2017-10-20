/* jshint maxparams: false */
/* global exports, XMLHttpRequest */
"use strict";

// module WebAudio.DynamicsCompressorNode

exports.createDynamicsCompressor = function(ctx) {
  return function() {
      console.log("create dynamics compressor node");
    return ctx.createDynamicsCompressor();
  };
};
