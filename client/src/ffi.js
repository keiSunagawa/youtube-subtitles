"use strict";

// module Kerfume.FFI

exports.makeIframe = function (h) {
  try {
    var pat1 = /src=\"\//g
    var pat2 = /href=\"\//g
    var h2 = h.replace(pat1, "src=\"https://www.youtube.com/")
    var h3 = h2.replace(pat2, "href=\"https://www.youtube.com/")

    $("div#content").html(h3)
  } catch(e) {
    console.log("not use jQuery")
  }
}
