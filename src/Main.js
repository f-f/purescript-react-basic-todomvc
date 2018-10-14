"use strict";

var locationHashChanged = function () {
  if (location.hash === "#foo") {
    console.log("foo");
  } else {
    console.log("bar");
  }
};

exports.startNavigation = function() {
  window.onhashchange = locationHashChanged;
};
