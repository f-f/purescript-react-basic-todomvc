"use strict";

exports.startNavigation = function(navFn) {
  window.onhashchange = function () {
    // We double call here because navFn returns an effect
    // (which is just a wrapper function, so we unwrap)
    navFn(location.hash)();
  };
};
