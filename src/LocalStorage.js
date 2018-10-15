"use strict";

/*
  It would be very tempting to write these as:
  exports.setItem_ =  window.localStorage.setItem;

  However, it creates weird errors and does not work as expected.

  JavaScript, not even once.
*/

exports.setItem_ = function(k, v) {
  window.localStorage.setItem(k, v);
};

exports.getItem_ = function(k) {
  return window.localStorage.getItem(k);
};

exports.removeItem_ = function(k) {
  window.localStorage.removeItem(k);
}
