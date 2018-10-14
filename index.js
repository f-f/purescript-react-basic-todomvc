import React from 'react';
import ReactDOM from 'react-dom';

import createReactClass from 'create-react-class';
React.createClass = createReactClass;
var Main = require('./output/Main');

function main() {
  const myComponent = (
    <Main.app/>
  );

  ReactDOM.render(myComponent, document.getElementById('app'));
}


// HMR stuff
// For more info see: https://parceljs.org/hmr.html
if (module.hot) {
  module.hot.accept(function () {
    console.log('Reloaded, running main again');
    main();
  });
}

console.log('Starting app');
main();

