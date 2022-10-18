# PureScript `react-basic` TodoMVC

An implementation of [TodoMVC](http://todomvc.com/) in [PureScript](http://www.purescript.org/) using the [`react-basic`](https://github.com/lumihq/purescript-react-basic) library.

You can see this deployed [here at `purescript-react-basic-todomvc.netlify.com`](https://purescript-react-basic-todomvc.netlify.com/)

## Project structure

- Entry point for the app is [`index.js`](https://github.com/f-f/purescript-react-basic-todomvc/blob/master/index.js),
  that imports React and just instantiates the `Todo.Main` component (defined in PureScript).
  This is where you might want to hook up more JS components in your project.
- The tasklist is defined in the [`Main` component](https://github.com/f-f/purescript-react-basic-todomvc/blob/master/src/Todo/Main.purs).
  The list of tasks is kept in this component's `State`, together with some more things (e.g. the current selector, etc.)
- The above component then creates a [`Task` component](https://github.com/f-f/purescript-react-basic-todomvc/blob/master/src/Todo/Task.purs) for every task.
  The only state we need to keep in it is the current edits for a focused `Task`.
- Some things are achieved with a thin layer of JS FFI: [LocalStorage](https://github.com/f-f/purescript-react-basic-todomvc/blob/master/src/LocalStorage.js) and [routing](https://github.com/f-f/purescript-react-basic-todomvc/blob/master/src/Todo/App.js)

## Development

Download [zephyr](https://github.com/coot/zephyr/releases).

```bash
## Install npm dependencies, PureScript compiler, etc
npm install

## Build the PureScript project
npm run build

## Start the dev server with hot reload and stuff
##
## Note: Parcel has hot reload on JS files only, so if you'd like to reload
## when changing PureScript files, you have two options:
## - use an editor integration - this will run `purs ide` and recompile the files you edit
## - run `spago build --watch` in another terminal
##
## Note: the hot reload won't work if you change any FFI file,
## so you'll have to `yarn build` again in this case.
npm start
```
