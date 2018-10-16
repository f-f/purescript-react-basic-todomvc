module Todo.View where

import React.Basic (JSX)

classy
  :: ({ className :: String, children :: Array JSX } -> JSX)
  -> String
  -> (Array JSX -> JSX)
classy element className children = element { className, children }
