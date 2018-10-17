module Todo.Task where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Monoid (guard)
import Data.String as String
import Effect (Effect)
import React.Basic (JSX)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (key, targetValue)
import React.Basic.Events as Events
import Todo.View (classy)

-- | Type of our single Todo item
type Task =
  { description :: String
  , id          :: Int
  , completed   :: Boolean
  }

-- | Every component keeps track of the fact that it's being edited,
--   and what's the new value
type State = { edits :: Maybe String }

-- | Callbacks that we pass into the component to update the main list
--   in the parent's state when things happen.
--   Note: the `key` here is needed so that React can disambiguate our items on render
type Props =
  { key      :: Int
  , task     :: Task
  , onCheck  :: Effect Unit
  , onDelete :: Effect Unit
  , onCommit :: String -> Effect Unit
  }

type SetState = (State -> State) -> Effect Unit

-- | We start in a "non editing" state
initialState :: State
initialState = { edits: Nothing }

component :: React.Component Props
component = React.component
    { displayName: "Task"
    , render
    , initialState
    , receiveProps
    }
    where
      receiveProps _ = pure unit

render :: forall r. { state :: State, setState :: SetState, props :: Props | r } -> JSX
render { state, setState, props } =
  let
    classNames = String.joinWith " "
                   [ guard (isJust state.edits) "editing"
                   , guard props.task.completed "completed"
                   ]

    -- | The description of the task is either the edited one if present,
    --   or the original description
    description = fromMaybe props.task.description state.edits

    -- | Action to set the field in edit mode when focused
    onFocus = setState _ { edits = Just props.task.description }

    -- | Action to commit our changes to the parent component once we're done editing
    commit = case newDescription of
      "" -> pure unit
      _ -> do
        setState _ { edits = Nothing }
        props.onCommit newDescription
      where
        newDescription = String.trim $ fromMaybe "" state.edits

    -- | Handler to update the input field
    onChange =
      Events.handler
        (Events.merge { targetValue })
        \{ targetValue } -> setState _ { edits = targetValue }

    -- | Handler for special casing some keys that might be inserted:
    --   on Enter commit the changes, on Esc discard them
    --   (otherwise, type normally)
    onKeyDown =
      Events.handler
        (Events.merge { targetValue, key })
        \{ targetValue, key } -> case key of
          Just "Escape" -> setState _ { edits = Nothing }
          Just "Enter"  -> commit
          _             -> pure unit

  in
    DOM.li
      { className: classNames
      , children:
          [ classy DOM.div "view"
              [ DOM.input
                  { className: "toggle"
                  , "type": "checkbox"
                  , checked: props.task.completed
                  , onChange: Events.handler_ props.onCheck
                  }
              , DOM.label
                  { onDoubleClick: Events.handler_ onFocus
                  , children: [ DOM.text description ]
                  }
              , DOM.button
                  { className: "destroy"
                  , onClick: Events.handler_ props.onDelete
                  }
              ]
          , DOM.input
              { className: "edit"
              , value: description
              , name: "title"
              , onChange
              , onBlur: Events.handler_ commit
              , onKeyDown
              }
          ]
      }
