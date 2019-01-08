module Todo.Task where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Monoid (guard)
import Data.String as String
import Effect (Effect)
import React.Basic (JSX, StateUpdate(..))
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

data Action
  = Focus
  | Change (Maybe String)
  | KeyDown (Maybe String)
  | Commit

type SetState = (State -> State) -> Effect Unit

-- | We start in a "non editing" state
initialState :: State
initialState = { edits: Nothing }

taskComponent :: React.Component Props
taskComponent = React.createComponent "Task"

component :: Props -> JSX
component props = React.make taskComponent
    { render
    , initialState
    , update
    } props
    where

      update self action =
        case action of
          Focus ->
            Update $ self.state { edits =  Just self.props.task.description }

          Change value ->
            Update (self.state { edits = value })

          KeyDown key ->
            case key of
              Just "Escape" -> Update $ self.state { edits = Nothing }
              Just "Enter"  -> commit
              _             -> NoUpdate

          Commit ->
            commit

        where
          newDescription :: String
          newDescription = String.trim $ fromMaybe "" self.state.edits

          commit :: StateUpdate Props State Action
          commit =
            case newDescription of
              "" ->
                NoUpdate
              _ ->
                let
                  state' = self.state { edits = Nothing }
                in
                 UpdateAndSideEffects state' (const $ self.props.onCommit newDescription)

render :: React.Self Props State Action -> JSX
render self@{state, props}  =
  let
    classNames = String.joinWith " "
                   [ guard (isJust state.edits) "editing"
                   , guard props.task.completed "completed"
                   ]

    -- | The description of the task is either the edited one if present,
    --   or the original description
    description = fromMaybe props.task.description state.edits

    -- | Action to set the field in edit mode when focused
    onFocus = React.capture_ self Focus

    -- | Action to commit our changes to the parent component once we're done editing
    commit =
      React.capture_ self Commit

    -- | Handler to update the input field
    onChange =
      React.capture self targetValue Change

    -- | Handler for special casing some keys that might be inserted:
    --   on Enter commit the changes, on Esc discard them
    --   (otherwise, type normally)
    onKeyDown = React.monitor self key KeyDown
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
                  { onDoubleClick: onFocus
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
              , onBlur: commit
              , onKeyDown
              }
          ]
      }
