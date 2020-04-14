module Todo.Task where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Monoid (guard)
import Data.String as String
import Effect (Effect)
import React.Basic (JSX, StateUpdate(..))
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events as DOM.Events
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
    } props 

render :: React.Self Props State -> JSX
render self@{state, props} =
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
                -- Set the field in edit mode when focused
                { onDoubleClick: DOM.Events.capture_ (send self Focus)
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
            -- Update the input field
            , onChange: DOM.Events.capture DOM.Events.targetValue (send self <<< Change)
            -- Commit our changes to the parent component once we're done editing
            , onBlur: DOM.Events.capture_ (send self Commit)
            -- Special case some keys that might be inserted: on Enter commit the changes, 
            -- on Esc discard them - otherwise, type normally
            , onKeyDown: Events.handler DOM.Events.key (send self <<< KeyDown)
            }
        ]
    }
  where
    send = React.runUpdate \_self -> 
      case _ of
        Focus ->
          Update $ self.state { edits =  Just self.props.task.description }

        Change value ->
          Update (self.state { edits = value })

        KeyDown key ->
          case key of
            Just "Escape" -> Update $ self.state { edits = Nothing }
            Just "Enter"  -> commitAction
            _             -> NoUpdate

        Commit -> commitAction

    commitAction =
      let newDescription = String.trim $ fromMaybe "" self.state.edits
      in case newDescription of
        "" ->
          NoUpdate
        _ ->
          let
            state' = self.state { edits = Nothing }
          in
            UpdateAndSideEffects state' (const $ self.props.onCommit newDescription)

    classNames = String.joinWith " "
                [ guard (isJust state.edits) "editing"
                , guard props.task.completed "completed"
                ]

    -- | The description of the task is either the edited one if present,
    --   or the original description
    description = fromMaybe props.task.description state.edits
