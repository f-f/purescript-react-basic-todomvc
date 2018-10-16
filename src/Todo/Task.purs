module Todo.Task where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Effect (Effect)
import Effect.Class.Console as Console
import React.Basic (JSX)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (key, targetValue)
import React.Basic.Events as Events
import Todo.View (classy)


type Task =
  { description :: String
  , id          :: Int
  , completed   :: Boolean
  }

type State = { edits :: Maybe String }

type Props =
  { key      :: Int
  , task     :: Task
  , onCheck  :: Effect Unit
  , onDelete :: Effect Unit
  , onCommit :: String -> Effect Unit
  }

type SetState = (State -> State) -> Effect Unit

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
    classNames = (if props.task.completed then "completed " else "")
               <> case state.edits of
                 Just _ -> "editing"
                 Nothing -> ""

    description = fromMaybe props.task.description state.edits

    elementId = "todo-" <> show props.task.id

    onFocus = setState _ { edits = Just props.task.description }

    onChange =
      Events.handler
        (Events.merge { targetValue })
        \{ targetValue } -> setState _ { edits = targetValue }

    onBlur = Events.handler_ do
      Console.log ""
      commit

    onKeyDown =
      Events.handler
        (Events.merge { targetValue, key })
        \{ targetValue, key } -> case key of
          Just "Escape" -> setState _ { edits = Nothing }
          Just "Enter"  -> commit
          otherwise     -> pure unit

    commit = case not (String.null newDescription) of
      true  -> do
        setState _ { edits = Nothing }
        props.onCommit newDescription
      false -> pure unit
      where
        newDescription = String.trim $ fromMaybe "" state.edits

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
              , onBlur
              , onKeyDown
              }
          ]
      }
