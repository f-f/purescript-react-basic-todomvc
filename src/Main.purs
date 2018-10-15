module Main where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Effect (Effect)
import Effect.Uncurried (EffectFn1)
import Footer (Visibility(..))
import Footer as Footer
import React.Basic (JSX)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (key, preventDefault, targetChecked, targetValue)
import React.Basic.Events as Events
import Task (Task)
import Task as Task
import Utils (classy)

foreign import startNavigation :: Effect Unit


type Props = {}

type State =
  { tasks      :: Array Task
  , newTodo    :: String
  , uid        :: Int
  , visibility :: Visibility
  }

type SetState = (State -> State) -> Effect Unit


initialState :: State
initialState =
  { tasks: []
  , visibility: All
  , newTodo: ""
  , uid: 0
  }

app :: React.Component Props
app = React.component
  { displayName: "App"
  , initialState
  , receiveProps
  , render
  }
  where
    receiveProps = \_ -> pure unit

render :: forall r. { state :: State, setState :: SetState | r } -> JSX
render { state, setState } =
  classy DOM.div "todomvc-wrapper"
    [ classy DOM.section "todoapp"
      [ taskEntry state.newTodo onEditNewTodo onSubmitNewTodo
      , taskList state.tasks state.visibility onTaskCheck onTaskDelete onTaskEdit onTaskUpdate checkAllTasks
      , React.element
          Footer.component
            { tasks: state.tasks
            , onClearCompleted: clearCompleted
            , visibility: state.visibility
            }
      ]
    ]
  where
    onEditNewTodo =
      Events.handler
        (preventDefault >>> Events.merge { targetValue })
        \{ targetValue } -> setState _ { newTodo = fromMaybe "" targetValue }

    onSubmitNewTodo =
      Events.handler
        (Events.merge { targetValue, key })
        \{ targetValue, key } -> case key of
          Just "Enter" | not (String.null newDescription) -> do
            _ <- pure preventDefault
            setState _ { newTodo = ""
                       , tasks = Array.cons newTodo state.tasks
                       , uid = state.uid + 1
                       }
          otherwise -> pure unit
            where
              newDescription = String.trim state.newTodo

              newTodo =
                { description: newDescription
                , id: state.uid
                , completed: false
                , edits: Nothing
                }

    onTaskCheck id =
      setState _ { tasks = map negateCheck state.tasks }
      where
        negateCheck task =
          if task.id == id then task { completed = not task.completed } else task

    onTaskUpdate id newDescription =
      setState _ { tasks = map updateTask state.tasks }
      where
        updateTask task =
          if task.id == id
          then task { description = newDescription, edits = Nothing }
          else task

    onTaskEdit id newEdits =
      setState _ { tasks = map editTask state.tasks }
      where
        editTask task =
          if task.id == id then task { edits = newEdits } else task

    onTaskDelete task =
      setState _ { tasks = Array.deleteBy (\a b -> a.id == b.id) task state.tasks }

    checkAllTasks =
      Events.handler
        targetChecked
        \targetChecked ->
          let
            toggle task = task { completed = fromMaybe task.completed targetChecked }
          in
            setState _ { tasks = (map toggle state.tasks) }

    clearCompleted = setState _ { tasks = Array.filter (not <<< _.completed) state.tasks }


taskEntry :: String -> Events.EventHandler -> Events.EventHandler -> JSX
taskEntry value onEdit onSubmit =
  classy DOM.header "header"
    [ DOM.h1_ [ DOM.text "todos" ]
    , DOM.input attributes
    ]
  where
    attributes =
      { className: "new-todo"
      , placeholder: "What needs to be done?"
      , autoFocus: "true"
      , value: value
      , name: "newTodo"
      , onChange: onEdit
      , onKeyDown: onSubmit
      }

taskList
  :: Array Task
  -> Visibility
  -> (Int -> Effect Unit)
  -> (Task -> Effect Unit)
  -> (Int -> (Maybe String) -> Effect Unit)
  -> (Int -> String -> Effect Unit)
  -> Events.EventHandler
  -> JSX
taskList tasks visibility onCheck onDelete onEdit onCommit checkAllTasks =
  DOM.section
    { className: "main"
    , style: DOM.css { visibility: if Array.null tasks then "hidden" else "visible" }
    , children:
        [ DOM.input toggleAllAttributes
        , DOM.label { htmlFor: "toggle-all", children: [ DOM.text "Mark all as complete" ]}
        , classy DOM.ul "todo-list" (map taskView (Array.filter isVisible tasks))
        ]
    }
  where
    toggleAllAttributes =
      { className: "toggle-all"
      , id: "toggle-all"
      , "type": "checkbox"
      , checked: Array.all _.completed tasks
      , onChange: checkAllTasks
      }

    -- | Is a task visible?
    isVisible task = case visibility of
      Completed -> task.completed
      Active    -> not task.completed
      otherwise -> true

    taskView task =
      React.element
        Task.component
          { task: task
          , onCheck: onCheck task.id
          , onDelete: onDelete task
          , onEdit: onEdit task.id
          , onCommit: onCommit task.id
          }

noopHandler :: EffectFn1 Events.SyntheticEvent Unit
noopHandler = Events.handler_ $ pure unit
