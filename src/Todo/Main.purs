module Todo.Main where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import LocalStorage as LocalStorage
import React.Basic (JSX)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (key, preventDefault, targetChecked, targetValue)
import React.Basic.Events as Events
import Todo.Footer (Visibility(..))
import Todo.Footer as Footer
import Todo.Task (Task)
import Todo.Task as Task
import Todo.View (classy)


-- | Hook to set the navigation function
foreign import startNavigation :: EffectFn1 (String -> Effect Unit) Unit

-- | The main component doesn't have any props since no one is passing them to us
type Props = {}

-- | State type: we keep the list of tasks, the state of the field,
--   the current task visibility filter and the next id to assign.
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

-- | The localStorage key under which we'll save the state of the app
localStorageKey :: String
localStorageKey = "todomvc-purescript-state"

-- | Action to persist the state to LocalStorage
saveState :: State -> Effect Unit
saveState state = LocalStorage.setItem localStorageKey state

app :: React.Component Props
app = React.component
  { displayName: "App"
  , initialState
  -- Here we inject a modified `setState` that persists the state to LocalStorage
  -- after every modification
  , receiveProps: \a -> receiveProps $ a { setState = \f -> a.setStateThen f saveState }
  , render:       \a -> render $ a { setState = \f -> a.setStateThen f saveState }
  }
  where
    -- This is the only place we can run stuff only at the first mount
    receiveProps { state, setState, isFirstMount } = when isFirstMount do
      -- On first mount, we start the navigation:
      -- we have something super simple here, in which we match on
      -- the hash string and execute a side effect.
      -- For something fancier we might want to use a parser.
      let matchRoutes hash = case hash of
            "#/"          -> setState _ { visibility = All }
            "#/active"    -> setState _ { visibility = Active }
            "#/completed" -> setState _ { visibility = Completed }
            otherwise     -> pure unit
      runEffectFn1 startNavigation matchRoutes

      -- Then we try to read if we had some state persisted in LocalStorage
      -- If yes, we overwrite the state with it
      persisted <- LocalStorage.getItem localStorageKey
      setState \_ -> case persisted of
        Just (oldState :: State) -> oldState
        _                        -> state

-- | Pure render function
render :: forall r. { state :: State, setState :: SetState | r } -> JSX
render { state, setState } =
  classy DOM.div "todomvc-wrapper"
    [ classy DOM.section "todoapp"
      [ taskEntry state.newTodo onEditNewTodo onSubmitNewTodo
      , taskList
          { tasks: state.tasks
          , visibility: state.visibility
          , onCheck: onTaskCheck
          , onDelete: onTaskDelete
          , onCommit: onTaskUpdate
          , checkAllTasks
          }
      , React.element
          Footer.component
            { tasks: state.tasks
            , onClearCompleted: clearCompleted
            , visibility: state.visibility
            }
      ]
    ]
  where
    -- | Handler for editing the newTodo field
    onEditNewTodo =
      Events.handler
        (preventDefault >>> Events.merge { targetValue })
        \{ targetValue } -> setState _ { newTodo = fromMaybe "" targetValue }

    -- | Handler for submitting a new task after pressing enter
    onSubmitNewTodo =
      Events.handler
        -- Events.merge lets us run two events in parallel
        -- (in this case they are both pure)
        -- and returns a record with their values
        (Events.merge { targetValue, key })
        \{ targetValue, key } -> case key of
          Just "Enter" | not (String.null newDescription) -> do
            _ <- pure preventDefault
            setState _ { newTodo = ""
                       , tasks = Array.cons newTodo state.tasks
                       , uid = state.uid + 1
                       }
          _ -> pure unit
            where
              newDescription = String.trim state.newTodo

              newTodo =
                { description: newDescription
                , id: state.uid
                , completed: false
                }

    -- | Action to apply when a task gets checked:
    --   we go through the tasks and mark that one as completed
    onTaskCheck id =
      setState _ { tasks = map negateCheck state.tasks }
      where
        negateCheck task =
          if task.id == id then task { completed = not task.completed } else task

    -- | Action to apply when a task has been edited:
    --   we go through the tasks and edit the description of it with the new value
    onTaskUpdate id newDescription =
      setState _ { tasks = map updateTask state.tasks }
      where
        updateTask task =
          if task.id == id
          then task { description = newDescription }
          else task

    -- | Action to apply when deleting a task:
    --   we go through the list and remove the one with the same `id`
    onTaskDelete id =
      setState _ { tasks = Array.filter ((/=) id <<< _.id) state.tasks }

    -- | Action to remove all completed tasks: filter the list by active ones
    clearCompleted = setState _ { tasks = Array.filter (not _.completed) state.tasks }

    -- | Handler to check all tasks that are not completed
    checkAllTasks =
      Events.handler
        targetChecked
        \targetChecked -> do
          let toggle task = task { completed = fromMaybe task.completed targetChecked }
          setState _ { tasks = (map toggle state.tasks) }


-- | View for the newTodo input
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

type TaskListProps =
  { tasks         :: Array Task
  , visibility    :: Visibility
  , onCheck       :: Int -> Effect Unit
  , onDelete      :: Int -> Effect Unit
  , onCommit      :: Int -> String -> Effect Unit
  , checkAllTasks :: Events.EventHandler
  }

-- | View for the list of tasks
taskList :: TaskListProps -> JSX
taskList { tasks, visibility, onCheck, onDelete, onCommit, checkAllTasks } =
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

    -- | Wrapper around creating a new Task component for every task
    taskView task =
      React.element
        Task.component
          { key: task.id
          , task: task
          , onCheck: onCheck task.id
          , onDelete: onDelete task.id
          , onCommit: onCommit task.id
          }
