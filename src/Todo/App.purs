module Todo.App where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import LocalStorage as LocalStorage
import React.Basic (JSX, capture_, StateUpdate(..), capture, monitor, Self)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events ( key, targetChecked, targetValue)
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

data Action
  = EditNewTodo (Maybe String)
  | SubmitNewTodo String
  | TaskCheck Int
  | TaskUpdate Int String
  | TaskDelete Int
  | ClearCompleted
  | CheckAllTasks (Maybe Boolean)
  | UpdateVisibility Visibility
  | LoadState State
  | Noop

component :: React.Component Props
component = React.createComponent "App"

app :: JSX
app = React.make component
  { initialState
  , didMount
  , render
  , update
  , didUpdate
  } {}
  where
    -- This is the only place we can run stuff only at the first mount
    didMount self@{ state } = do
      let setVisibility visibility =
            React.send self (UpdateVisibility visibility)
      -- On first mount, we start the navigation:
      -- we have something super simple here, in which we match on
      -- the hash string and execute a side effect.
      -- For something fancier we might want to use a parser.
      let matchRoutes hash = case hash of
            "#/"          -> setVisibility All
            "#/active"    -> setVisibility Active
            "#/completed" -> setVisibility Completed
            otherwise     -> pure unit
      runEffectFn1 startNavigation matchRoutes

      -- Then we try to read if we had some state persisted in LocalStorage
      -- If yes, we overwrite the state with it
      persisted <- LocalStorage.getItem localStorageKey
      case persisted of
        Just (oldState :: State) -> React.send self (LoadState oldState)
        _ -> pure unit

    didUpdate self _ = do
      -- Here we persist the state to LocalStorage
      -- called after every update call that resulted in a state change.
      saveState self.state

    update self action =
      case action of
        EditNewTodo val ->
          Update self.state { newTodo = fromMaybe "" val}

        SubmitNewTodo description ->
          let
            newTodo =
              { description: description
              , id: self.state.uid
              , completed: false
              }
          in
           Update self.state
             { newTodo = ""
             , tasks = Array.cons newTodo self.state.tasks
             , uid = self.state.uid + 1
             }

        TaskCheck id ->
          let
            negateCheck task =
              if task.id == id
              then task { completed = not task.completed }
              else task
          in
           Update self.state { tasks = map negateCheck self.state.tasks }

        TaskUpdate id newDescription ->
          let
            updateTask task =
              if task.id == id
              then task { description = newDescription }
              else task
          in
           Update self.state { tasks = map updateTask self.state.tasks }

        TaskDelete id ->
          let
            tasks' = Array.filter ((/=) id <<< _.id) self.state.tasks
          in
           Update self.state { tasks = tasks' }

        ClearCompleted ->
          let
            tasks' = Array.filter (not _.completed) self.state.tasks
          in
           Update self.state { tasks = tasks' }

        CheckAllTasks targetChecked ->
          let
            toggle task = task { completed = fromMaybe task.completed targetChecked }
            tasks' = map toggle self.state.tasks
          in
           Update self.state { tasks = tasks' }

        UpdateVisibility vis ->
          Update self.state { visibility = vis }

        LoadState loadedState ->
          Update loadedState

        Noop ->
          NoUpdate

-- | Pure render function
render :: Self Props State Action -> JSX
render self =
  classy DOM.div "todomvc-wrapper"
    [ classy DOM.section "todoapp"
      [ taskEntry self.state.newTodo onEditNewTodo onKeyDown
      , taskList
          { tasks: self.state.tasks
          , visibility: self.state.visibility
          , onCheck: onTaskCheck
          , onDelete: onTaskDelete
          , onCommit: onTaskUpdate
          , checkAllTasks
          }
      , Footer.footer
            { tasks: self.state.tasks
            , onClearCompleted: clearCompleted
            , visibility: self.state.visibility
            }
      ]
    ]
  where
    -- | Handler for editing the newTodo field
    onEditNewTodo =
      capture self targetValue EditNewTodo

    -- | Handler for submitting a new task after pressing enter
    onKeyDown =
      monitor self key
        \key ->
          case key of
            Just "Enter" | hasNewDescription -> SubmitNewTodo newDescription
            _                                -> Noop
            where
              newDescription = String.trim self.state.newTodo
              hasNewDescription = not (String.null newDescription)


    -- | Action to apply when a task gets checked:
    --   we go through the tasks and mark that one as completed
    onTaskCheck id =
      React.send self (TaskCheck id)

    -- | Action to apply when a task has been edited:
    --   we go through the tasks and edit the description of it with the new value
    onTaskUpdate id newDescription =
      React.send self (TaskUpdate id newDescription)

    -- | Action to apply when deleting a task:
    --   we go through the list and remove the one with the same `id`
    onTaskDelete id =
      React.send self (TaskDelete id)

    -- | Action to remove all completed tasks: filter the list by active ones
    clearCompleted =
      capture_ self ClearCompleted

    -- | Handler to check all tasks that are not completed
    checkAllTasks =
      monitor self targetChecked CheckAllTasks

-- | View for the newTodo input
taskEntry :: String -> Events.EventHandler -> Events.EventHandler -> JSX
taskEntry value onEdit onKeyDown =
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
      , onKeyDown: onKeyDown
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
      Task.component
          { key: task.id
          , task: task
          , onCheck: onCheck task.id
          , onDelete: onDelete task.id
          , onCommit: onCommit task.id
          }
