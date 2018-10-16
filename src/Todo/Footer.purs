module Todo.Footer where

import Prelude

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect (Effect)
import React.Basic (JSX)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.Events as Events
import Simple.JSON (class ReadForeign, class WriteForeign)
import Foreign.Generic.EnumEncoding (genericDecodeEnum, genericEncodeEnum)
import Data.String (toLower)
import Todo.Task (Task)
import Todo.View (classy)

data Visibility
  = All
  | Completed
  | Active

derive instance eqVisibility :: Eq Visibility
derive instance genericVisibility :: Generic Visibility _
instance showVisibility :: Show Visibility where
  show = genericShow
instance readVisibility :: ReadForeign Visibility where
  readImpl = genericDecodeEnum { constructorTagTransform: toLower }
instance writeVisibility :: WriteForeign Visibility where
  writeImpl = genericEncodeEnum { constructorTagTransform: toLower }


type Props =
  { tasks            :: Array Task
  , onClearCompleted :: Effect Unit
  , visibility       :: Visibility
  }

component :: React.Component Props
component = React.stateless { displayName: "Footer", render }

render :: Props -> JSX
render props =
  let
    tasksCompleted = Array.length (Array.filter _.completed props.tasks)

    tasksLeft = Array.length props.tasks - tasksCompleted

    pluralizedItem = if tasksLeft == 1 then " item" else " items"
  in
    DOM.footer
      { className: "footer"
      , hidden: Array.null props.tasks
      , children:
          [ classy DOM.span "todo-count"
              [ DOM.strong_ [ DOM.text (show tasksLeft) ]
              , DOM.text (pluralizedItem <> " left")
              ]
          , classy DOM.ul "filters"
              [ changeVisibilityLink "#/" All props.visibility
              , DOM.text " "
              , changeVisibilityLink "#/active" Active props.visibility
              , DOM.text " "
              , changeVisibilityLink "#/completed" Completed props.visibility
              ]
          , DOM.button
              { className: "clear-completed"
              , hidden: tasksCompleted == 0
              , onClick: Events.handler_ props.onClearCompleted
              , children: [ DOM.text "Clear completed" ]
              }
          ]
      }

changeVisibilityLink :: String -> Visibility -> Visibility -> JSX
changeVisibilityLink uri visibility actualVisibility =
  DOM.li_
    -- TODO: maybe here we need an onClick?
    [ DOM.a
        { className: if visibility == actualVisibility then "selected" else ""
        , href: uri
        , children: [ DOM.text (show visibility) ]
        }
    ]
