module LocalStorage where

import Prelude

import Data.Either (hush)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Simple.JSON (class ReadForeign, class WriteForeign)
import Simple.JSON as JSON


foreign import setItem_ :: EffectFn2 String String Unit
foreign import getItem_ :: EffectFn1 String (Nullable String)
foreign import removeItem_ :: EffectFn1 String Unit

setItem :: forall a. WriteForeign a => String -> a -> Effect Unit
setItem key val = runEffectFn2 setItem_ key (JSON.writeJSON val)

getItem :: forall a. ReadForeign a => String -> Effect (Maybe a)
getItem key = do
  itemString <- runEffectFn1 getItem_ key
  pure $ (hush <<< JSON.readJSON) =<< toMaybe itemString

removeItem :: String -> Effect Unit
removeItem = runEffectFn1 removeItem_
