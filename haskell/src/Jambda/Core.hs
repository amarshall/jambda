module Jambda.Core (
  initCore
)
where

import Control.Monad.Trans.State.Strict
import Jambda.Env
import Jambda.Types

jAdd :: [JForm] -> JResult
jAdd [JInteger a, JInteger b] = Right $ JInteger (a + b)
jAdd _ = Left "TypeError"

jLength :: [JForm] -> JResult
jLength [JList xs] = Right $ JInteger $ (length xs)
jLength [JString str] = Right $ JInteger $ (length str)
jLength _ = Left "TypeError"

initCore :: State Env ()
initCore = do
  envSet "+" (JFunction jAdd)
  envSet "length" (JFunction jLength)
