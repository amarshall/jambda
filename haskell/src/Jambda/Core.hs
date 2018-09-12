module Jambda.Core (
  initCore
)
where

import Control.Monad.Trans.State.Strict
import Jambda.Env
import Jambda.Types

jAdd :: [JForm] -> JResult
jAdd [JString a, JString b] = Right $ JString (a ++ b)
jAdd [JInteger a, JInteger b] = Right $ JInteger (a + b)
jAdd _ = Left "TypeError"

jLength :: [JForm] -> JResult
jLength [JList xs] = Right $ JInteger $ (length xs)
jLength [JString str] = Right $ JInteger $ (length str)
jLength _ = Left "TypeError"

jStr :: [JForm] -> JResult
jStr (form:[]) = return $ JString (show form)
jStr (_:_) = Left "wrong arity"
jStr [] = return $ JString ""

initCore :: State Env ()
initCore = do
  envSet "+" (JFunction jAdd)
  envSet "length" (JFunction jLength)
  envSet "str" (JFunction jStr)
