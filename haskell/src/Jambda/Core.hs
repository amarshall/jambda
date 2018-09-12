module Jambda.Core (
  initCore
)
where

import Control.Monad.Trans.State.Strict
import Jambda.Env
import Jambda.Types

jAdd :: [JForm] -> JResult
jAdd forms =
  foldl add (Right $ JInteger 0) forms
  where
    add (Right (JInteger a)) (JInteger b) = Right $ JInteger (a + b)
    add (Right (JFloat a)) (JInteger b) = Right $ JFloat (a + (fromIntegral b))
    add (Right (JFloat a)) (JFloat b) = Right $ JFloat (a + b)
    add (Right (JInteger a)) (JFloat b) = Right $ JFloat ((fromIntegral a) + b)
    add err@(Left _) _ = err
    add _ _ = Left "TypeError"

jConcat :: [JForm] -> JResult
jConcat forms =
  foldl fn (Right $ JString "") forms
  where
    fn (Right (JString a)) (JString b) = Right $ JString (a ++ b)
    fn err@(Left _) _ = err
    fn _ _ = Left "TypeError"

jLength :: [JForm] -> JResult
jLength [JList xs] = Right $ JInteger $ (length xs)
jLength [JString str] = Right $ JInteger $ (length str)
jLength _ = Left "TypeError"

jStr :: [JForm] -> JResult
jStr (form:[]) =
  toStr form >>= (return . JString)
  where
    toStr (JBoolean False) = Right "false"
    toStr (JBoolean True) = Right "true"
    toStr (JFloat x) = Right $ show x
    toStr (JInteger x) = Right $ show x
    toStr (JList x) = Right $ show x
    toStr (JString x) = Right x
    toStr JNothing = Right ""
    toStr _ = Left "TypeError: cannot convert to string"
jStr (_:_) = Left "wrong arity"
jStr [] = return $ JString ""

jFirst :: [JForm] -> JResult
jFirst ((JList (x:_)):[]) = Right x
jFirst ((JList ([])):[]) = Right JNothing
jFirst (_:[]) = Left "TypeError"
jFirst _ = Left "wrong arity"

jRest :: [JForm] -> JResult
jRest ((JList (_:xs)):[]) = Right $ JList xs
jRest ((JList []):[]) = Right $ JList []
jRest (_:[]) = Left "TypeError"
jRest _ = Left "wrong arity"

jEqual :: [JForm] -> JResult
jEqual (a:b:forms) =
  case a == b of
    False -> return $ JBoolean False
    True -> jEqual $ [b] ++ forms
jEqual [_] = return $ JBoolean True
jEqual [] = Left "wrong arity"

jGt :: [JForm] -> JResult
jGt (a:b:[]) = return $ JBoolean $ a > b
jGt _ = Left "wrong arity"

jLt :: [JForm] -> JResult
jLt (a:b:[]) = return $ JBoolean $ a < b
jLt _ = Left "wrong arity"

jGte :: [JForm] -> JResult
jGte (a:b:[]) = return $ JBoolean $ a >= b
jGte _ = Left "wrong arity"

jLte :: [JForm] -> JResult
jLte (a:b:[]) = return $ JBoolean $ a <= b
jLte _ = Left "wrong arity"

initCore :: State Env ()
initCore = do
  envSet "+" (JFunction jAdd)
  envSet "++" (JFunction jConcat)
  envSet "<" (JFunction jLt)
  envSet "<=" (JFunction jLte)
  envSet "=" (JFunction jEqual)
  envSet ">" (JFunction jGt)
  envSet ">=" (JFunction jGte)
  envSet "first" (JFunction jFirst)
  envSet "length" (JFunction jLength)
  envSet "rest" (JFunction jRest)
  envSet "str" (JFunction jStr)
