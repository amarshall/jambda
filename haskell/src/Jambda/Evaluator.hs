module Jambda.Evaluator (
  jeval,
) where

import qualified Data.HashMap.Strict as HMap
import Flow
import Jambda.Types

coreAdd :: [JForm] -> JResult
coreAdd [JInteger a, JInteger b] = Right $ JInteger (a + b)
coreAdd _ = Left "TypeError"

newEnv :: JEnv
newEnv =
  HMap.empty
  |> HMap.insert "+" (JFunction coreAdd)

evalAst :: JEnv -> [JForm] -> JResult
evalAst env (JIdentifier ident:args) =
  case HMap.lookup ident env of
    Just (JFunction fn) -> fn args
    Just _ -> Left "Identifier is not a function"
    Nothing -> Left "undefined is not a function"
evalAst _ _ = Left "Literal is not a function"

jeval :: JForm -> JResult
jeval (JList forms) = evalAst newEnv forms
jeval (JIdentifier ident) = case HMap.lookup ident newEnv of
  Just form -> Right form
  Nothing -> Left "undefined"
jeval form = Right form

