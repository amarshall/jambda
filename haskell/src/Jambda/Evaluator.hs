module Jambda.Evaluator (
  jeval,
) where

import qualified Data.HashMap.Strict as HMap
import Flow
import Jambda.Types

coreAdd :: [JForm] -> JForm
coreAdd [JInteger a, JInteger b] = JInteger (a + b)
coreAdd _ = JNothing

newEnv :: JEnv
newEnv =
  HMap.empty
  |> HMap.insert "+" (JFunction coreAdd)

evalAst :: JEnv -> [JForm] -> Maybe JForm
evalAst env (JIdentifier ident:args) =
  case HMap.lookup ident env of
    Just (JFunction fn) -> Just $ fn args
    Just _ -> Nothing
    Nothing -> Nothing
evalAst _ _ = Nothing

jeval :: JForm -> Maybe JForm
jeval (JList forms) = evalAst newEnv forms
jeval form = Just form

