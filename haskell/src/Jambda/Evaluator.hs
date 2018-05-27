module Jambda.Evaluator (
  jeval,
) where

import Control.Monad.Trans.State.Strict
import Jambda.Env
import Jambda.Types

jeval :: JForm -> State Env JResult
jeval (JList (form1:argForms)) = do
  case form1 of
    JIdentifier "def" -> do
      case argForms of
        (JIdentifier name):val:_ -> do
          result <- jeval val
          case result of
            Left _ -> do
              return result
            Right _ -> do
              envSet name val
              return result
        _ -> return $ Left "first argument of def must be an identifier"
    _ -> do
      formInFnPosition <- jeval form1
      case formInFnPosition of
        Right (JFunction fn) -> do
          args <- mapM (jeval) argForms
          return $ (mapM id args) >>= fn
        Right _ -> return $ Left "not a function"
        Left _ -> return formInFnPosition
jeval (JIdentifier ident) = do
  form <- envGet ident
  return $ case form of
    Just f -> Right f
    Nothing -> Left "undefined"
jeval form = return $ Right form
