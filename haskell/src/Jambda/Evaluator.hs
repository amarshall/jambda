module Jambda.Evaluator (
  jeval,
) where

import Control.Monad.Trans.State.Strict
import Data.HashMap.Strict as HMap
import Jambda.Env
import Jambda.Types

letBindToMap :: JForm -> Either String Env
letBindToMap (JList ((JIdentifier name):val:xs)) =
  (letBindToMap $ JList xs) >>= (\env -> Right $ HMap.insert name val env)
letBindToMap (JList []) = Right $ HMap.empty
letBindToMap _ = Left "bad bind list"

letBindUnion :: Env -> JForm -> Either String Env
letBindUnion env binds =
  (letBindToMap binds) >>= (\env2 -> Right $ HMap.union env2 env)

jeval :: Env -> JForm -> State Env JResult
jeval localEnv (JList (form1:argForms)) = do
  let jeval2 = jeval localEnv
  case form1 of
    JIdentifier "def" -> do
      case argForms of
        (JIdentifier name):val:_ -> do
          result <- jeval2 val
          case result of
            Left _ -> do
              return result
            Right _ -> do
              envSet name val
              return result
        _ -> return $ Left "first argument of def must be an identifier"
    JIdentifier "let" -> do
      case argForms of
        binds@(JList _):form:[] -> do
          case (letBindUnion localEnv binds) of
            Right env -> jeval env form
            Left err -> return $ Left err
        _ -> return $ Left "bad let (missing binds/form, or too many args)"
    _ -> do
      formInFnPosition <- jeval2 form1
      case formInFnPosition of
        Right (JFunction fn) -> do
          args <- mapM (jeval2) argForms
          return $ (mapM id args) >>= fn
        Right _ -> return $ Left "not a function"
        Left _ -> return formInFnPosition
jeval localEnv (JIdentifier ident) = do
  case HMap.lookup ident localEnv of
    Just f -> return $ Right f
    Nothing -> do
      form <- envGet ident
      return $ case form of
        Just f -> Right f
        Nothing -> Left "undefined"
jeval _ form = return $ Right form
