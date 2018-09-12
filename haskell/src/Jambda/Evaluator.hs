module Jambda.Evaluator (
  jeval,
) where

import Control.Monad.Trans.State.Strict
import Data.HashMap.Strict as HMap
import Jambda.Env
import Jambda.Types

letBindToMap :: JForm -> Either String Env
letBindToMap (JList ((JIdentifier name):val:xs)) =
  (letBindToMap $ JList xs) >>= (return . HMap.insert name val)
letBindToMap (JList []) = Right $ HMap.empty
letBindToMap _ = Left "bad bind list"

letBindUnion :: Env -> JForm -> Either String Env
letBindUnion env binds =
  (letBindToMap binds) >>= (return . flip HMap.union env)

fnBindToMap :: JForm -> JForm -> Either String Env
fnBindToMap (JList ((JIdentifier param):params)) (JList (arg:args)) =
  fnBindToMap (JList params) (JList args) >>= (\env -> Right $ HMap.insert param arg env)
fnBindToMap (JList []) (JList []) = Right $ HMap.empty
fnBindToMap _ _ = Left "mismatched arity"

fnBindUnion :: Env -> JForm -> JForm -> Either String Env
fnBindUnion env params args = (fnBindToMap params args) >>= (return . flip HMap.union env)

jeval :: JForm -> State Env JResult
jeval (JList (form1:argForms)) = do
  env <- get
  case form1 of
    JIdentifier "typeof" -> do
      let form = head argForms
      case form of
        name@(JIdentifier _) -> do
          result <- jeval name
          return $ case result of
            Right f -> Right $ typeof f
            Left _ -> result
        _ -> return $ Right $ typeof form
      where
        typeof (JBoolean _) = JString "Boolean"
        typeof (JFloat _) = JString "Float"
        typeof (JFunction _) = JString "Function"
        typeof (JIdentifier _) = JString "Identifier"
        typeof (JInteger _) = JString "Integer"
        typeof (JList _) = JString "List"
        typeof (JNothing) = JString "Nothing"
        typeof (JString _) = JString "String"
    JIdentifier "def" -> do
      case argForms of
        (JIdentifier name):form:_ -> do
          result <- jeval form
          case result of
            Left _ -> do
              return result
            Right val -> do
              envSet name val
              return result
        _ -> return $ Left "first argument of def must be an identifier"
    JIdentifier "let" -> do
      case argForms of
        binds@(JList _):form:[] -> do
          case (letBindUnion env binds) of
            Right env2 -> return $ evalState (jeval form) env2
            Left err -> return $ Left err
        _ -> return $ Left "bad let (missing binds/form, or too many args)"
    JIdentifier "fn" -> do
      case argForms of
        params@(JList _):form:[] ->
          return $ Right $ JFunction (\args ->
            case (fnBindUnion env params (JList args)) of
              Right env2 -> evalState (jeval form) env2
              Left err -> Left err
          )
        _ -> return $ Left "bad fn (missing params/form, or too many args)"
    _ -> do
      formInFnPosition <- jeval form1
      case formInFnPosition of
        Right (JFunction fn) -> do
          args <- mapM (jeval) argForms
          return $ (mapM id args) >>= fn
        Right _ -> return $ Left "not a function"
        Left _ -> return formInFnPosition
jeval (JIdentifier ident) = do
  env <- get
  case HMap.lookup ident env of
    Just f -> return $ Right f
    Nothing -> do
      form <- envGet ident
      return $ case form of
        Just f -> Right f
        Nothing -> Left $ ident ++ " is undefined"
jeval form = return $ Right form
