module Jambda.Evaluator (
  jeval,
  jevalTop,
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

jevalTop :: JForm -> State Env JResult
jevalTop form@(JList (form1:argForms)) = do
  case form1 of
    JIdentifier "def" -> do
      case argForms of
        (JIdentifier name):valForm:[] -> do
          env <- get
          result <- return $ jeval env newEnv valForm
          case result of
            Left _ -> return result
            Right val -> do
              envSet name val
              return result
        _:_:[] -> return $ Left "first argument of def must be an identifier"
        _ -> return $ Left "bad def (missing name/form, or too many args)"
    _ -> do
      env <- get
      return $ jeval env newEnv form
jevalTop form = do
  env <- get
  return $ jeval env newEnv form

jeval :: Env -> Env -> JForm -> JResult
jeval globalEnv localEnv (JList (form1:argForms)) = do
  let jevalGlobal = jeval globalEnv
  let jevalHere = jeval globalEnv localEnv

  case form1 of
    JIdentifier "typeof" -> do
      let form = head argForms
      case form of
        name@(JIdentifier _) -> do
          let result = jevalHere name
          case result of
            Right f -> Right $ typeof f
            Left _ -> result
        _ -> Right $ typeof form
      where
        typeof (JBoolean _) = JString "Boolean"
        typeof (JFloat _) = JString "Float"
        typeof (JFunction _) = JString "Function"
        typeof (JIdentifier _) = JString "Identifier"
        typeof (JInteger _) = JString "Integer"
        typeof (JLambda _) = JString "Function"
        typeof (JList _) = JString "List"
        typeof (JNothing) = JString "Nothing"
        typeof (JString _) = JString "String"
    JIdentifier "def" -> Left "def is only available at the top-level"
    JIdentifier "quote" -> do
      case argForms of
        form:[] -> Right form
        _ -> Left "bad quote (more than one arg)"
    JIdentifier "let" -> do
      case argForms of
        binds@(JList _):form:[] -> do
          case (letBindUnion localEnv binds) of
            Right newLocalEnv -> jevalGlobal newLocalEnv form
            Left err -> Left err
        _ -> Left "bad let (missing binds/form, or too many args)"
    JIdentifier "fn" -> do
      case argForms of
        params@(JList _):form:[] ->
          Right $ JLambda $ \callingGlobalEnv args -> do
            case (fnBindUnion localEnv params (JList args)) of
              Right newLocalEnv -> jeval callingGlobalEnv newLocalEnv form
              Left err -> Left err
        _ -> Left "bad fn (missing params/form, or too many args)"
    JIdentifier "if" -> do
      case argForms of
        condition:whenTrue:whenFalse:[] ->
          case jevalHere condition of
            Right (JBoolean False) -> jevalHere whenFalse
            Right _ -> jevalHere whenTrue
            err@(Left _) -> err
        _ -> Left "bad if (incorrect number of forms)"
    _ -> do
      let formInFnPosition = jevalHere form1
      case formInFnPosition of
        Right (JFunction fn) -> mapM jevalHere argForms >>= fn
        Right (JLambda fn) -> mapM jevalHere argForms >>= fn globalEnv
        Right _ -> Left "not a function"
        Left _ -> formInFnPosition
jeval globalEnv localEnv (JIdentifier ident) =
  case HMap.lookup ident localEnv of
    Just f -> Right f
    Nothing -> case HMap.lookup ident globalEnv of
      Just f -> Right f
      Nothing -> Left $ ident ++ " is undefined"
jeval _ _ form = Right form
