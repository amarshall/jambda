module Jambda.Evaluator (
  jeval,
  jevalTop,
) where

import Control.Monad.Trans.State.Strict
import Data.HashMap.Strict as HMap
import Data.Monoid
import Jambda.Env
import Jambda.Types

letBindToMap :: JForm -> Either String Env
letBindToMap (JList ((JIdentifier name):val:xs)) =
  letBindToMap (JList xs) >>= return . HMap.insert name val
letBindToMap (JList []) = return $ HMap.empty
letBindToMap (JList (_:_:_)) = Left "name in bind list must be an identifier"
letBindToMap (JList (_:[])) = Left "uneven bind list"
letBindToMap _ = Left "bad bind list"

letBindUnion :: Env -> JForm -> Either String Env
letBindUnion env binds = letBindToMap binds >>= return . flip HMap.union env

fnBindToMap :: JForm -> JForm -> Either String Env
fnBindToMap (JList ((JIdentifier param):params)) (JList (arg:args)) =
  fnBindToMap (JList params) (JList args) >>= return . HMap.insert param arg
fnBindToMap (JList []) (JList []) = return $ HMap.empty
fnBindToMap (JList params) (JList args) =
  Left $ "mismatched arity (given " ++ (show $ length args) ++ ", expected " ++ (show $ length params)
fnBindToMap _ _ = Left "bug!"

fnBindUnion :: Env -> JForm -> JForm -> Either String Env
fnBindUnion env params args = fnBindToMap params args >>= return . flip HMap.union env

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
    JIdentifier "typeof" ->
      case head argForms of
        name@(JIdentifier _) -> jevalHere name >>= return . typeof
        form -> Right $ typeof form
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
    JIdentifier "quote" ->
      case argForms of
        form:[] -> return form
        _ -> Left "bad quote (more than one arg)"
    JIdentifier "let" ->
      case argForms of
        binds@(JList _):form:[] -> do
          letBindUnion localEnv binds >>= \newLocalEnv -> jevalGlobal newLocalEnv form
        _ -> Left "bad let (missing binds/form, or too many args)"
    JIdentifier "fn" ->
      case argForms of
        params@(JList _):form:[] ->
          return $ JLambda $ \callingGlobalEnv args -> do
            fnBindUnion localEnv params (JList args) >>= \newLocalEnv -> jeval callingGlobalEnv newLocalEnv form
        _ -> Left "bad fn (missing params/form, or too many args)"
    JIdentifier "if" ->
      case argForms of
        condition:whenTrue:whenFalse:[] ->
          case jevalHere condition of
            Right (JBoolean False) -> jevalHere whenFalse
            Right JNothing -> jevalHere whenFalse
            Right _ -> jevalHere whenTrue
            err -> err
        _ -> Left "bad if (incorrect number of forms)"
    _ -> do
      let formInFnPosition = jevalHere form1
      case formInFnPosition of
        Right (JFunction fn) -> mapM jevalHere argForms >>= fn
        Right (JLambda fn) -> mapM jevalHere argForms >>= fn globalEnv
        Right _ -> Left "not a function"
        Left _ -> formInFnPosition
jeval globalEnv localEnv (JIdentifier ident) =
  case getFirst $ (First $ lookupIdent localEnv) <> (First $ lookupIdent globalEnv) of
    Just f -> return f
    Nothing -> Left $ ident ++ " is undefined"
  where lookupIdent = HMap.lookup ident
jeval _ _ form = Right form
