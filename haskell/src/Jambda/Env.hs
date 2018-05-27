module Jambda.Env
where

import Control.Monad.Trans.State.Strict
import qualified Data.HashMap.Strict as HMap
import Jambda.Types

newEnv :: Env
newEnv = HMap.empty

envGet :: String -> State Env (Maybe JForm)
envGet ident = gets $ HMap.lookup ident

envSet :: String -> JForm -> State Env ()
envSet ident form = modify $ HMap.insert ident form
