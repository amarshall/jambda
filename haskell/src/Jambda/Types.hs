module Jambda.Types
where

import qualified Data.HashMap.Strict as HMap

type Env = HMap.HashMap String JForm
type JResult = Either String JForm

data JForm =
  JBoolean Bool |
  JFloat Double |
  JFunction ([JForm] -> JResult) |
  JIdentifier String |
  JInteger Int |
  JList [JForm] |
  JNothing |
  JString String

instance Show JForm where
  show (JBoolean False) = "false"
  show (JBoolean True) = "true"
  show (JFloat x) = show x
  show (JFunction _) = "ƒ"
  show (JIdentifier x) = "⸨Identifier " ++ x ++ "⸩"
  show (JInteger x) = show x
  show (JList x) = show x
  show (JString x) = x
  show JNothing = "∅"
