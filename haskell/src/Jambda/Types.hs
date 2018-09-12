module Jambda.Types
where

import qualified Data.HashMap.Strict as HMap

type Env = HMap.HashMap String JForm
type JResult = Either String JForm

data JForm =
  JBoolean Bool |
  JFloat Double |
  JFunction ([JForm] -> JResult) |
  JLambda (Env -> [JForm] -> JResult) |
  JIdentifier String |
  JInteger Int |
  JList [JForm] |
  JNothing |
  JString String

instance Eq JForm where
  (JBoolean a) == (JBoolean b) = a == b
  (JFloat a) == (JFloat b) = a == b
  (JFloat a) == (JInteger b) = a == (fromIntegral b)
  (JInteger a) == (JFloat b) = (fromIntegral a) == b
  (JInteger a) == (JInteger b) = a == b
  (JList a) == (JList b) = a == b
  (JString a) == (JString b) = a == b
  _ == _ = False

instance Ord JForm where
  _ >= _ = False
  _ <= _ = False
  compare (JBoolean a) (JBoolean b) = compare a b
  compare (JFloat a) (JFloat b) = compare a b
  compare (JFloat a) (JInteger b) = compare a (fromIntegral b)
  compare (JInteger a) (JFloat b) = compare (fromIntegral a) b
  compare (JInteger a) (JInteger b) = compare a b
  compare (JList a) (JList b) = compare a b
  compare (JString a) (JString b) = compare a b
  compare _ _ = EQ

instance Show JForm where
  show (JBoolean False) = "false"
  show (JBoolean True) = "true"
  show (JFloat x) = show x
  show (JFunction _) = "ƒ"
  show (JIdentifier x) = "⸨Identifier " ++ x ++ "⸩"
  show (JInteger x) = show x
  show (JLambda _) = "ƒ"
  show (JList x) = show x
  show (JString x) = "\"" ++ x ++ "\""
  show JNothing = "∅"
