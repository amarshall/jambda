module Main where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Morph
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
import Jambda.Core
import Jambda.Env
import Jambda.Evaluator
import Jambda.Reader
import Jambda.Types
import qualified System.Console.Haskeline as Hline
import qualified System.Console.ANSI.Codes as Term
import qualified System.Console.ANSI.Types as TermT
import System.Posix.IO (stdInput)
import System.Posix.Terminal (queryTerminal)

jprint :: JForm -> Either String String
jprint x = Right $ show x

colorizeStr :: TermT.Color -> String -> String
colorizeStr color str =
  Term.setSGRCode [TermT.SetColor TermT.Foreground TermT.Dull color]
    ++ str
    ++ Term.setSGRCode [Term.Reset]

stateify :: (r1 -> Either l r2) -> Either l r1 -> State Env (Either l r2)
stateify fn x = state $ \env -> (x >>= fn, env)

restateify :: (r1 -> State Env (Either l r2)) -> Either l r1 -> State Env (Either l r2)
restateify fn (Right x) = fn x
restateify _ (Left x) = state $ \env -> (Left x, env)

rep :: String -> State Env (Either String String)
rep x =
  (stateify jread $ Right x) >>= (restateify jeval) >>= (stateify jprint)

baseEnv :: Env
baseEnv = execState initCore newEnv

replOut :: MonadIO m => Either String String -> Hline.InputT m ()
replOut (Left out) = Hline.outputStrLn $ (colorizeStr TermT.Red "↯ ") ++ out
replOut (Right out) = Hline.outputStrLn $ (colorizeStr TermT.Green "∎ ") ++ out

hLineSettings :: Hline.Settings (StateT Env IO)
hLineSettings = Hline.defaultSettings {Hline.historyFile = Just ".jambda-history"}

repl :: IO ()
repl = do
  evalStateT (Hline.runInputT hLineSettings loop) baseEnv
  where
    loop :: Hline.InputT (StateT Env IO) ()
    loop = do
      minput <- Hline.getInputLine (colorizeStr TermT.Blue "λ ")
      case minput of
        Nothing -> return ()
        Just input -> do
          resultStr <- lift $ hoist generalize $ rep input
          replOut resultStr
          loop

once :: IO ()
once = do
  input <- getContents
  case evalState (rep input) baseEnv of
    Left out -> putStrLn $ "↯ " ++ out
    Right out -> putStrLn out

main :: IO ()
main = do
  isTTY <- queryTerminal stdInput
  if isTTY
  then repl
  else once
