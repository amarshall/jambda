module Main where

import Jambda.Evaluator
import Jambda.Reader
import Jambda.Types
import qualified System.Console.Haskeline as Hline
import System.Posix.IO (stdInput)
import System.Posix.Terminal (queryTerminal)

jprint :: JForm -> Either String String
jprint x = Right $ show x

rep :: String -> Either String String
rep x = Right x >>= jread >>= jeval >>= jprint

repl :: IO ()
repl =
  Hline.runInputT Hline.defaultSettings loop
  where
    loop :: Hline.InputT IO ()
    loop = do
      minput <- Hline.getInputLine "λ "
      case minput of
        Nothing -> return ()
        Just input -> do
          case rep input of
            Left out -> Hline.outputStrLn $ "↯ " ++ out
            Right out -> Hline.outputStrLn $ "∎ " ++ out
          loop

main :: IO ()
main = do
  isTTY <- queryTerminal stdInput
  if isTTY
    then repl
    else do
      input <- getContents
      case rep input of
        Left out -> putStrLn $ "↯ " ++ out
        Right out -> putStrLn out
