module Main where

import Jambda.Evaluator
import Jambda.Reader
import Jambda.Types
import qualified System.Console.Haskeline as Hline
import System.Posix.IO (stdInput)
import System.Posix.Terminal (queryTerminal)

jprint :: JForm -> Maybe String
jprint x = Just $ show x

rep :: String -> Maybe String
rep x = Just x >>= jread >>= jeval >>= jprint

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
            Nothing -> Hline.outputStrLn "!!ERROR"
            Just out -> Hline.outputStrLn $ "∎ " ++ out
          loop

main :: IO ()
main = do
  isTTY <- queryTerminal stdInput
  if isTTY
    then repl
    else do
      input <- getContents
      case rep input of
        Nothing -> putStrLn "!!ERROR"
        Just out -> putStrLn out
