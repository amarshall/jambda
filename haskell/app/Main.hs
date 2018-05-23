module Main where

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

rep :: String -> Either String String
rep x = Right x >>= jread >>= jeval >>= jprint

repl :: IO ()
repl =
  Hline.runInputT Hline.defaultSettings loop
  where
    loop :: Hline.InputT IO ()
    loop = do
      minput <- Hline.getInputLine (colorizeStr TermT.Blue "λ ")
      case minput of
        Nothing -> return ()
        Just input -> do
          case rep input of
            Left out -> Hline.outputStrLn $ (colorizeStr TermT.Red "↯ ") ++ out
            Right out -> Hline.outputStrLn $ (colorizeStr TermT.Green "∎ ") ++ out
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
