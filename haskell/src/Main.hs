module Main where

import Flow
import qualified System.Console.Haskeline as Hline

jread :: String -> String
jread x = x

jeval :: String -> String
jeval x = x

jprint :: String -> String
jprint x = x

rep :: String -> String
rep x = x |> jread |> jeval |> jprint

repl :: IO ()
repl =
  Hline.runInputT Hline.defaultSettings loop
  where
    loop :: Hline.InputT IO ()
    loop = do
      minput <- Hline.getInputLine "λ "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> do
          input |> (++) "∎ " |> Hline.outputStrLn
          loop

main :: IO ()
main = do
  repl
