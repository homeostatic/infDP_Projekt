module Main (main) where

import Vars
import Base.Type
import Base.IOHelper
import Subst
import SLD
import Pretty





-- copied over from müsterlosungen

-- Possible commands
data Command = Quit | Help | Load String | SetStrategy Strategy

-- Error messages are strings
type ErrorMessage = String

-- Parse input commands
parseCommand :: String -> Either ErrorMessage Command
parseCommand input = case input of
  'q' : _ -> Right Quit
  'h' : _ -> Right Help
  'l' : s -> case s of
    ' ' : fn -> Right (Load fn)
    _ -> Left "Missing filename"
  's' : s -> case s of
    ' ' : strat -> case strat of
      "dfs" -> Right (SetStrategy dfs)
      "bfs" -> Right (SetStrategy bfs)
      _ -> Left ("Unknown strategy: " ++ strat)
    _ -> Left "Missing strategy"
  _ -> Left ("Unknown command: " ++ input)

-- Print solutions until done or interrupted
printAnswers :: [Subst] -> IO () -> IO ()
printAnswers [] continue = putStrLn "No more solutions." >> continue
printAnswers (s : ss) continue = do
  putStr (show s ++ " ") >> flushBuffer --if we impliment pretty properly show can be swapped out
  c <- getKeypress
  putStrLn ""
  if c `elem` [';', ' ', 'n']
    then printAnswers ss continue
    else continue

welcomeMessage :: String
welcomeMessage = "Welcome!\nType \":h\" for help."

-- Main function
main :: IO ()
main = do
    putStrLn welcomeMessage
    loop

loop ::IO ()
loop = do
    input <- getLine
    case parseCommand input of
        Right Quit -> putStrLn "exiting... \n"
        _ -> main

    -- Eingabe einlesen
    -- Prüfen ob ein Befehl oder eine Anfrage eingegeben wurde
    -- Befehl ausführen oder die Anfrage abarbeiten und die Lösungen anbieten
    -- Falls ein Fehler aufgetreten ist: Fehler ausgeben
    -- Zu 1. springen
