module Main (main) where

import Vars
import Base.Type
import Base.IOHelper
import Base.Parser
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

helpMessage :: String
helpMessage = "Commands available from the prompt:\n  <goal>      Solves/proves the specified goal.\n  :h          Shows this help message.\n  :l <file>   Loads the specified file.\n  :q          Exits the interactive environment.\n  :s <strat>  Sets the specified search strategy\n              where <strat> is one of 'dfs' or 'bfs'."


-- Main function
main :: IO ()
main = do
    putStrLn welcomeMessage
    loop

loop ::IO ()
loop = do
    -- Eingabe einlesen
    input <- getLine   
    case input of
      --command handling, commands begin with ':'
      ':':rest -> case parseCommand rest of
        Left err -> do
          putStrLn err
          loop
        Right Help -> do
          putStrLn helpMessage
          loop
        Right (Load fn) -> do 
          putStrLn ("load file"++fn) --placeholder, TODO: load file logic
          loop
        Right Quit -> putStrLn "exiting... \n"
      --Anfrage handling
      anf -> case parse anf of
        Right (Prog p) -> do
          putStrLn ("parsed program:"++ show p)
          loop     --TODO test anfrage logic
        Left err -> do
          putStrLn err
          loop
    
    
    -- Befehl ausführen oder die Anfrage abarbeiten und die Lösungen anbieten
    -- Falls ein Fehler aufgetreten ist: Fehler ausgeben
    -- Zu 1. springen
