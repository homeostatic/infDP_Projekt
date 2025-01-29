module Main (main) where

import Vars
import Base.Type
import Base.IOHelper
import Subst
import SLD
import Pretty




--Datatype Command
data Command = Quit | Help | Load String | SetStrategy Strategy

--Datatype Error (synonm für String)
type ErrorMessage = String

parseCommand :: String -> Either ErrorMessage Command
parseCommand input = case input of
    ':':'h':_                -> Right Help
    ':':'q':_                -> Right Quit
    ':':'l':f             -> case f of
        (' ':fn)  -> Right (Load fn)
        _   -> Left "Missing filename"
    ':':'s':strat        -> case strat of
        -- " dfs" -> Right (SetStrategy dfs)
        -- " bfs" -> Right (SetStrategy bfs)
        _ -> Left "Unknown Strategy"
    _                       -> Left "could not parse command, type :h for help"
     


printAnswers :: [Subst] -> IO () -> IO ()
printAnswers [] action = action
printAnswers (ans:answers) action = do
    putStr (show ans)
    k <- getKeypress 
    if k `elem` [' ', 'n', ';'] --keypresses which ask for next result
        then printAnswers answers action
        else action


welcomeMessage :: String
welcomeMessage = "Welcome!\nType \":h\" for help."

-- Main function
main :: IO ()
main = undefined

