module UI where

import Prog
import Evaluation
import PrettyPrinting

type State = (FilePath, Prog, Strategy)

main :: IO()
main = do
        putStrLn "Welcome to Simple Haskell!"
        putStrLn "Type :help for help."
        firstContact ( "> "  ,Prog [] ,loStrategy)

firstContact :: State -> IO()
firstContact state@(filePath, program, strategy) = do
    putStr filePath
    userIn <- getLine
    case userIn of
      | userIn == ":l" || userIn == ":load" -> --unload und was ist wenn der User leerzeichen dahinter einfügt
      | take 3 userIn == ":l " || take 6 userIn == ":load " -> -- load
      | take 2 userIn == ":r" || take 7 userIn == ":reload" -> --reload
      | take 2 userIn == ":q" || take 5 userIn == ":quit" -> -- bye bye
      | take 2 userIn == ":h" || take 5 userIn == ":help" -> sendHelp state
      | take 3 userIn == ":s " || take 5 userIn == ":set " -> -- set strat
      | otherwise -> do
        evaluationResult <- -- Es fehlt noch die Termevaluation und darin das Errorhandeling
        putStr evaluationResult
        firstContact state


sendHelp :: State -> IO()
sendHelp state = do
    putStrLn "Commands available from the prompt:"
    putStrLn "  <expression>       Evaluates the specified expression."
    putStrLn "  :h[elp]            Shows this help message."
    putStrLn "  :l[oad] <file>     Loads the specified file."
    putStrLn "  :l[oad]            Unloads the currently loaded file."
    putStrLn "  :r[eload]          Reloads the lastly loaded file."
    putStrLn "  :s[et] <strategy>  Sets the specified evaluation strategy"
    putStrLn "                     where <strategy> is one of 'lo', 'li',"
    putStrLn "                     'ro', 'ri', 'po', or 'pi'."
    putStrLn "  :q[uit]            Exits the interactive environment."
    firstContact state
