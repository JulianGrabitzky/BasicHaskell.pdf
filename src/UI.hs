module UI where

import Prog
import Evaluation
import PrettyPrinting
import Data.List
import Parser

type State = (FilePath, Prog, Strategy)

main :: IO()
main = do
        putStrLn "Welcome to Simple Haskell!"
        putStrLn "Type :help for help."
        firstContact ( "", Prog [], loStrategy)

firstContact :: State -> IO()
firstContact state@(filePath, _, _) = do
    putStr $ (getModuleName filePath) ++ "> "
    userIn <- getLine
    case userIn of
        u |        u == ":l"  ||        u == ":load"   -> unloadProgram state
          | take 3 u == ":l " || take 6 u == ":load "  -> loadProgram state (last $ words u)
          | take 2 u == ":r"  || take 7 u == ":reload" -> reloadProgram state
          | take 2 u == ":q"  || take 5 u == ":quit"   -> putStrLn "Bye."
          | take 2 u == ":h"  || take 5 u == ":help"   -> sendHelp state
          | take 3 u == ":s " || take 5 u == ":set "   -> setStrategy state (last $ words u)
          | otherwise -> evaluate state u

getModuleName :: String -> String
getModuleName str = reverse $ drop 3 $ reverse str

-- evaluate a user input
evaluate :: State -> String -> IO()
evaluate state@(_, program, strategy) input = do
        case parse input of
            (Left  errorMsg) -> putStrLn "Invalid input for evaluation" >> firstContact state
            (Right term)     -> (putStrLn $ pretty $ evaluateWith strategy program term) >> firstContact state

-- split a string on a given character
splitOn :: String -> Char -> [String]
splitOn str char = groupBy (\_ b -> b /= char) str

-- unload a program
unloadProgram :: State -> IO()
unloadProgram (filePath, _, strategy) = do
        putStrLn ("Unloading " ++ filePath)
        firstContact ("", Prog [], strategy)

-- load a program from a user input
loadProgram :: State -> String -> IO()
loadProgram state@(_, _, strategy) path = do
        putStrLn $ "Loading " ++ path
        parsedFile <- parseFile path
        case parsedFile of
                (Left errorMsg)    -> putStrLn errorMsg >> firstContact state
                (Right newProgram) -> putStrLn "loaded." >> firstContact (path, newProgram, strategy)

-- reload a program
reloadProgram :: State -> IO()
reloadProgram state@(filePath, _, _) = putStrLn "Reloading program" >> loadProgram state filePath

-- set the evaluation strategy
setStrategy :: State -> String -> IO()
setStrategy (filePath, program, strategy) newStrat =
    case newStrat of
        "lo" -> putStrLn "Set evaluation strategy to leftmost outermost."  >> firstContact (filePath, program, loStrategy)
        "li" -> putStrLn "Set evaluation strategy to leftmost innermost."  >> firstContact (filePath, program, liStrategy)
        "ro" -> putStrLn "Set evaluation strategy to rightmost outermost." >> firstContact (filePath, program, roStrategy)
        "ri" -> putStrLn "Set evaluation strategy to rightmost innermost." >> firstContact (filePath, program, riStrategy)
        "po" -> putStrLn "Set evaluation strategy to parallel outermost."  >> firstContact (filePath, program, poStrategy)
        "pi" -> putStrLn "Set evaluation strategy to parallel innermost."  >> firstContact (filePath, program, piStrategy)
        _    -> putStrLn "Invalid evaluation strategy."                    >> firstContact (filePath, program, strategy)

-- print helping screen
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
