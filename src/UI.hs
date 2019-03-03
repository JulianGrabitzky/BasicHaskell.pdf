module UI where

import Prog
import Evaluation

data Status = Status Maybe(String, Prog), Strategy

main :: IO()
main = do
        putStr "Welcome to Simple Haskell!"
        putStr "Type :help for help."
        firstContact (Status Nothing loStrategy)

firstContact :: Status -> IO()
firstContact (Status program _ ) = do
    case program of
        Nothing -> putChar '>'

helpMe :: Nothing

loadFile :: Nothing

unloadFile :: Nothing

setStrategy :: Nothing
