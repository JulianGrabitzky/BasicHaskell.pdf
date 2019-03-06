module Test where

import System.Exit
import TestPrettyPrinting
import TestSubstitution
import TestEvaluation

-- With help from this site
-- https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html

main :: IO()
main = do
    good <- and <$> sequence
        [testPretty,
         testSubstitution,
         testEvaluation]
    if good then exitSuccess
            else exitFailure
