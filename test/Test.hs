module Test where

import System.Exit
import TestPrettyPrinting

-- With help from this site
-- https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html

main :: IO()
main = do
    good <- and <$> sequence
        [testPretty]
    if good then exitSuccess
            else exitFailure
