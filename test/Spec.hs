module Main (main) where

import UncertainGantt (projectName)


main :: IO ()
main = putStrLn ("Tests for " ++ projectName)
