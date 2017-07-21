module Main where

import Lib
import Dataset
import BranchAndBound

main :: IO ()
main = do
    problem <- readData "test_data/8.knp"
    print $ bnb problem

    -- let linSol = solveNode problem $ Node Nothing Nothing [] []
    -- print $ solve problem linSol []
