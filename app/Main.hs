module Main where

import Lib
import Data.Time.Clock
import Dataset
import Text.Printf
import BranchAndBound
import Control.Monad
import System.Process

main :: IO ()
main = do
    problem <- readData "test_data/20.knp"

    -- Fully solve
    let candidate = bnb problem

    -- -- Best solve in time threshold
    -- let nullNode = Node Nothing Nothing [] []
    --     linearSolution = solveNode problem nullNode
    -- t <- getCurrentTime
    -- candidate <- solveInTimeThreshold t 120 problem [linearSolution] 0 nullNode


    -- Print full solution
    print $ "# Solution: " ++ show (nodeValue candidate)

    -- Print solution value
    -- print candidate

