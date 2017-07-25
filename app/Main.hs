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
    problem <- readData "test_data/4096.knp"

    -- -- Fully solve
    -- print $ bnb problem

    -- Best solve in time threshold
    let linSol = solveNode problem $ Node Nothing Nothing [] []
    t <- getCurrentTime
    (sol, candidates) <- solveInTimeThreshold t 60 problem linSol []
    print $ "best candidate" ++ show (maximum candidates)
