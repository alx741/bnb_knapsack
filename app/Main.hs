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
    -- print $ bnb problem
    let linSol = solveNode problem $ Node Nothing Nothing [] []
    t <- getCurrentTime
    (sol, candidates) <- solveInTimeThreshold t 60 problem linSol []
    print $ "best candidate" ++ show (maximum candidates)

    -- t <- getCurrentTime
    -- _ <- readProcess "sleep" ["0.5"] ""
    -- t' <- getCurrentTime
    -- let diff = diffUTCTime t' t
    -- print $ "time: " ++ show diff
    -- if diff < 1 then print "menos de 1s" else print "mas de 1s"
    -- return ()

    -- let linSol = solveNode problem $ Node Nothing Nothing [] []
    -- print $ solve problem linSol []
