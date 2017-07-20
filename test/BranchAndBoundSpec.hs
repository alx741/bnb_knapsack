module BranchAndBoundSpec where

import BranchAndBound
import Lib
import Test.Hspec
import Data.List
import qualified Data.Vector as V

spec :: Spec
spec = do
    describe "isFeasible" $ do
        it "tells if a solution is feasible" $ do
            isFeasible 1000 integralSolution `shouldBe` True
            isFeasible 1 integralSolution `shouldBe` False
            isFeasible 1000 linearSolution `shouldBe` True
            isFeasible 1 linearSolution `shouldBe` False

    describe "partialSolution" $ do
        it "gives a solution for a given set of items and available room" $ do
            let items = SortedItems testItems
                solution = V.fromList
                    [ IntegralSel (testItems V.! 0)
                    , IntegralSel (testItems V.! 1)
                    , FractionalSel 0.5 (testItems V.! 2)
                    ]
            partialSolution items 10  `shouldBe` solution

    describe "solveNode" $ do
        it "solves a single node" $ do
            let selected = [testItems V.! 0, testItems V.! 2]
                rejected = [testItems V.! 1, testItems V.! 3, testItems V.! 5,
                    testItems V.! 7, testItems V.! 9, testItems V.! 11,
                    testItems V.! 13, testItems V.! 14]
                node = Node Nothing Nothing selected rejected
                solution = fmap IntegralSel $ V.fromList
                        [testItems V.! 0, testItems V.! 2, testItems V.! 8,
                        testItems V.! 10, testItems V.! 4, testItems V.! 12,
                        testItems V.! 6]
                solvedNode = Node (Just 19.0) (Just solution) selected rejected
            solveNode testProblem node  `shouldBe` solvedNode


intSel :: Selection
intSel = IntegralSel (testItems V.! 0)

confSel :: Selection
confSel = FractionalSel 0.5 (testItems V.! 1)

linearSolution :: Solution
linearSolution = V.fromList [IntegralSel (testItems V.! 0), FractionalSel 0.5 (testItems V.! 1),
    FractionalSel 0.2 (testItems V.! 2)]

integralSolution :: Solution
integralSolution = V.fromList [IntegralSel (testItems V.! 0), IntegralSel (testItems V.! 1),
    IntegralSel (testItems V.! 2)]

testProblem :: KnapsackProblem
testProblem = KnapsackProblem (V.length testItems) 50 $ sortByDensity testItems

-- Total Weight = 70
-- Total Value = 73
testItems :: V.Vector Item
testItems = V.fromList
    [ Item 1 5 5
    , Item 2 4 8
    , Item 3 2 1
    , Item 4 3 7
    , Item 5 4 4
    , Item 6 5 9
    , Item 7 9 3
    , Item 8 8 7
    , Item 9 1 3
    , Item 10 7 3
    , Item 11 1 2
    , Item 12 8 4
    , Item 13 3 1
    , Item 14 4 9
    , Item 15 6 7
    ]
