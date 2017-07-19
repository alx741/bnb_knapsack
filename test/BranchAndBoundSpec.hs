module BranchAndBoundSpec where

import BranchAndBound
import Lib
import Test.Hspec
import Data.List
import qualified Data.Vector as V

spec :: Spec
spec = do
    describe "isSolutionFeasible" $ do
        it "tells if a solution is feasible" $ do
            isSolutionFeasible integralSolution 1000  `shouldBe` True
            isSolutionFeasible integralSolution 1 `shouldBe` False
            isSolutionFeasible linearSolution 1000 `shouldBe` False

    describe "partialSolution" $ do
        it "gives a solution for a given set of items and available room" $ do
            let items = SortedItems testItems
            let solution = V.fromList
                    [ Selection (testItems V.! 0)
                    , Selection (testItems V.! 1)
                    , Conflict 0.5 (testItems V.! 2)
                    ]
            partialSolution items 10  `shouldBe` solution



intSel :: Selection
intSel = Selection (testItems V.! 0)

confSel :: Selection
confSel = Conflict 0.5 (testItems V.! 1)

linearSolution :: Solution
linearSolution = V.fromList [Selection (testItems V.! 0), Conflict 0.5 (testItems V.! 1),
    Conflict 0.2 (testItems V.! 2)]

integralSolution :: Solution
integralSolution = V.fromList [Selection (testItems V.! 0), Selection (testItems V.! 1),
    Selection (testItems V.! 2)]

-- Total Weight = 70
-- Total Value = 73
testItems :: V.Vector Item
testItems = V.fromList
    [ Item 5 5
    , Item 4 8
    , Item 2 1
    , Item 3 7
    , Item 4 4
    , Item 5 9
    , Item 9 3
    , Item 8 7
    , Item 1 3
    , Item 7 3
    , Item 1 2
    , Item 8 4
    , Item 3 1
    , Item 4 9
    , Item 6 7
    ]
