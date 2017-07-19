module BranchAndBound where

import Data.List
import qualified Data.Vector as V
import Lib

data Node = Node
    { level :: Int
    , selectedItems :: V.Vector Item
    , totalValue :: Float
    , totalWeight :: Float
    , nodeBound :: Float
    } deriving (Show)

data Selection
    = Selection Item
    | Conflict Float Item
    deriving (Show)

class SelectionType a where
    isIntegral :: a -> Bool
    isConflictive :: a -> Bool

instance SelectionType Selection where
    isIntegral (Selection _) = True
    isIntegral _ = False
    isConflictive s = not $ isIntegral s

type Solution = [Selection]
type Space = V.Vector Item

isSolutionFeasible :: Solution -> Room -> Bool
isSolutionFeasible sol room =
  (solutionWeight sol <= room) && isSolutionIntegral sol

isSolutionIntegral :: Solution -> Bool
isSolutionIntegral = all isIntegral

selectionWeight :: Selection -> Float
selectionWeight (Selection i) = value i
selectionWeight (Conflict partial i) = partial * (value i)

solutionWeight :: Solution -> Float
solutionWeight = sum . (fmap selectionWeight)

valueWeightDensity :: Item -> Float
valueWeightDensity e = value e / weight e

densityCompare :: Item -> Item -> Ordering
densityCompare e1 e2 = compare density1 density2
  where
    density1 = valueWeightDensity e1
    density2 = valueWeightDensity e2

sortByDensity :: Space -> Space
sortByDensity es = V.fromList $ reverse $ sortBy densityCompare $ V.toList es

partialSolution :: Space -> Room -> Solution
partialSolution = undefined



-- testing stuff
s1 :: Selection
s1 = Selection (testItems V.! 0)

s2 :: Selection
s2 = Conflict 0.5 (testItems V.! 1)

someSolution :: Solution
someSolution = [Selection (testItems V.! 0), Conflict 0.5 (testItems V.! 1),
    Conflict 0.2 (testItems V.! 2)]

someIntSolution :: Solution
someIntSolution = [Selection (testItems V.! 0), Selection (testItems V.! 1),
    Selection (testItems V.! 2)]
-- / testing stuff

