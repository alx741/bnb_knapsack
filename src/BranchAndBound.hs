module BranchAndBound where

import Lib
import Data.List
import qualified Data.Vector as V

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
    deriving (Show, Eq)

class SelectionType a where
    isIntegral :: a -> Bool
    isConflictive :: a -> Bool

instance SelectionType Selection where
    isIntegral (Selection _) = True
    isIntegral _ = False
    isConflictive = not . isIntegral

type Items = V.Vector Item
newtype SortedItems = SortedItems Items

type Solution = V.Vector Selection

isSolutionFeasible :: Solution -> Room -> Bool
isSolutionFeasible sol room =
  (solutionWeight sol <= room) && isSolutionIntegral sol

isSolutionIntegral :: Solution -> Bool
isSolutionIntegral = all isIntegral

solutionWeight :: Solution -> Float
solutionWeight = sum . (fmap selectionWeight)

selectionWeight :: Selection -> Float
selectionWeight (Selection i) = value i
selectionWeight (Conflict partial i) = partial * (value i)

valueWeightDensity :: Item -> Float
valueWeightDensity e = value e / weight e

densityCompare :: Item -> Item -> Ordering
densityCompare e1 e2 = compare density1 density2
  where
    density1 = valueWeightDensity e1
    density2 = valueWeightDensity e2

sortByDensity :: Items -> SortedItems
sortByDensity is =
  SortedItems $ V.fromList $ reverse $ sortBy densityCompare $ V.toList is

partialSolution :: SortedItems -> Room -> Solution
partialSolution (SortedItems is) room = undefined

selectItems :: [Item] -> Room -> V.Vector Selection -> Solution
selectItems _ 0 selections = selections
selectItems (i:is) room selections =
    let (roomLeft, selection) = selectItem room i
        selections' = V.snoc selections selection
    in  selectItems is roomLeft selections'

selectItem :: Room -> Item -> (Room, Selection)
selectItem room item = if roomLeft >= 0
    then (roomLeft, Selection item)
    else (0, Conflict (room / weight item) item)
    where roomLeft = room - (weight item)

-- test stuff
-- intSel :: Selection
-- intSel = Selection (testItems V.! 0)

-- confSel :: Selection
-- confSel = Conflict 0.5 (testItems V.! 1)

-- linearSolution :: Solution
-- linearSolution = V.fromList [Selection (testItems V.! 0), Conflict 0.5 (testItems V.! 1),
--     Conflict 0.2 (testItems V.! 2)]

-- integralSolution :: Solution
-- integralSolution = V.fromList [Selection (testItems V.! 0), Selection (testItems V.! 1),
--     Selection (testItems V.! 2)]

-- -- Total Weight = 70
-- -- Total Value = 73
-- testItems :: V.Vector Item
-- testItems = V.fromList
--     [ Item 5 5
--     , Item 4 8
--     , Item 2 1
--     , Item 3 7
--     , Item 4 4
--     , Item 5 9
--     , Item 9 3
--     , Item 8 7
--     , Item 1 3
--     , Item 7 3
--     , Item 1 2
--     , Item 8 4
--     , Item 3 1
--     , Item 4 9
--     , Item 6 7
--     ]
