{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

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

type Items = V.Vector Item
newtype SortedItems = SortedItems Items

type Solution = V.Vector Selection

class IsLinear a where
    isIntegral :: a -> Bool
    isConflictive :: a -> Bool
    isConflictive = not . isIntegral

instance IsLinear Selection where
    isIntegral (Selection _) = True
    isIntegral _ = False

instance IsLinear Solution where
    isIntegral = all isIntegral

class Weight a where
    weight :: a -> Float

instance Weight Selection where
    weight (Selection i) = itemWeight i
    weight (Conflict partial i) = partial * (itemWeight i)

instance Weight Solution where
    weight = sum . (fmap weight)

class Density a where
    density :: a -> Float

instance Density Item where
    density i = itemValue i / itemWeight i

isSolutionFeasible :: Solution -> Room -> Bool
isSolutionFeasible sol room = (weight sol <= room) && isIntegral sol

densityCompare :: Item -> Item -> Ordering
densityCompare i1 i2 = compare (density i1) (density i2)

sortByDensity :: Items -> SortedItems
sortByDensity is =
    SortedItems $ V.fromList $ reverse $ sortBy densityCompare $ V.toList is

partialSolution :: SortedItems -> Room -> Solution
partialSolution (SortedItems is) room =
    selectItems (V.toList is) room (V.fromList [])
    where
        selectItems :: [Item] -> Room -> V.Vector Selection -> Solution
        selectItems _ 0 selections = selections
        selectItems (i:is) room selections =
            let (roomLeft, selection) = selectItem room i
                selections' = V.snoc selections selection
            in  selectItems is roomLeft selections'

        selectItem :: Room -> Item -> (Room, Selection)
        selectItem room item = if roomLeft >= 0
            then (roomLeft, Selection item)
            else (0, Conflict (room / itemWeight item) item)
            where roomLeft = room - (itemWeight item)

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
