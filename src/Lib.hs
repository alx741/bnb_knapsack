module Lib where

import Data.List
import qualified Data.Vector as V

type Room = Float
type Items = V.Vector Item
newtype SortedItems = SortedItems Items deriving (Show)

data KnapsackProblem = KnapsackProblem
    { numberItems :: Int
    , maxWeight :: Room
    , availableItems :: SortedItems
    } deriving (Show)

data Item = Item
    { itemId :: Int
    , itemWeight :: Float
    , itemValue :: Float
    }

instance Eq Item where
    (==) i1 i2 = itemId i1 == itemId i2

instance Ord Item where
    (<=) e1 e2 = (itemValue e1) <= (itemValue e2)

instance Show Item where
    show (Item i w v) =
        "Item: " ++ show i ++ " \n\tWeight: " ++ show w ++ "\n\tValue: " ++ show v ++ "\n"

class Density a where
    density :: a -> Float

instance Density Item where
    density i = itemValue i / itemWeight i

densityCompare :: Item -> Item -> Ordering
densityCompare i1 i2 = compare (density i1) (density i2)

sortByDensity :: Items -> SortedItems
sortByDensity is =
    SortedItems $ V.fromList $ reverse $ sortBy densityCompare $ V.toList is

-- Total Weight = 70
-- Total Value = 73
items :: V.Vector Item
items = V.fromList
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
