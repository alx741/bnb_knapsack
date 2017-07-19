module Lib where

import qualified Data.Vector as V

type Room = Int

data KnapsackProblem = KnapsackProblem
    { numberItems :: Int
    , maxWeight :: Room
    , elements :: V.Vector Item
    } deriving (Show)

data Item = Item
    { weight :: Float
    , value :: Float
    } deriving (Eq)

instance Ord Item where
    (<=) e1 e2 = (value e1) <= (value e2)

instance Show Item where
    show (Item w v) =
        "Item: \n\tWeight: " ++ show w ++ "\n\tValue: " ++ show v ++ "\n"

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
