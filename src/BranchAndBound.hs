{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module BranchAndBound where

import Lib
import Data.Maybe
import qualified Data.Vector as V

data Node = Node
    { nodeValue :: Maybe Float
    , nodeSolution :: Maybe Solution
    , nodeSelectedItems :: [Item]
    , nodeRejectedItems :: [Item]
    } deriving (Show, Eq)

instance Ord Node where
    compare (Node Nothing _ _ _) (Node Nothing _ _ _) = EQ
    compare (Node Nothing _ _ _) n2 = LT
    compare n1 (Node Nothing _ _ _) = GT
    compare (Node (Just a) _ _ _) (Node (Just b) _ _ _) = compare a b

data Selection
    = IntegralSel Item
    | FractionalSel Float Item
    deriving (Show, Eq)

getItem :: Selection -> Item
getItem (IntegralSel item) = item
getItem (FractionalSel _ item) = item

type Solution = V.Vector Selection

branch :: KnapsackProblem -> Node -> Maybe Node
branch problem@(KnapsackProblem _ room (SortedItems items))
    node@(Node _ _ selected rejected) =
    case conflictiveItem node of
        Nothing -> Nothing
        Just p ->
            let n1 = solveNode problem $ Node Nothing Nothing selected [p]
                n2 = solveNode problem $ Node Nothing Nothing (p : selected) []
            in if isFeasible room n1 && isFeasible room n2
                then return $ max n1 n2
                else listToMaybe $ filter (isFeasible room) [n1, n2]

someNode1 = Node (Just 1.0) (Just linearSolution) [] []
someNode2 = Node (Nothing) (Just linearSolution) [] []

conflictiveItem :: Node -> Maybe Item
conflictiveItem n = nodeSolution n
    >>= (\n -> return $ V.filter isFractional n)
    >>= (flip (V.!?)) 0
    >>= return . getItem

solveNode :: KnapsackProblem -> Node -> Node
solveNode (KnapsackProblem _ room (SortedItems items)) (Node _ _ selected rejected) =
    let items' = SortedItems $ ((drop selected') . (drop rejected')) items
        solution = fmap IntegralSel selected' V.++ partialSolution items' roomLeft
    in  Node (Just $ value solution) (Just solution) selected rejected
    where
        selected' = V.fromList selected
        rejected' = V.fromList rejected
        roomLeft  = room - weight selected'
        drop disposables = V.filter (not . (flip V.elem) disposables)

partialSolution :: SortedItems -> Room -> Solution
partialSolution (SortedItems is) room =
    selectItems (V.toList is) room (V.fromList [])
    where
        selectItems :: [Item] -> Room -> V.Vector Selection -> Solution
        selectItems _ 0 selections = selections
        selectItems [] _ selections = selections
        selectItems (i:is) room selections =
            let (roomLeft, selection) = selectItem room i
                selections' = V.snoc selections selection
            in  selectItems is roomLeft selections'

        selectItem :: Room -> Item -> (Room, Selection)
        selectItem room item = if roomLeft >= 0
            then (roomLeft, IntegralSel item)
            else (0, FractionalSel (room / itemWeight item) item)
            where roomLeft = room - (itemWeight item)

class IsFeasible a where
    isFeasible :: Room -> a -> Bool

instance IsFeasible Solution where
    isFeasible room sol = (weight sol <= room)

instance IsFeasible a => IsFeasible (Maybe a) where
    isFeasible room (Just a) = isFeasible room a
    isFeasible _ Nothing = False

instance IsFeasible Node where
    isFeasible room node = isFeasible room (nodeSolution node)

class IsLinear a where
    isIntegral :: a -> Bool
    isFractional :: a -> Bool
    isFractional = not . isIntegral

instance IsLinear Selection where
    isIntegral (IntegralSel _) = True
    isIntegral _ = False

instance IsLinear Solution where
    isIntegral = all isIntegral

class Weight a where
    weight :: a -> Float

instance Weight Selection where
    weight (IntegralSel i) = itemWeight i
    weight (FractionalSel partial i) = partial * (itemWeight i)

instance Weight Solution where
    weight = sum . (fmap weight)

instance Weight Items where
    weight = sum . (fmap itemWeight)

class Value a where
    value :: a -> Float

instance Value Selection where
    value (IntegralSel i) = itemValue i
    value (FractionalSel partial i) = partial * (itemValue i)

instance Value Solution where
    value = sum . (fmap value)

instance Value Items where
    value = sum . (fmap itemValue)


-- test stuff
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

-- -- Total Weight = 70
-- -- Total Value = 73
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
