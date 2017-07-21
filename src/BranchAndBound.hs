{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module BranchAndBound where

import Lib
import Data.Maybe
import qualified Data.Vector as V
import Debug.Trace

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
type Candidates = [Node]

bnb :: KnapsackProblem -> Solution
bnb p =
    let linearSolution = solveNode p $ Node Nothing Nothing [] []
        (Node _ (Just solution) _ _) = solve p linearSolution []
    in solution

solve :: KnapsackProblem -> Node -> Candidates -> Node
solve p@(KnapsackProblem _ room _) n cs =
    case branch p n of
        Nothing -> maximum cs
        Just (n1, n2) ->
            let feasibles = filter (isFeasible room) [n1, n2]
                cs' = filter isIntegral feasibles
                branchOn = (maximum feasibles)
            in --trace ("solve again with " ++ show (length cs') ++ " candidates on item: " ++ show branchOn)
                solve p (maximum feasibles) (cs ++ cs')

branch :: KnapsackProblem -> Node -> Maybe (Node, Node)
branch problem@(KnapsackProblem _ room (SortedItems items))
    node@(Node _ _ selected rejected) = do
    pivot <- conflictiveItem node
    let n1 = Node Nothing Nothing selected (pivot : rejected)
        n2 = Node Nothing Nothing (pivot : selected) rejected
    return (solveNode problem n1, solveNode problem n2)

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
        shrunkenRoom = room - weight selected'
        roomLeft = if shrunkenRoom < 0 then 0 else shrunkenRoom
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

instance IsLinear Node where
    isIntegral (Node _ (Just sol) _ _) = isIntegral sol
    isIntegral (Node _ Nothing _ _) = False

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
