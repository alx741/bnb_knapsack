{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module BranchAndBound where

import Lib
import Data.Time.Clock
import Data.Maybe
import Data.Foldable
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
type LB = Int

bnb :: KnapsackProblem -> Node
bnb p =
    let nullNode = Node Nothing Nothing [] []
        linearSolution = solveNode p nullNode
    in solve p [linearSolution] 0 nullNode

solveInTimeThreshold :: UTCTime
  -> NominalDiffTime
  -> KnapsackProblem
  -> [Node]
  -> LB
  -> Node
  -> IO Node
solveInTimeThreshold _ _ _ [] _ candidate = return candidate
solveInTimeThreshold init seconds p@(KnapsackProblem _ room _) (n:ns) lb candidate = do
    t <- getCurrentTime
    if diffUTCTime t init < seconds
    then
        case branch p n of
            Nothing -> solveInTimeThreshold init seconds p ns lb candidate
            Just (n1, n2) ->
                let prunees = foldr' (prune p lb) [] [n1, n2]
                    candidates = filter
                        (\n -> isIntegral n && isFeasible room n)
                        (candidate : prunees)
                    candidate' = if length candidates /= 0
                        then best candidates
                        else candidate
                    lb' = fromMaybe lb $ round <$> nodeValue candidate'
                in trace ("Candidate: " ++ show (nodeValue candidate')) solveInTimeThreshold init seconds p (ns ++ prunees) lb' candidate'
    else return candidate

solve :: KnapsackProblem -> [Node] -> LB -> Node -> Node
solve _ [] _ candidate = candidate
solve p@(KnapsackProblem _ room _) (n:ns) lb candidate =
    case branch p n of
        Nothing -> solve p ns lb candidate
        Just (n1, n2) ->
            let prunees = foldr' (prune p lb) [] [n1, n2]
                candidates = filter
                    (\n -> isIntegral n && isFeasible room n)
                    (candidate : prunees)
                candidate' = if length candidates /= 0
                    then best candidates
                    else candidate
                lb' = fromMaybe lb $ round <$> nodeValue candidate'
            in trace ("Candidate: " ++ show (nodeValue candidate')) solve p (ns ++ prunees) lb' candidate'

-- foldr (prune p lb) n [ns]
prune :: KnapsackProblem -> LB -> Node -> [Node] -> [Node]
prune p lb n ns = if not (shouldPrune p n lb) then n : ns else ns

shouldPrune :: KnapsackProblem -> Node -> LB -> Bool
shouldPrune p n lb =
    case nodeValue n of
        Nothing -> False
        Just v -> not (isIntegral n) && v <= fromIntegral lb

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

class Best a where
    best :: [a] -> a

instance Best Node where
    best = maximum
