module Dataset (readData) where

import Lib
import System.Exit
import Text.Megaparsec
import Text.Megaparsec.String
import Control.Monad
import qualified Data.Vector as V

readData :: FilePath -> IO KnapsackProblem
readData fp = do
    inputData <- readFile fp
    case (parse knapsackParser fp inputData) of
        Left err -> putStr (parseErrorPretty err) >> exitFailure
        Right xs -> return xs

knapsackParser :: Parser KnapsackProblem
knapsackParser = do
    numberItems <- liftM read $ some digitChar
    space
    maxWeight <- liftM read $ some digitChar
    eol
    eol
    items <- some itemParser
    eof
    return $ KnapsackProblem numberItems maxWeight
           $ V.fromList $ identifyItems items

itemParser :: Parser ItemSimple
itemParser = do
    weight <- liftM read $ some digitChar
    space
    value <- liftM read $ some digitChar
    eol
    return $ ItemSimple weight value

data ItemSimple = ItemSimple
    { itemWeight :: Float
    , itemValue :: Float
    } deriving (Show)

identifyItems :: [ItemSimple] -> [Item]
identifyItems is = fmap (\(i, ItemSimple w v) -> Item i w v) $ zip [1..] is
