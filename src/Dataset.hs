module Dataset where

import Control.Monad
import qualified Data.Vector as V
import Lib
import System.Exit
import Text.Megaparsec
import Text.Megaparsec.String

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
    elements <- some elementParser
    eof
    return $ KnapsackProblem numberItems maxWeight $ V.fromList elements

elementParser :: Parser Item
elementParser = do
    weight <- liftM read $ some digitChar
    space
    value <- liftM read $ some digitChar
    eol
    return $ Item weight value
