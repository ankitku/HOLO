module Test where

import Ptron
import Winnow
import Utils
import Data.Ord
import Data.List
import System.IO
import Control.Monad
import Control.Monad.State
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import Data.Char (toLower,isLetter)
import Data.List.Split

readDouble :: String -> Double
readDouble = read

csvToVector str = let xs = map readDouble $ splitOn "," str
                       in (vector $ tail $ init xs,floor $ last xs)

startWeightTest :: Vector Double
startWeightTest = vector $ replicate 9 0

main :: IO ()
main = do
        file <- readFile "breast-cancer-wisconsin.data.txt"
        let dataset = map csvToVector (filter (\str -> not $ "?" `isInfixOf` str) $ lines file)
        --evalStateT (Ptron.trainOnSet dataset) (startWeightTest,0)
        evalStateT (Winnow.trainOnSet dataset 1.1) (initV d 1.0,1.0,0)
