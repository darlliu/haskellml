module Data
    (Column(..),
    DatasetC(..),
    Row, 
    DatasetR(..),
    (!?),
    (!),
    someFunc
    ) where

import qualified Data.List as DL

data Column = ICol [Int] | FCol [Float] | SCol [String] 
    deriving (Show)
data DatasetC = DatasetC{
    headerC :: [String],
    dC :: [Column]
} deriving (Show)

type Row = [Float]
data DatasetR = DatasetR{
    headerR :: [String],
    dR :: [Row]
} deriving (Show)

(!) :: [String] -> String -> Maybe Int
hh ! h = h `DL.elemIndex` hh

(!?) :: DatasetC -> String -> Maybe Column
dd !? h = 
        let hh = headerC dd 
        in do 
        idx <- hh ! h
        return ((dC dd) !! idx)



someFunc :: IO()
someFunc = putStr ("Here is something" ++ (foldl (\x y -> x++y) "" ["1","2","3"]))