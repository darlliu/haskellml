module Data
    (Column(..),
    DatasetC(..),
    Row, 
    DatasetR(..),
    (<?>),
    (<!!>),
    (<!>)
    ) where

import qualified Data.List as DL

data Column = ICol [Int] | FCol [Float] | SCol [String] 
    deriving (Show,Eq)

data DatasetC = DatasetC{
    headerC :: [String],
    dC :: [Column]
} deriving (Show, Eq)

type Row = [Float]
data DatasetR = DatasetR{
    headerR :: [String],
    dR :: [Row]
} deriving (Show,Eq)

(!) :: [String] -> String -> Maybe Int
hh ! h = h `DL.elemIndex` hh

(<?>) :: DatasetC -> String -> Maybe Column  --getCol
dd <?> h = 
        let hh = headerC dd 
        in do 
        idx <- hh ! h
        return ((dC dd) !! idx)

getIdxs :: [String] -> [String] -> Maybe [Int]
getIdxs hh hs  = let idxs_ = [hh ! x |x<-hs] 
    in foldr (\x y -> do
        x_ <- x
        y_ <- y
        Just(x_ : y_)) 
        (Just []) idxs_
    --   case x of 
    --     Nothing -> Nothing
    --     Just x_ -> case y of 
    --         Nothing -> Nothing
    --         Just y_ -> Just(x_++[y_])) (Just []) idxs_

(<!>) :: DatasetC -> [String] -> Maybe DatasetC --subSet Columns
dc <!> hs = do
    idxs <- getIdxs (headerC dc) hs
    let cs = dC dc
    let cc = fmap (\x -> cs!!x) idxs
    return DatasetC {
        headerC = hs,
        dC = cc
    }

(<!!>) :: DatasetR -> [String] -> Maybe DatasetR --subSet Rows
dr <!!> hs = do
    idxs <- getIdxs (headerR dr) hs
    let rs = dR dr
    let rr = fmap (\r -> 
             fmap (\x ->r!!x) idxs) rs
    return DatasetR {
        headerR = hs,
        dR = rr
    }

-- cToR :: [Col] :: [Row]
-- cToR [] = []
-- cToR col:cols = 


-- (toR):: DatasetC :: DatasetR
-- toR dc = DatasetR{
--     headerR = headerC dc,
--     dR = 
-- }

-- (toC):: DatasetC :: DatasetR

--(>>) :: DatasetC -> [String] -> DatasetR --subSet and convert

(<+>) :: DatasetC -> DatasetC -> DatasetC --Append
dc1 <+> dc2 = DatasetC {
    headerC = (headerC dc1) ++ (headerC dc2),
    dC = (dC dc1) ++ (dC dc2)
}

(<++>) :: DatasetR -> DatasetR -> DatasetR --Append
dr1 <++> dr2 = DatasetR {
    headerR = (headerR dr1) ++ (headerR dr2),
    dR = zipWith (++) (dR dr1) (dR dr2)
}