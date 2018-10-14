-- some trivial exercises juggling two types of data classes
module Data
    (Datum(..),
    Column(..),
    DatasetC(..),
    Row, 
    DatasetR(..),
    (<?>),
    (<!!>),
    (<!>),
    toR,
    toC,
    (<+>),
    (<++>),
    (>->)
    ) where
import qualified Data.List as DL
import Data.Char

data Datum = IVal Int | FVal Double | SVal String | Null
    deriving (Show,Eq)

instance Read Datum where
    readsPrec _ input 
        | input == "NaN" = [(FVal (read "NaN"::Double),"")]
        | input == "" = [(FVal (read "NaN"::Double),"")]
        | foldl (&&) True $ fmap isDigit input = 
            [(IVal (read input::Int),"")]
        | ((head input == '-')||(isDigit $ head input)) && (foldl (&&) True $ fmap (\x->isDigit x || x=='.' ) $ tail input) 
            && ((length $ filter (\x -> x=='.') input) == 1) 
            = [(FVal (read input::Double),"")]
        | otherwise = [(SVal input, "")]

data Column = IVec [Int] | FVec [Double] | SVec [String] | NullCol
    deriving (Show,Eq)

data DatasetC = DatasetC{
    headerC :: [String],
    dC :: [Column]
} deriving (Show, Eq)

type Row = [Datum]

data DatasetR = DatasetR{
    headerR :: [String],
    dR :: [Row]
} deriving (Show,Eq)

addCol :: Column -> Column -> Column
addCol (IVec iv1) (IVec iv2) = IVec (iv1++iv2)
addCol (FVec iv1) (FVec iv2) = FVec (iv1++iv2)
addCol (SVec iv1) (SVec iv2) = SVec (iv1++iv2)
addCol _ _ =  NullCol

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

colToRow :: Column -> Row
colToRow v = case v of 
  IVec iv -> [IVal v | v <- iv]
  FVec fv -> [FVal v | v <- fv]
  SVec sv -> [SVal v | v <- sv]
  _ -> [Null]

getColLen :: [Column] -> Int
getColLen (IVec iv:ivs) = length iv
getColLen (FVec fv:fvs) = length fv
getColLen (SVec sv:svs) = length sv
getColLen _ = 0

cToR :: [Column] -> [Row]
cToR [] = []
cToR cols = fmap (\idx ->  [rs!!idx | rs<- cols_]) idxs where
    idxs = take (getColLen cols) [0..]
    cols_ = fmap colToRow cols


toR :: DatasetC -> DatasetR
toR dc = DatasetR{
    headerR = headerC dc,
    dR = cToR $ dC dc
}

rowToCol :: Row -> [Column]
rowToCol [] = []
rowToCol (IVal i:rs) = (IVec [i]): (rowToCol rs) 
rowToCol (FVal i:rs) = (FVec [i]): (rowToCol rs) 
rowToCol (SVal i:rs) = (SVec [i]): (rowToCol rs) 

rToC :: [Row] -> [Column]
rToC rows_ = let rows = fmap rowToCol rows_ in
    foldl (\x y -> zipWith (addCol) x y) (head rows) (tail rows)

toC:: DatasetR -> DatasetC
toC dr = DatasetC {
    headerC = headerR dr,
    dC = rToC $ dR dr
}

(>->) :: DatasetC -> [String] -> Maybe DatasetR --subSet and convert
dc >-> hs = toR dc <!!> hs

(<-<) ::DatasetR -> [String] -> Maybe DatasetC
dr <-< hs = toC dr <!> hs

(<+>) :: DatasetC -> DatasetC -> DatasetC --Append
dc1 <+> dc2 = DatasetC {
    headerC = (headerC dc1) ++ (headerC dc2),
    dC = (dC dc1) ++ (dC dc2)
}

(<++>) :: DatasetR -> DatasetR -> Maybe DatasetR --Append
dr1 <++> dr2 = if 
    headerR dr1 /= headerR dr2 
        then Nothing
        else Just DatasetR {
        headerR = headerR dr1,
        dR = dR dr1 ++ dR dr2
        }
