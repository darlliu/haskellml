-- some trivial exercises juggling two types of data classes
module Data where
import Prelude hiding (foldl, (++), zipWith, head, tail, take, length, foldr, filter,map)
import qualified Prelude as P (foldl, (++), zipWith, head, tail, take, length, foldr, filter,map)
import Data.Char
import Data.Vector

data Datum = IVal Int | FVal Double | SVal String | Null
    deriving (Show,Eq)

instance Read Datum where
    readsPrec _ input 
        | input == "NaN" = [(FVal (read "NaN"::Double),"")]
        | input == "" = [(FVal (read "NaN"::Double),"")]
        | P.foldl (&&) True $ fmap isDigit input = 
            [(IVal (read input::Int),"")]
        | ((P.head input == '-')||(isDigit $ P.head input)) && (P.foldl (&&) True $ fmap (\x->isDigit x || x=='.' ) $ P.tail input) 
            && ((P.length $ P.filter (\x -> x=='.') input) == 1) 
            = [(FVal (read input::Double),"")]
        | otherwise = [(SVal input, "")]

data Column = IVec (Vector Int) | FVec (Vector Double) | SVec (Vector String) | NullCol
    deriving (Show,Eq)

data DatasetC = DatasetC{
    headerC :: Vector String,
    dC :: Vector Column
} deriving (Show, Eq)

type Row = Vector Datum

data DatasetR = DatasetR{
    headerR :: Vector String,
    dR :: Vector Row
} deriving (Show,Eq)

addCol :: Column -> Column -> Column
addCol (IVec iv1) (IVec iv2) = IVec (iv1++iv2)
addCol (FVec iv1) (FVec iv2) = FVec (iv1++iv2)
addCol (SVec iv1) (SVec iv2) = SVec (iv1++iv2)
addCol _ _ =  NullCol

(<?>) :: DatasetC -> String -> Maybe Column  --getCol
dd <?> h = 
        let hh = headerC dd 
        in do 
        idx <- elemIndex h hh
        return ((dC dd) ! idx)

getIdxs :: Vector String -> Vector String -> Maybe (Vector Int)
getIdxs hh hs  = let idxs_ = map (\h -> elemIndex h hh) hs 
    in foldr (\x y -> do
        x_ <- x
        y_ <- y
        Just(x_ `cons` y_)) 
        (Just empty) idxs_
    --   case x of 
    --     Nothing -> Nothing
    --     Just x_ -> case y of 
    --         Nothing -> Nothing
    --         Just y_ -> Just(x_++[y_])) (Just []) idxs_

(<!>) :: DatasetC -> [String] -> Maybe DatasetC --subSet Columns
dc <!> hs_ = do
    let hs = fromList hs_
    idxs <- getIdxs (headerC dc) hs
    let cs = dC dc
    let cc = map (\x -> cs ! x) idxs
    return DatasetC {
        headerC = hs,
        dC = cc
    }

(<!!>) :: DatasetR -> [String] -> Maybe DatasetR --subSet Rows
dr <!!> hs_ = do
    let hs = fromList hs_
    idxs <- getIdxs (headerR dr) hs
    let rs = dR dr
    let rr = fmap (\r -> 
             fmap (\x ->r ! x) idxs) rs
    return DatasetR {
        headerR = hs,
        dR = rr
    }

colToRow :: Column -> Row
colToRow v = case v of 
  IVec iv -> map (\v -> IVal v) iv
  FVec fv -> map (\v -> FVal v) fv
  SVec sv -> map (\v -> SVal v) sv
  _ -> singleton Null

getColLen :: Vector Column -> Int
getColLen cols
  | length cols == 0 = 0
  | otherwise = let col = cols ! 0
    in case col of
      (IVec iv) -> length iv
      (FVec fv) -> length fv
      (SVec sv) -> length sv
      NullCol -> 0  

cToR :: Vector Column -> Vector Row
cToR cols = if length cols == 0 then empty else
    map (\idx ->  map (\rs -> rs ! idx ) cols_ ) idxs where
      idxs = enumFromN 0 (getColLen cols)
      cols_ = map colToRow cols


toR :: DatasetC -> DatasetR
toR dc = DatasetR{
    headerR = headerC dc,
    dR = cToR $ dC dc
}

rowToCol :: Row -> Vector Column
rowToCol row
  | length row == 0 = empty
  | otherwise = let i_ = head row in
    case i_ of
        IVal i -> (IVec $ singleton i) `cons` rowToCol (tail row)
        FVal i -> (FVec $ singleton i) `cons` rowToCol (tail row)
        SVal i -> (SVec $ singleton i) `cons` rowToCol (tail row)
        Null -> (NullCol) `cons` rowToCol (tail row) 

rToC :: Vector Row -> Vector Column
rToC rows_ = let rows = map rowToCol rows_ in
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
(<+++>) :: DatasetR -> DatasetR -> DatasetR --Append
dr1 <+++> dr2 =  DatasetR {
        headerR = (headerR dr1) ++ (headerR dr2),
        dR = zipWith (++) (dR dr1) (dR dr2)
        }
