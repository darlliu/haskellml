module Preprocess where
import Data
import qualified Data.List as DL
import Prelude hiding (foldl, (++), zipWith, head, tail, take, length, foldr, filter,map, minimum, maximum)
import qualified Prelude as P (foldl, (++), zipWith, head, tail, take, length, foldr, filter,map, minimum, maximum)
import Data.Vector
getNA :: Column -> Vector Int -- get NA indexes
getNA (IVec iv) = filter (\x-> x/= -1) $ zipWith (\x y -> if isNaN $ fromIntegral x then y else -1) iv (enumFromN 0 (length iv))
getNA (FVec fv) = filter (\x-> x/= -1) $ zipWith (\x y -> if isNaN x then y else -1) fv (enumFromN 0 (length fv))
getNA _ = empty

filterNA :: Column -> Column
filterNA (IVec iv) = IVec $ filter (\x-> not $ isNaN $ fromIntegral x) iv
filterNA (FVec iv) = FVec $ filter (\x-> not $ isNaN x) iv
filterNA col = col

getMean :: Column -> Maybe Double
getMean (FVec fv) = let len = length fv in 
    if len == 0 then Nothing
    else Just $ sum/(fromIntegral len::Double)
    where sum = foldl (+) 0 fv
getMean (IVec iv) = getMean (FVec $ map (\x -> fromIntegral x::Double) iv)
getMean col = Nothing

getVar :: Column -> Maybe Double
getVar (FVec fv) = do 
    mu <- getMean (FVec fv)
    let len = fromIntegral (length fv)::Double
    if len == 0 
        then Nothing
        else 
            let vals = fmap (\x -> x*x/len) fv in
            let sval = foldl (+) 0.0 vals in
            return (sval - (mu*mu))
getVar (IVec iv) = getVar (FVec $ map (\x -> fromIntegral x::Double) iv)
getVar col = Nothing

getMedian :: Column -> Maybe Double
getMedian (FVec fv) = do
    let fvv = DL.sort $ toList fv
    let len = length fv
    if len > 1 && mod len 2 == 0 then
        return $ (fvv !! (quot len 2) + fvv !! (quot len 2 - 1))/2
    else
        return $ fvv !! (quot len 2)
getMedian (IVec iv) = getMedian (FVec $ map (\x -> fromIntegral x::Double) iv)
getMedian col = Nothing

nanToMedian :: Column -> Column
nanToMedian (FVec fv) =
    let med = getMedian $ filterNA (FVec fv) in
    case med of
    Nothing -> FVec fv 
    Just med_ -> FVec (fmap (\x -> if isNaN x then med_ else x) fv)
nanToMedian (IVec iv) = nanToMedian (FVec $ map (\x -> fromIntegral x::Double) iv)
nanToMedian col = col

mmNormalize :: Column -> Column --normalize by max and min val, no NaNs
mmNormalize (FVec fv) = 
    let min = minimum fv in
    let max = maximum fv in
    if min >= max then (FVec fv)
    else 
        FVec $ fmap(\x-> (x-min)/(max-min)) fv 
mmNormalize (IVec iv) = mmNormalize (FVec $ map (\x -> fromIntegral x::Double) iv)
mmNormalize col = col

stdNormalizeInner :: Column -> Maybe Double -> Maybe Double -> Column
stdNormalizeInner (FVec fv) (Just mu_) (Just var_) =
    if var_ == 0 then FVec fv
    else
        FVec $ fmap (\x -> (x-mu_)/(sqrt var_)) fv
stdNormalizeInner col _ _ = col

stdNormalize :: Column -> Column --normalize to standard gaussian, no NaNs
stdNormalize (FVec fv) = 
    let mu = getMean $ FVec fv in
    let var = getVar $ FVec fv in
    stdNormalizeInner (FVec fv) mu var
stdNormalize (IVec iv) = stdNormalize (FVec $ map (\x -> fromIntegral x::Double) iv)
stdNormalize col = col

encodeInt :: (Eq a) => a-> [a] -> Row
encodeInt i is = fromList [ if i==ii then (IVal 1) else (IVal 0) | ii <- is]

oneHotEncode :: Column -> String -> Maybe DatasetR
oneHotEncode (IVec iv) hh = do
    let ivv = DL.sort $ toList $ uniq iv
    let kvv = fmap (\x -> hh P.++ "-" P.++ (show x)) ivv
    return $ DatasetR{
        headerR = fromList kvv,
        dR = map (\x-> encodeInt x ivv) iv
    }
oneHotEncode (FVec fv) hh = oneHotEncode (IVec $ fmap (\x -> round x) fv) hh  
oneHotEncode (SVec sv) hh = do
    let svv = DL.sort $ toList $ uniq sv
    return $ DatasetR {
        headerR = fromList [ hh P.++ "-" P.++ x | x<- svv],
        dR = map (\x -> encodeInt x svv) sv
    }  
oneHotEncode _ _ = Nothing

