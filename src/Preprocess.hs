module Preprocess where
import Data

getNA :: Column -> [Int] -- get NA indexes
getNA (IVec iv) = filter (\x-> x/= -1) $ zipWith (\x y -> if isNaN $ fromIntegral x then y else -1) iv (take  (length iv) [0..])
getNA (FVec fv) = filter (\x-> x/= -1) $ zipWith (\x y -> if isNaN x then y else -1) fv (take  (length fv) [0..])
getNA col = []

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
getMedian col = Nothing

mmNormalize :: Column -> Column --normalize by max and min val, no NaNs
mmNormalize (IVec iv) = IVec iv
mmNormalize (FVec fv) = FVec fv
mmNormalize col = col

stdNormalize :: Column -> Column --normalize to standard gaussian, no NaNs
stdNormalize col = col

oneHotEncode :: Column -> Maybe DatasetC
oneHotEncode col = Nothing

