module Preprocess where
import Data
import Data.List as DL
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
getMedian (FVec fv) = do
    let fvv = sort fv
    let len = length fvv
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

oneHotEncode :: Column -> Maybe DatasetC
oneHotEncode col = Nothing

