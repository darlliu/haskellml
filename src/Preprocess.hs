module Preprocess where
import Data
import Math.Statistics

getNA :: Column -> [Int] -- get NA indexes
getNA (IVec iv) = filter (\x-> x/= -1) $ zipWith (\x y -> if isNaN $ fromIntegral x then y else -1) iv (take  (length iv) [0..])
getNA (FVec fv) = filter (\x-> x/= -1) $ zipWith (\x y -> if isNaN x then y else -1) fv (take  (length fv) [0..])
getNA col = []

getMean :: Column -> Maybe Double
getMean col = Nothing

getVar :: Column -> Maybe Double
getVar col = Nothing

getMedian :: Column -> Maybe Double
getMedian col = Nothing

mmNormalize :: Column -> Column --normalize by max and min val
mmNormalize (IVec iv) = IVec iv
mmNormalize (FVec fv) = FVec fv
mmNormalize col = col

stdNormalize :: Column -> Column --normalize to standard gaussian
stdNormalize col = col

oneHotEncode :: Column -> Maybe DatasetC
oneHotEncode col = Nothing

