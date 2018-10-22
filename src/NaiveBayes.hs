module NaiveBayes where
-- normal dist on all features
-- binary class for labels priors 0.5
import Data
import Prelude hiding (foldl, (++), zipWith, head, tail, take, length, foldr, filter, notElem, map, splitAt, init)
import qualified Prelude as P (foldl, (++), zipWith, head, tail, take, length, foldr, filter, notElem, map, splitAt, init )
import Preprocess
import Data.Vector

data NBModel = NBModel {
    weights0 :: Vector (Maybe Double, Maybe Double),
    weights1 :: Vector (Maybe Double, Maybe Double),
    pr0 :: Double,
    pr1 :: Double
}

trainNaiveBayes :: DatasetC -> Vector Int -> IO NBModel
trainNaiveBayes dc labels = do
    let dcc = dC dc 
    let dr = toR dc  
    let idxs0 = Data.Vector.filter (\x-> labels ! x == 0) $ enumFromN 0 $ Data.Vector.length (dR dr) 
    let idxs1 = Data.Vector.filter (\x-> labels ! x == 1) $ enumFromN 0 $ Data.Vector.length (dR dr) 
    let dc0 = toC $ dropRows dr (toList idxs1) 
    let dc1 = toC $ dropRows dr (toList idxs0) 
    let means0 = Data.Vector.map getMean (dC dc0) 
    let vars0 = Data.Vector.map getVar (dC dc0) 
    let means1 = Data.Vector.map getMean (dC dc1) 
    let vars1 = Data.Vector.map getVar (dC dc1) 
    putStrLn "mean/var 0"
    putStrLn $ show means0
    putStrLn $ show means1
    putStrLn "mean/var 1"
    putStrLn $ show vars0
    putStrLn $ show vars1
    return NBModel {
        weights0 = Data.Vector.zipWith (\x y -> (x,y)) means0 vars0,
        weights1 = Data.Vector.zipWith (\x y -> (x,y)) means1 vars1,
        pr0 = (fromIntegral $ Data.Vector.length idxs0) / (fromIntegral $ Data.Vector.length labels),
        pr1 = (fromIntegral $ Data.Vector.length idxs1) / (fromIntegral $ Data.Vector.length labels)
    }

evalNaiveBayes_ :: Datum -> (Maybe Double, Maybe Double) -> Double
evalNaiveBayes_ (FVal v) (Just mu, Just var) = exp ((-(v-mu)^2)/var)
evalNaiveBayes_ (IVal v) (Just mu, Just var) = exp ((-((fromIntegral v)-mu)^2)/var)
evalNaiveBayes_ _ _ = 0.0

evalNaiveBayes :: DatasetC -> NBModel -> Vector Int
evalNaiveBayes dc model =
    let dr = cToR $ dC dc in 
    let w0 = weights0 model in
    let w1 = weights1 model in 
    let y0 = map (\row-> zipWith evalNaiveBayes_ row w0) dr in
    let y1 = map (\row-> zipWith evalNaiveBayes_ row w1) dr in
    let yy0 = map (\row -> foldl (*) (pr0 model) row) y0 in
    let yy1 = map (\row -> foldl (*) (pr1 model) row) y1 in
    zipWith (\x y -> if x>y then 0 else 1) yy0 yy1
    