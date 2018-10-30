module DecisionTree where
import Data

data DTree = DTree { 
    left :: DTree,
    right :: DTree,
    prediction :: Int,
    feature :: Int,
    test :: (Int, Double)
}

maxSplit :: DatasetC -> (Int , Double) 
maxSplit dc = (0, 1.0)

splitData :: DatasetR -> (Int, Double) -> (DatasetR, DatasetR)
splitData dr (fidx, thrs) = (dr, dr)

cost :: DatasetC -> Int -> Datum -> Double
cost dr fidx thrs = 0.0

terminate :: DTree -> Int -> DatasetC -> Bool
terminate node depth dr = False

trainDT :: DTree -> DatasetR -> Int -> DTree
trainDT node dr depth =
    let dc = toC dr in
    if terminate node depth dc then
        node
    else
        let (dls, drs) = splitData dr (test node) (feature node) in
