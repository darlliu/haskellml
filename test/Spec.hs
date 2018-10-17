import Test.HUnit
import Data 
import Preprocess

v10 = IVec [1,3,4,5,2]
v11 = IVec [1,1,2,2]
v20 = FVec [1.0,2.0,3.0,4.0,5.0]
v21 = FVec [1.0,2.0,0/0,4.0,0/0]
v22 = FVec [1.0,2.0,2.0,4.0,2.0]

testCountNA1 = 
    TestCase $ assertEqual "Counting nan 1"
      [] (getNA v10)
testCountNA2 = 
    TestCase $ assertEqual "Counting nan 2"
      [2,4] (getNA v21)
testMean1 =
    TestCase $ assertEqual "mean int"
      (Just 3.0) (getMean v10) 
testMean2 =
    TestCase $ assertEqual "mean double with na"
      (Just $ (1+2+4)/3) (getMean $ filterNA v21)
testVar1 =
    TestCase $ assertEqual "variance test 1"
      (Nothing) (getVar $ FVec [])
testVar2 = 
    TestCase $ assertEqual "variance test 2"
      (Just 2.0) (getVar v10)
testMedian1 = 
    TestCase $ assertEqual "median test 1"
      (Just 3.0) (getMedian v10)
testMedian2 = 
    TestCase $ assertEqual "median test 2"
      (Just 1.5) (getMedian v11)
testMedian3 = 
    TestCase $ assertEqual "median test 3: nan to median"
      (v22) (nanToMedian v21)

testMmNormalize =
    TestCase $ assertEqual "test min max normalize"
      (FVec [0.0, 0.5, 0.75, 1, 0.25]) (mmNormalize v10)

testStdNormalize =
    TestCase $ assertEqual "test std normalize, testing indirectly for now"
      (stdNormalize $ FVec [-0.5, 0, 0.5]) (stdNormalize $ FVec [-1.0, 0.0, 1.0])

dc1 = DatasetC {
    headerC = ["Int","Float","Obj"],
    dC = [IVec [1,2], FVec [0.1,0.2], SVec ["One","Two"]]
}
dc2 = DatasetC {
    headerC = ["One","Two","Three"],
    dC = [FVec [1.0, 1.1], FVec[ 2.0, 2.1], IVec [0, 1]]
}
dc3 = DatasetC {
    headerC = ["One","Two"],
    dC = [FVec [1.0, 1.1], FVec[ 2.0, 2.1]]
}
dc4 = DatasetC {
    headerC = ["Three"],
    dC = [IVec [0,1]]
}
dr1 = DatasetR {
    headerR = ["One","Two","Three"],
    dR = [[FVal 1.0, FVal 2.0,IVal 0], [FVal 1.1, FVal 2.1, IVal 1]]
}
dr2 = DatasetR {
    headerR = ["One","Two","Three"],
    dR = [[FVal 1.0, FVal 2.0,IVal 0]]
}
dr3 = DatasetR {
    headerR = ["One","Two"],
    dR = [[FVal 1.0, FVal 2.0], [FVal 1.1, FVal 2.1]]
}
dr4 = DatasetR {
    headerR = ["One","Two","Three"],
    dR = [[FVal 1.1, FVal 2.1, IVal 1]]
}

testDatasetCGet1 = 
    TestCase $ assertEqual "Getting a non-empty column"
      (Just (SVec ["One", "Two"])) (dc1 <?> "Obj") 

testDatasetCGet2 = 
    TestCase $ assertEqual "Getting an invalid column"
      Nothing (dc1 <?> "Asd")

testDatasetGetCols1 = 
    TestCase $ assertEqual "Valid col indices"
      (Just dc3) (dc2 <!> ["One","Two"]) 

testDatasetGetCols2 = 
    TestCase $ assertEqual "Invalid col indices"
      Nothing (dc2 <!> ["Asd","Two"]) 

testDatasetGetRows1 = 
    TestCase $ assertEqual "Valid row indices"
      (Just DatasetR{
          headerR = ["One", "Two"],
          dR = [[FVal 1.0, FVal 2.0],[ FVal 1.1, FVal 2.1]]
          }) (dr1 <!!> ["One","Two"]) 

testDatasetGetRows2 = 
    TestCase $ assertEqual "Invalid row indices"
      Nothing (dr1 <!!> ["Asd"]) 

testDatasetCToR = 
    TestCase $ assertEqual "Convert datasetC to datasetR"
      dr1 (toR dc2)

testDatasetRToC = 
    TestCase $ assertEqual "Convert datasetR to datasetC"
      dc2 (toC dr1)

testDatasetCToRSub = 
    TestCase $ assertEqual "Convert datasetC to datasetR and subset"
      (Just dr3) (dc2 >-> ["One","Two"])

testDatasetCAdd =
    TestCase $ assertEqual "Combine datasetC"
      dc2 (dc3 <+> dc4)

testDatasetRAdd =
    TestCase $ assertEqual "Combine datasetR"
      (Just dr1) (dr2 <++> dr4)

testParseInt = 
    TestCase $ assertEqual "parsing int datum"
      (IVal 132) (read "132"::Datum)

testParseFloat = 
    TestCase $ assertEqual "parsing float datum"
      (FVal 132.32) (read "132.32"::Datum)

testParseString = 
    TestCase $ assertEqual "parsing string datum"
      (SVal "192.168.1.1") (read "192.168.1.1"::Datum)

testParseString2 = 
    TestCase $ assertEqual "parsing string datum"
      (SVal "<1H OCEAN") (read "<1H OCEAN"::Datum)

main :: IO Counts
main = runTestTT $ TestList [testDatasetCGet1, testDatasetCGet2, testDatasetGetRows1,
    testDatasetGetRows2, testDatasetCToR, testDatasetRToC, testDatasetCToRSub, testDatasetCAdd,
    testDatasetRAdd, testDatasetGetCols1, testDatasetGetCols2,
    testParseInt, testParseFloat, testParseString, testParseString2,
    testCountNA1,testCountNA2, testMean1, testMean2, testVar1, testVar2,
    testMedian1, testMedian2,testMedian3,
    testMmNormalize, testStdNormalize
    ]
