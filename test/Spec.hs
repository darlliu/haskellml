import Test.HUnit
import Data 

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

main :: IO Counts
main = runTestTT $ TestList [testDatasetCGet1, testDatasetCGet2, testDatasetGetRows1,
    testDatasetGetRows2, testDatasetCToR, testDatasetRToC, testDatasetCToRSub, testDatasetCAdd,
    testDatasetRAdd, testDatasetGetCols1, testDatasetGetCols2
    ]
