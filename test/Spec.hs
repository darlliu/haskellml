import Test.HUnit
import Data 

ds = DatasetC {
    headerC = ["Int","Float","Obj"],
    dC = [ICol [1,2], FCol [0.1,0.2], SCol ["One","Two"]]
}
ds2 = DatasetR {
    headerR = ["One","Two","Three"],
    dR = [[1.0, 2.0, 0.0], [1.1, 2.1, 0.1]]
}

testDatasetCGet1 = 
    TestCase $ assertEqual "Getting a non-empty column"
      (Just (SCol ["One", "Two"])) (ds <?> "Obj") 

testDatasetCGet2 = 
    TestCase $ assertEqual "Getting an invalid column"
      Nothing (ds <?> "Asd")

testDatasetGetIdxs1 = 
    TestCase $ assertEqual "Valid row indices"
      (Just DatasetR{
          headerR = ["One", "Two"],
          dR = [[1.0, 2.0],[1.1, 2.1]]
          }) (ds2 <!!> ["One","Two"]) 

testDatasetGetIdxs2 = 
    TestCase $ assertEqual "Invalid row indices"
      Nothing (ds2 <!!> ["Asd"]) 

main :: IO Counts
main = runTestTT $ TestList [testDatasetCGet1, testDatasetCGet2, testDatasetGetIdxs1,
    testDatasetGetIdxs2]
