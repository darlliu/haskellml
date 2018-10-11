import Data 

ds = DatasetC {
    headerC = ["Int","Float","Obj"],
    dC = [ICol [1], FCol [0.2], SCol ["Sth"]]
}

main :: IO ()
main = do
    let c = (ds !? "Obj")
    putStrLn ("\n<<---Testing datasetC get column 1 -->>" ++ (show c))
    let c = (ds !? "asd")
    putStrLn ("<<---Testing datasetC get column 2 -->>" ++ (show c))
