module Data
    (Column,
    DatasetC,
    Row, 
    DatasetR,
    someFunc
    ) where
data Column = ICol [Int] | FCol [Float] | SCol [String] 
data DatasetC = DatasetC{
    headerC :: [String],
    dC :: [Column]
}
type Row = [Float]
data DatasetR = DatasetR{
    headerR :: [String],
    dR :: [Row]
}

ds = DatasetC {
    headerC = ["Int","Float","Obj"],
    dC = [ICol [1], FCol [0.2], SCol ["Sth"]]
}
someFunc :: IO()

someFunc = putStr ("Here is something" ++ (foldl (\x y -> x++y) "" (headerC ds)))