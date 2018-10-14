module CSV(
readCSV
) where
import Data
import Data.List.Split

parseRow :: String -> Row
parseRow s =  let ss = splitOn "," s in
    fmap (\x->read x::Datum) ss

parseRows :: [String] -> [Row]
parseRows = fmap parseRow

readCSV::String -> IO DatasetR
readCSV fname = do
    fstr <- readFile fname
    let sls = lines fstr
    let header = splitOn "," $ sls !! 0
    let rows = parseRows $ tail sls
    return DatasetR {
        headerR = header,
        dR = rows
    }
