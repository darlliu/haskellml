module CSV(
readCSV
) where
import Data
import Data.List.Split
import Prelude hiding (foldl, (++), zipWith, head, tail, take, length, foldr, filter,map)
import qualified Prelude as P (foldl, (++), zipWith, head, tail, take, length, foldr, filter,map)
import Data.Char
import Data.Vector

parseRow :: String -> Row
parseRow s =  let ss = splitOn "," s in
    fromList $ fmap (\x->read x::Datum) ss

parseRows :: Vector String -> Vector Row
parseRows = map parseRow

readCSV::String -> IO DatasetR
readCSV fname = do
    fstr <- readFile fname
    let sls = lines fstr
    let header = fromList $ splitOn "," $ sls !! 0
    let rows = parseRows $ fromList $ P.tail sls
    return DatasetR {
        headerR = header,
        dR = rows
    }
