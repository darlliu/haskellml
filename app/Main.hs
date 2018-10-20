import qualified Data.Vector as DV
import Data 
import Preprocess
import CSV



main :: IO ()
main = do
    dr <- readCSV "data/housing.csv"
    drs <- processHousing dr 0.1
    let dr = fst drs
    let dr2 = snd drs
    putStrLn $ show $ DV.length $ dR dr 
    putStrLn $ show $ headerR dr
    -- putStrLn $ show $ headerR dr2
    putStrLn $ show $ DV.length $ dR dr2
    putStrLn $ show $ headerR dr2
