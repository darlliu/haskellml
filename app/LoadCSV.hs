import qualified Data.Vector as DV
import Data 
import Preprocess
import CSV



main :: IO ()
main = do
    putStrLn "Loading `housing.csv` to perform a preprocesisng test"
    dr_ <- readCSV "data/housing.csv"
    drs <- processHousing dr_ 0.1
    let dr = fst drs
    let dr2 = snd drs
    putStrLn $ show $ DV.length $ dR dr 
    putStrLn $ show $ (dR dr) DV.! 0
    putStrLn $ show $ headerR dr

    -- putStrLn $ show $ headerR dr2
    putStrLn $ show $ DV.length $ dR dr2
    putStrLn $ show $ (dR dr2) DV.! 0
    putStrLn $ show $ headerR dr2
