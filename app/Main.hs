module Main where
import qualified Data.Vector as DV
import Data 
import CSV

main :: IO ()
main = do
    dr <- readCSV "data/housing.csv"
    putStrLn $ show $ headerR dr
    putStrLn $ show $ DV.length $ dR dr 
    putStrLn $ show $ dR dr DV.! 20639
    let dc = toC dr
    putStrLn $ show $ DV.length $ dC $ dc
