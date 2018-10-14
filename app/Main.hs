module Main where
import Data 
import CSV

main :: IO ()
main = do
    dr <- readCSV "data/housing.csv"
    putStrLn $ show $ headerR dr
    putStrLn $ show $ length $ dR dr 
    putStrLn $ show $ length $ dC $ toC dr
    putStrLn $ show $ dR dr !! 20639
