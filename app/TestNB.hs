module TestNB where
import qualified Data.Vector as DV
import Data 
import Preprocess
import NaiveBayes
import CSV

main :: IO ()
main = do
    putStrLn "Loading `housing.csv` to perform a naive bayes classifier on high/low value"
    dr_ <- readCSV "data/housing.csv"
    drs <- processHousing2 dr_ 0.1
    let dc = toC $ fst drs
    let dc2 = toC $ snd drs
    let lb1 = dc <?> "median_house_value"
    let lb2 = dc2 <?> "median_house_value"
    let dc_ = dropCol dc "median_house_value"
    let dc2_ = dropCol dc2 "median_house_value"
    putStrLn "Done loading, now training"
    case lb1 of
        Nothing -> putStrLn "Error getting label1"
        Just (FVec lb1_) -> do 
            let intlb1 = DV.map (\x -> if x < 0 then 0 else 1) lb1_
            model <- trainNaiveBayes dc_ intlb1
            let pred0 = evalNaiveBayes dc_ model
            putStrLn "Baseline accuracy = 0 is "
            putStrLn $ show $ getAcu intlb1 (DV.map (\x->0) pred0)
            putStrLn "Baseline accuracy = 1 is "
            putStrLn $ show $ getAcu intlb1 (DV.map (\x->1) pred0)
            putStrLn "Train accuracy is "
            putStrLn $ show $ getAcu intlb1 pred0
            let pred = evalNaiveBayes dc2_ model
            case lb2 of
                Nothing -> putStrLn "Error getting label2"
                Just (FVec lb2_) -> do
                    let intlb2 = DV.map (\x -> if x < 0 then 0 else 1) lb2_
                    putStrLn "Accuracy is"
                    putStrLn $ show $ getAcu intlb2 pred