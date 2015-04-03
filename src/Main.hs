module Main where

import Genes;
import Data.List
import Data.List.Split
import Data.Function(on)
import Data.Maybe

fit::Fitness
fit chr = sum $ map fromIntegral chr

cityfit:: Int -> String -> Fitness
cityfit cityNum citymap chr = do ((sum ( map read mapfit :: [Double])) + allq::Double)
                 where mapfit = map (\city -> (lcitymap !! (fst city)) !! (snd city) ) roadList
                       roadList = zip decChr $ tail decChr
                       decChr = map bin2dec $ splitEvery chrLength chr
                       lcitymap = map (\x -> words x) $ lines citymap
                       chrLength = ceiling $ logBase 2.0 $ fromIntegral cityNum  
                       allq = if ((length decChr) == (length $ nub decChr)) 
                              then 0.0
                              else -1000.0

bin2dec :: [Int] -> Int
bin2dec l = sum $ map (2^) $ findIndices (==1) $ reverse l

opts::EvOptions
opts= EvOptions{populationSize = 42,
                individLength = 10,
                maxGeneration = 100,
                fitness = fit,
                targetFitness = 0,
                mutationChance = 0.3,
                elitePart = 0.1}

main::IO()
main = do
   cnum <- getLine
   let cityNum = read cnum :: Int
   let chrLength = ceiling $ logBase 2.0 $ fromIntegral cityNum
   citymap <- readFile "citymap.txt"   

   let lcitymap = map (\x -> words x) $ lines citymap
   let z = [1,0,0,0,1,1] :: [Int]
   
   let l = cityfit cityNum citymap

   let cityOpts = EvOptions 42 (chrLength*cityNum) 100 l 0 0.3 0.1   
   (n, result) <- initEvol cityOpts
  
   print $ show n ++ "-" ++ show result 
   print $ snd . maximumBy (compare `on` fst) $ zip (map l result) result
   let bestChr = snd . maximumBy (compare `on` fst) $ zip (map l result) result
   let decChr = map bin2dec $ splitEvery chrLength bestChr
   let roadList = zip decChr $ tail decChr 
   print roadList
   print $ l bestChr