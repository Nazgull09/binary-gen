module Main where

import Genes;
import Data.List
import Data.List.Split
import Data.Function(on)
--import Debug.Trace
                                                                    --Изменения - мутация длинной в город, кроссинговером режем длинной в город.
fit::Fitness
fit chr = sum $ map fromIntegral chr

cityfit:: Int -> String -> Fitness
cityfit cityNum citymap chr = do ((sum ( map read mapfit :: [Double])) + allq::Double)
                 where mapfit = map (\(a,b) -> (lcitymap !! a !!b)) roadList
                       roadList = zip decChr $ tail decChr
                       decChr = map bin2dec $ chunksOf chrLength chr
                       lcitymap = map (\x -> words x) $ lines citymap
                       chrLength = ceiling lo
                       lo::Double
                       lo = logBase 2.0 $ fromIntegral cityNum
                       allq = if ((length decChr) == (length $ nub decChr)) 
                              then 0.0
                              else -5000.0



bin2dec :: [Int] -> Int
bin2dec l = sum $ map (2^) $ findIndices (==1) $ reverse l

opts::EvOptions
opts= EvOptions{populationSize = 42,
                individLength = 10,
                maxGeneration = 100,
                fitness = fit,
                targetFitness = 0,
                mutationChance = 0.3,
                elitePart = 0.3}

main::IO()
main = do
   cnum <- getLine
   let cityNum = read cnum :: Int
   let lo = logBase 2.0 $ fromIntegral cityNum :: Double
   let chrLength = ceiling lo
   citymap <- readFile "citymap.txt"

--   let lcitymap = map (\x -> words x) $ lines citymap
--   let z = [1,0,0,0,1,1] :: [Int]
   
   let l = cityfit cityNum citymap

   let cityOpts = EvOptions 200 (chrLength*cityNum) 2000 l 0 0.5 0.1
   (n, result) <- initEvol cityOpts
  
   print $ show n ++ "-" ++ show result 
   print $ snd . maximumBy (compare `on` fst) $ zip (map l result) result
   let bestChr = snd . maximumBy (compare `on` fst) $ zip (map l result) result
   let decChr = map bin2dec $ chunksOf chrLength bestChr
   let roadList = zip decChr $ tail decChr 
   print roadList
   print $ l bestChr