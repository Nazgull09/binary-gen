module Main where

import Genes;
import Data.List
import Data.List.Split
import Data.Maybe

fit::Fitness
fit chr = sum $ map fromIntegral chr

nextElem :: Eq a => a -> [a] -> Maybe a         --Хаха, оказалось проще зипануть лист и его тэйл
nextElem e xs = listToMaybe . drop 1 . dropWhile (/= e) $ xs ++ (take 1 xs)

cityfit:: Int -> String -> Chromosome-> Int
cityfit cityNum citymap chr = do sum ( map read mapfit :: [Int])
                 where mapfit = map (\city -> (lcitymap !! (fst city)) !! (snd city) ) roadList
                       roadList = zip decChr $ tail decChr
                       decChr = map bin2dec $ splitEvery chrLength chr
                       lcitymap = map (\x -> words x) $ lines citymap
                       chrLength = ceiling $ logBase 2.0 $ fromIntegral cityNum     


bin2dec :: [Int] -> Int
bin2dec l = sum $ map (2^) $ findIndices (==1) $ reverse l

opts::EvOptions
opts= EvOptions{populationSize = 42,
                individLength = 10,
                maxGeneration = 10,
                fitness = fit,
                targetFitness = 10,
                mutationChance = 0.3,
                elitePart = 0.1}

main::IO()
main = do
   cnum <- getLine
   let cityNum = read cnum :: Int
   let chrLength = ceiling $ logBase 2.0 $ fromIntegral cityNum
   citymap <- readFile "citymap.txt"
   let cityOpts = EvOptions 42 (chrLength*cityNum) 100 fit 10 0.3 0.1   
   (n, result) <- initEvol cityOpts
   let lcitymap = map (\x -> words x) $ lines citymap
   let z = [1,0,0,0,1,1] :: [Int]
   let decChr = map bin2dec $ splitEvery chrLength z
   let roadList = zip decChr $ tail decChr 
   let mapfit = map (\city -> (lcitymap !! (fst city) !! (snd city) )) roadList
--   let cityfitness = sum ( map read mapfit :: [Int])
   let l = cityfit cityNum citymap
   print $ l z