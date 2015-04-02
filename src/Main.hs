module Main where

import Genes;

fit::Fitness
fit chr = sum $ map fromIntegral chr

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
   let cityOpts = EvOptions 42 (chrLength*cityNum) 100 fit 10 0.3 0.1
       
   (n, result) <- initEvol cityOpts
   cityMap <- readFile "citymap.txt"
   print cityMap
   print $ show n ++ "-" ++ show result
    
   