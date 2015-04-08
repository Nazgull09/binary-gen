module Main where

import Genes;
import Data.List
--import Data.Function(on)
import Debug.Trace
import Control.Monad.Random
import Control.Monad                                         --Изменения - мутация длинной в город, кроссинговером режем длинной в город.
fit::Fitness
fit chr = sum $ map fromIntegral chr

cityfit:: [[Int]] -> Fitness
cityfit intCityMap = foo 
   where
       foo :: Fitness 
       foo chr = traceShow (chr, chrPure) $ if length chrPure == 0 
                    then -9001.0 
                    else traceShow bonus bonus + traceShow cost cost
         where chrPure = filter (/=(-1)) chr
               bonus::Double
               bonus = 100.0 * (fromIntegral $ length chr) / (fromIntegral $ length chrPure)
               cost :: Double
               cost = fromIntegral (sum $ map (\(a,b) -> (intCityMap!!a)!!b) $ zip chrPure $ tail chrPure) 


bin2dec :: [Int] -> Int
bin2dec l = sum $ map (2^) $ findIndices (==1) $ reverse l

opts::EvOptions
opts= EvOptions{populationSize = 42,
                individLength = 30,
                cityn = 16,
                maxGeneration = 30,
                fitness = fit,
                targetFitness = 9000,
                mutationChance = 0.3,
                elitePart = 0.3}

main::IO()
main = do
    cityMap <- readFile "citymap.txt"
    let lcityMap = map (\x -> words x) $ lines cityMap
    let intCityMap = map (\row -> map (\way -> read way ::Int) row) lcityMap
    let cityOpts= EvOptions{populationSize = 42,
                individLength = 10,
                cityn = 3,
                maxGeneration = 50,
                fitness = cityfit intCityMap,
                targetFitness = 9000,
                mutationChance = 0.3,
                elitePart = 0.3}
    {-
    print $ cityfit intCityMap [0,-1,-1,2,1,0,1,0,2,2]
    forever $ do
        rng <- newStdGen
        print =<< evalRandT (mutateChromosome [0,-1,-1,2,1,0,1,0,2,2] cityOpts) rng 
    -}
    
    (n, result, best) <- initEvol cityOpts
    print $ "Best one: " ++ show best
    print $ "Best fit: " ++ (show $ cityfit intCityMap best)
    print $ show n ++ "-" ++ show result