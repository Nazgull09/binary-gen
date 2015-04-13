module Main where

import Genes;
import Data.List
import Debug.Trace
import Control.Monad.Random
import Control.Monad                                   
import System.Environment   

fit chr = sum $ map fromIntegral chr

cityfit:: [[Int]] -> Fitness
cityfit intCityMap = foo 
   where
       foo :: Fitness 
       foo chr = {- traceShow (chr, chrPure) $ -} if length chrPure == 0 
                    then -9001.0 
                    else bonus + cost + bonusnub
         where chrPure = filter (/=(-1)) chr
               bonus::Double
               bonus = 100.0 * (fromIntegral $ length chr) / (fromIntegral $ length chrPure)
               cost :: Double
               cost = fromIntegral (sum $ map (\(a,b) -> (intCityMap!!a)!!b) $ zip chrPure $ tail chrPure)
               bonusnub ::Double
               bonusnub = fromIntegral $ (-300)*(length chr - (length $ nub chr) ) 


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
                elitePart = 0.1}

main::IO()
main = do
    args <- getArgs
    let intArgs = map (\arg -> read arg ::Int) args
    cityMap <- readFile "citymap.txt"
    let lcityMap = map (\x -> words x) $ lines cityMap
    let intCityMap = map (\row -> map (\way -> read way ::Int) row) lcityMap
    let cityOpts= EvOptions{populationSize = 50,
                individLength = 8,
                cityn = 4,
                maxGeneration = 500,
                fitness = cityfit intCityMap,
                targetFitness = 500,
                mutationChance = 0.8,
                elitePart = 0.1}
    let argCityOpts= EvOptions{populationSize = intArgs !! 0,
                individLength = intArgs !! 1,
                cityn = intArgs !! 2,
                maxGeneration = intArgs !! 3,
                fitness = cityfit intCityMap,
                targetFitness = 500,
                mutationChance = (read $ args !! 4) :: Double,
                elitePart = 0.1}
    
    (n, result, best) <- initEvol cityOpts
    print $ "Best one: " ++ show best
    print $ "Best fit: " ++ (show $ cityfit intCityMap best)
    print $ "Best cost:" ++ show (sum $ map (\(a,b) -> (intCityMap!!a)!!b) $ zip (filter (/=(-1)) best) $ tail (filter (/=(-1)) best))
    --print $ show n ++ "-" ++ show result
