{-#LANGUAGE RecordWildCards#-}
module Genes where

import Control.Monad.Random;
import Control.Monad;  
import Data.Functor;
import Data.Function(on)
import Data.List(sortBy);
import Data.Foldable(maximumBy);
import Debug.Trace
import Control.Monad.IO.Class

type Chromosome = [Int]
type Population = [Chromosome]
type GenRand = RandT StdGen IO

data EvOptions = EvOptions{
                populationSize::Int,
                individLength::Int,
                cityn::Int,
                maxGeneration::Int,
                fitness::Fitness,
                targetFitness::Double,
                mutationChance::Double,
                elitePart::Double
                }

type Fitness = Chromosome -> Double

randChoice :: Rational -> GenRand a -> GenRand a -> GenRand a
randChoice chance th els = join (fromList [(th, chance), (els, 1 - chance)])        --fromList - часть рандома, выдает рандом из списка по вероятности

nextPopulation::EvOptions->Population->GenRand Population
nextPopulation opts@(EvOptions {..}) pop = do
        newPop' <-liftM concat $ replicateM (nonElite `div` 2) $ do
            a1 <- takeChr
            b1 <- takeChr
            (a2, b2) <- crossingover a1 b1
            a3 <- applyMutation a2
            b3 <- applyMutation b2
            return $ traceShow (a3, b3) $ [a3, b3]
        let newPop = elite ++ newPop'
        return $ if length newPop <= length pop then newPop else tail newPop
        where   fits = toRational <$> fitness  <$> pop                              --map fitness pop
                maxfit = maximum fits
                chances = zip pop ((/maxfit) <$> fits)
                takeChr = fromList chances
                -- с некоторым шансом либо смутировать, либо просто венуть хромосому с
                applyMutation c = randChoice (toRational mutationChance) (mutateChromosome c opts) (traceShow ("no mutation", c) $ return c) 
                sortedPop = snd $ unzip $ sortBy (compare `on` fst) $ zip (fitness <$> pop) pop 
                -- другой подход к сортировке, иквабл для хромосомы
                elite = take (ceiling $ fromIntegral (length pop) * elitePart) sortedPop
                nonElite = length pop - length elite

randChromosome::Int->Int->GenRand Chromosome
randChromosome n cityn = ([0] ++) <$> replicateM (n-1) randCity              --Добавить контакт [0]
        where randCity = uniform [-1..cityn-1]
        
randPopulation::Int->Int->Int->GenRand Population
randPopulation n m cityn = replicateM n $ randChromosome m cityn

mutateChromosome::Chromosome->EvOptions->GenRand Chromosome
mutateChromosome chr opts = do                              --заменяем н-ый элемент инверсией
        n <- uniform [0..length chr - 1]
        let arr = if n/=0 then take (n-1) chr ++ drop (n+1) chr else tail chr 
        newA <- uniform arr
        return $ take n chr ++ [newA] ++ drop (n+1) chr
        
crossingover::Chromosome->Chromosome->GenRand (Chromosome, Chromosome)
crossingover a b = do
        n <- uniform [0..length a -1]
        return $ f n
        where f n = (take n a ++ drop n b, take n b ++ drop n a)

runEvol::EvOptions->Int->Population->IO (Int,Population,Chromosome)
runEvol opts@(EvOptions{..}) n pop =
   if (fitness best >= targetFitness)||(n>=maxGeneration)
   then return (n,pop, best)
   else do
        -- when(n `mod` 10 == 0) $ print $ show n ++ "-" ++ show pop
        rng <- newStdGen
        newPop <- evalRandT (nextPopulation opts pop) rng
        runEvol opts (n+1) newPop
   where best = snd . maximumBy (compare `on` fst) $ zip (fitness <$> pop) pop


initEvol::EvOptions->IO (Int,Population,Chromosome)
initEvol opts = do 
   rng <- newStdGen
   initPopulation <- evalRandT (randPopulation (populationSize opts) (individLength opts) (cityn opts)) rng
   runEvol opts 0 initPopulation