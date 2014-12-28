{-#LANGUAGE RecordWildCards#-}
module Genes where

import Control.Monad.Random;
import Control.Monad;  
import Data.Functor;
import Data.Function(on)
import Data.List(sortBy);
import Data.Foldable(maximumBy);

type Chromosome = [Int]
type Population = [Chromosome]
type GenRand = RandT StdGen IO

data EvOptions = EvOptions{
                populationSize::Int,
                individLength::Int,
                maxGeneration::Int,
                fitness::Fitness,
                targetFitness::Double,
                mutationChance::Double,
                elitePart::Double
                }

type Fitness = Chromosome -> Double

randChoice :: Rational -> GenRand a -> GenRand a -> GenRand a
randChoice chance th els = join (fromList [(th, chance), (els, 1 - chance)])

nextPopulation::EvOptions->Population->GenRand Population
nextPopulation EvOptions {..} pop = do
        newPop' <-liftM Prelude.concat $ replicateM (nonElite `div` 2) $ do
            a1 <- takeChr
            b1 <- takeChr
            (a2, b2) <- crossingover a1 b1
            a3 <- applyMutation a2
            b3 <- applyMutation b2
            return [a3, b3]
        let newPop = elite ++ newPop'
        return $ if Prelude.length newPop <= Prelude.length pop then newPop else Prelude.tail newPop
        where   fits = toRational <$> fitness  <$> pop
                maxfit = maximum fits
                chances = zip pop ((/maxfit) <$> fits)
                takeChr = fromList chances
                applyMutation c = randChoice (toRational mutationChance) (muatateChromosome c) (return c)
                sortedPop = snd $ Prelude.unzip $ sortBy (compare `on` fst) $ Prelude.zip (fitness <$> pop) pop 
                elite = Prelude.take (ceiling $ fromIntegral (Prelude.length pop) * elitePart) sortedPop
                nonElite = Prelude.length pop - Prelude.length elite

randChromosome::Int->GenRand Chromosome
randChromosome n = replicateM n randBool
        where randBool = uniform [0, 1]
        
randPopulation::Int->Int->GenRand Population
randPopulation n m = replicateM n $ randChromosome m

muatateChromosome::Chromosome->GenRand Chromosome
muatateChromosome chr = do 
        n <- uniform [0..length chr - 1]
        return $ invertAt n chr
        where invertAt n orig = take n orig ++ [el] ++ drop (n+1) orig
                where el = if orig !! n == 0 then 1 :: Int else 0

crossingover::Chromosome->Chromosome->GenRand (Chromosome, Chromosome)
crossingover a b = do
        n <- uniform [0..length a -1]
        return $ f n
        where f n = (take n a ++ drop n b, take n b ++ drop n a)

runEvol::EvOptions->Int->Population->IO (Int,Population)
runEvol opts@(EvOptions{..}) n pop =
   if (fitness (best pop) >= targetFitness)||(n>=maxGeneration)
   then return (n,pop)
   else do
        when(n `mod` 10 == 0) $ print $ show n ++ "-" ++ show pop
        rng <- newStdGen
        newPop <- evalRandT (nextPopulation opts pop) rng
        runEvol opts (n+1) newPop
   where best p = snd . maximumBy (compare `on` fst) $ zip (fitness <$> p) p
   

initEvol::EvOptions->IO (Int,Population)
initEvol opts = do 
   rng <- newStdGen
   initPopulation <- evalRandT (randPopulation (populationSize opts) (individLength opts)) rng
   result <- runEvol opts 0 initPopulation
   return result