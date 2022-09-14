{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

module DND.Roll
where

import System.Random
import Data.Generics.Labels ()
import GHC.Generics (Generic)
import Control.Monad.Trans.State.Strict (State, state)

data Roll = Roll { runRoll :: Int}
  deriving (Read, Show, Eq, Generic)

type Dice = State StdGen

instance Num Roll where
    (+) (Roll a) (Roll b) = Roll (a + b)
    (*) (Roll a) (Roll b) = Roll (a * b)
    fromInteger a = Roll $ fromInteger a
    abs (Roll a) = Roll (abs a)
    signum (Roll a) = Roll $ signum a
    negate (Roll a) = Roll $ negate a

roll :: RandomGen g => Int -> g -> (Roll, g)
roll 0 g = (Roll 0, g)
roll n g = (Roll n1, g1)
  where
    (n1, g1) = randomR (1, n) g

dice :: Int -> Dice Roll
dice a = state $ roll a

roll2mint :: Roll -> Maybe Int
roll2mint r
    | runRoll r == 0 = Nothing
    | otherwise = Just $ runRoll r
{-- sumdice' :: [Int] -> Dice Roll
sumdice' [] = dice 0
sumdice' (r : rs) = do
    a <- dice r
    b <- sumdice rs
    return (a + b) --}

listdice :: [Int] -> Dice [Roll]
listdice a = dice ~> a

sumdice :: [Int] -> Dice Roll
sumdice a = fmap sum $ listdice a

-- function :: Applicative f => State a b -> State a (f b)
-- function f = state $ fmap (\(x,a) -> (pure x, a)) (runState f)

--function2 :: (Functor f, Applicative g) => (a -> f b) -> (a -> f (g b))
--function2 f = fmap (fmap pure) f

--function3 :: (a -> Dice b) -> Maybe a -> Dice (Maybe b)
--function3 _ Nothing = return Nothing
--function3 f (Just a) = function $ f a

--function35 :: (a -> Dice b) -> Maybe a -> Maybe (Dice b)
--function35 f a = a >>= (fmap Just f)

(~>) :: (Applicative f, Monad g, Traversable g) => (a -> f b) -> (g a) -> f (g b)
(~>) f a = sequenceA $ a >>= (fmap pure f)

{--
maybedice :: Maybe Int -> Dice (Maybe Roll)
maybedice Nothing = return Nothing
maybedice (Just a) = function $ dice a

maybesumdice :: Maybe [Int] -> Dice (Maybe Roll)
maybesumdice Nothing = return Nothing
maybesumdice (Just a) = function $ sumdice a

{--   function $ dice 0
maybesumdice (Just (r : rs)) = do
    a <- function $ dice r
    b <- maybesumdice (Just rs)
    return ((+) <$> a <*> b) --}

maybelistdice :: Maybe [Int] -> Dice (Maybe [Roll])
maybelistdice Nothing = return Nothing
maybelistdice (Just a) = function $ listdice a

{--(Just []) = return (Just [])
maybelistdice (Just (r : rs)) = do
    first <- function (function $ dice r)
    second <- maybelistdice (Just rs)
    return ((++) <$> first <*> second)--}
--}

đ :: Int -> Int -> [Int]
đ a b = take a $ cycle [b]