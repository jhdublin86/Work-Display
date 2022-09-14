{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

module DND.CHARACTER.AbilityScores
where

import Data.Generics.Labels ()
import GHC.Generics (Generic)

import DND.TemporaryValue (Temporary (T), temporary, permanent, revertToZero)

data AbilityScores a = AS { strength :: a
                         , dexterity :: a
                         , constitution :: a
                         , intelligence :: a
                         , wisdom :: a
                         , charisma :: a }
                         deriving (Generic, Eq, Ord)

instance Show a => Show (AbilityScores a) where
    show (AS s d c i w ch) = "AS " ++ (show s) ++ " " ++ (show d) ++ " " ++ (show c) ++ " " ++ (show i) ++ " " ++ (show w) ++ " " ++ (show ch)

instance Functor AbilityScores where
    fmap f (AS strength dexterity constitution intelligence wisdom charisma) 
        = AS (f strength) (f dexterity) (f constitution) (f intelligence) (f wisdom) (f charisma)

instance Applicative AbilityScores where
    pure a = AS a a a a a a
    (<*>) (AS f1 f2 f3 f4 f5 f6) (AS a1 a2 a3 a4 a5 a6) = AS (f1 a1) (f2 a2) (f3 a3) (f4 a4) (f5 a5) (f6 a6)

instance Monad AbilityScores where
    return = pure
    (AS a1 a2 a3 a4 a5 a6) >>= f = AS (strength . f $ a1) (dexterity . f $ a2) (constitution . f $ a3)
                                    (intelligence . f $ a4) (wisdom . f $ a5) (charisma . f $ a6)

instance Foldable AbilityScores where
    foldMap f (AS a1 a2 a3 a4 a5 a6) = f a1 <> f a2 <> f a3 <> f a4 <> f a5 <> f a6

instance Traversable AbilityScores where
    traverse f (AS a1 a2 a3 a4 a5 a6) = AS <$> f a1 <*> f a2 <*> f a3 <*> f a4 <*> f a5 <*> f a6

data Ability = Strength | Dexterity | Constitution | Intelligence | Wisdom | Charisma deriving (Show, Eq, Ord, Generic)
 
currentbonuses :: (Integral a, Num a) => AbilityScores (Temporary a) -> AbilityScores a
currentbonuses a = go <$> a
    where
        go :: (Num a ) => Temporary a -> a
        go t = permanent t - (temporary t)

as :: Int -> Int -> Int -> Int -> Int -> Int -> AbilityScores (Temporary Int)
as str dex con int wis cha = AS (T (pure 0) str) (T (pure 0) dex) (T (pure 0) con) (T (pure 0) int) (T (pure 0) wis) (T (pure 0) cha)

abix :: Maybe Ability -> (AbilityScores a -> a)
abix (Just Strength) = strength
abix (Just Dexterity) = dexterity
abix (Just Constitution) = constitution
abix (Just Intelligence) = intelligence
abix (Just Wisdom) = wisdom
abix (Just Charisma) = charisma
abix Nothing = strength 

revertas :: Num a => AbilityScores (Temporary a) -> AbilityScores (Temporary a)
revertas bs = fmap revertToZero bs                     