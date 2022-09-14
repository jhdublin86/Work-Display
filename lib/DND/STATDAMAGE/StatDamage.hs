{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}

module DND.STATDAMAGE.StatDamage
where

import Data.Generics.Labels ()
import GHC.Generics (Generic)

import DND.CHARACTER.AbilityScores (Ability)

import DND.DEFENSE.Defense (Target)

data StatDamageType = Magic | Poisen | Disease deriving (Show, Eq, Ord, Generic)

data StatDamage a = SD { statdamagetype :: Maybe StatDamageType
                     , defense :: Maybe Target
                     , target :: Maybe Ability
                     , permdice :: [Int]
                     , dice :: a
                     , dicebonus :: Int
                     , duration :: [Int]
                     , durationbonus :: Int} deriving (Eq, Ord, Generic, Show)

showsd :: Show a => StatDamage a -> String
showsd a = (go . target $ a) ++ " " ++ (show . durationbonus $ a)
        where
        go mtar = case mtar of
            Nothing -> "Drain"
            Just y -> show y

instance Functor StatDamage where
    fmap f (SD sdt mtar mabi pdice a int b int2) = SD sdt mtar mabi pdice (f a) int b int2

instance Applicative StatDamage where
    pure a = SD Nothing Nothing Nothing [0] a 0 [0] 0
    (<*>)  (SD fa fb fc fd f fe fg fh)  (SD _ _ _ _ f' _ _ _)
        {-| (fa == Nothing) && (fb == Nothing) && (fc == Nothing) = SD a b c (f d) fe (f' f'') fg
        | (fb == Nothing) && (fc == Nothing) = SD fa b c (f d) fe (f' f'') fg
        | (fa == Nothing) && (fc == Nothing) = SD a fb c (f d) fe (f' f'') fg
        | (fa == Nothing) && (fb == Nothing) = SD a b fc (f d) fe (f' f'') fg
        | otherwise -}= SD fa fb fc fd (f f') fe fg fh

instance Foldable StatDamage where
    foldMap f (SD _ _ _ _ a _ _ _) = f a

instance Traversable StatDamage where
    traverse f' (SD a b c d e f g h) = (\x -> (SD a b c d x f g h)) <$> (f' e)

instance Monad StatDamage where
    return = pure
    (>>=) (SD a b c d e f g h) f' = SD a b c d (dice $ f' e) f g h