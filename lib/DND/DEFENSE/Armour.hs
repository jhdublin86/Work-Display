{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

module DND.DEFENSE.Armour
where

import Control.Applicative
import Data.Generics.Labels ()
import GHC.Generics (Generic)

import DND.ATTACKS.DamageReduction ()

data Armour a = A { energy :: a
                  , physical :: a
                  , maxDex :: Maybe Int
                  , achPenalty :: Maybe Int
                  , name :: String}
                  deriving (Generic, Show, Eq, Ord)

defaultArmour :: Armour Int
defaultArmour = A 0 0 Nothing Nothing "Armour"

instance Functor Armour where
    fmap f (A plate shield maxdex acpen name) = A (f plate) (f shield) maxdex acpen name

instance Foldable Armour where
    foldMap f (A plate shield _ _ _) = (f plate) <> (f shield)

instance Applicative Armour where
    pure a = A a a Nothing Nothing "Armour"
    (<*>) (A f1 f2 f3 f4 f5) (A a1 a2 _ _ _) = A (f1 a1) (f2 a2) f3 f4 f5

instance Num a => Num (Armour a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger 

data NonArmour a = NA { saves :: a
                      , attacks :: a}
                      deriving (Generic, Show, Eq, Ord)

instance Functor NonArmour where
    fmap f (NA saves attacks) = NA (f saves) (f attacks)

instance Foldable NonArmour where
    foldMap f (NA saves attacks) = (f saves) <> (f attacks)

instance Applicative NonArmour where
    pure a = NA a a 
    (<*>) (NA f1 f2) (NA a1 a2) = NA (f1 a1) (f2 a2)

instance Num a => Num (NonArmour a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger 