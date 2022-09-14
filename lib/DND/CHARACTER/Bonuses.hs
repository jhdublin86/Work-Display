{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DND.CHARACTER.Bonuses
where

import Data.Generics.Labels ()
import GHC.Generics (Generic)

import DND.CHARACTER.AbilityScores (AbilityScores)
import DND.CHARACTER.Class (ClassInfo, Skills)

import DND.DEFENSE.Armour (NonArmour)

data BonusTypes a = BT { luck :: a
                    , deflection :: a
                    , magic :: a
                    , holy :: a
                    , misc :: a
                    , size :: a
                    , item :: a
                    , penalty :: a} deriving (Generic, Show, Ord, Eq)

instance Functor BonusTypes where
    fmap f (BT a1 a2 a3 a4 a5 a6 a7 a8) = BT (f a1) (f a2) (f a3) (f a4) (f a5) (f a6) (f a7) (f a8)

instance Applicative BonusTypes where
    pure a = BT a a a a a a a a
    (<*>) (BT f1 f2 f3 f4 f5 f6 f7 f8) (BT a1 a2 a3 a4 a5 a6 a7 a8) = BT (f1 a1) (f2 a2) (f3 a3) (f4 a4) (f5 a5) (f6 a6) (f7 a7) (f8 a8)
  
data Bonuses a = B { abilityscores :: AbilityScores a
                   , miscclass :: ClassInfo a
                   , defense :: NonArmour a
                   , maxdex :: a
                   , damage :: a
                   , spellresistance :: a
                   , initiative :: a
                   , skills :: Skills a} deriving (Generic, Show, Ord, Eq)

instance Functor Bonuses where
    fmap f (B a0 a1 a2 a3 a4 a5 a6 a7) = B (f <$> a0) (f <$> a1) (f <$> a2) (f a3) (f a4) (f a5) (f a6) (f <$> a7)

instance Applicative Bonuses where
    pure a = B (pure a) (pure a) (pure a) a a a a (pure a)
    (<*>) (B f0 f1 f2 f3 f4 f5 f6 f7) (B a0 a1 a2 a3 a4 a5 a6 a7) = B (f0 <*> a0) (f1 <*> a1) (f2 <*> a2) (f3 a3) (f4 a4) (f5 a5) (f6 a6) (f7 <*> a7)
 
instance Foldable BonusTypes where
    foldMap f (BT a1 a2 a3 a4 a5 a6 a7 a8) = (f a1) <> (f a2) <> (f a3) <> (f a4) <> (f a5) <> (f a6) <> (f a7) <> (f a8)