{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DND.CHARACTER.ClassFeatures
where

import Data.Generics.Labels ()
import GHC.Generics (Generic)
import Data.Traversable
import Control.Lens.Indexed

import DND.ATTACKS.Smite (FavouredEnemy)
import DND.ATTACKS.Weapon ()

import DND.CHARACTER.Class ()

import DND.DAMAGEDICE.Elemental (Elemental (Fire))

import DND.SPELLS.Spell (SpellSchool (Universal))

data ClassFeatures b a = CF { mysticspells :: SpellBook b a
                          , arcanespells :: SpellBook b a
                          , solarionfeatures :: SolarionFeatures
                          , envoyfeatures :: EnvoyFeatures
                          , feats :: Feats} deriving (Show, Eq, Generic, Ord)
                          
instance Functor (ClassFeatures b) where
    fmap f (CF a b c d e) = CF (f <$> a) (f <$> b) c d e

instance Applicative (ClassFeatures b) where
    pure a = CF (pure a) (pure a) (SolFeat False False []) (EnvFeat [] False) nofeats
    (<*>) (CF f1 f2 f3 f4 f5) (CF a1 a2 _ _ _) = CF (f1 <*> a1) (f2 <*> a2) f3 f4 f5

data SolarionFeatures = SolFeat { stellarrush :: Bool
                                , solaracceleration :: Bool
                                , sidereal :: [Int]} deriving (Show, Eq, Generic, Ord)

data EnvoyFeatures = EnvFeat { expertise :: [Int]
                             , improvedgetem :: Bool} deriving (Show, Eq, Generic, Ord)

data SpellLevel a = SL { zero :: a
                       , first :: a
                       , second :: a
                       , third :: a
                       , fourth :: a
                       , fifth :: a
                       , sixth :: a
                       , seventh :: a
                       , eighth :: a
                       , ninth :: a} deriving (Show, Eq, Generic, Ord)

instance Functor SpellLevel where
    fmap f (SL a0 a1 a2 a3 a4 a5 a6 a7 a8 a9) = SL (f a0) (f a1) (f a2) (f a3) (f a4) (f a5) (f a6) (f a7) (f a8) (f a9)

instance FunctorWithIndex Int SpellLevel

instance Applicative SpellLevel where
    pure a = SL a a a a a a a a a a
    (<*>) (SL f0 f1 f2 f3 f4 f5 f6 f7 f8 f9) (SL a0 a1 a2 a3 a4 a5 a6 a7 a8 a9) = 
        SL (f0 a0) (f1 a1) (f2 a2) (f3 a3) (f4 a4) (f5 a5) (f6 a6) (f7 a7) (f8 a8) (f9 a9)

instance Foldable SpellLevel where
    foldMap = foldMapDefault
    length _ = 10

instance FoldableWithIndex Int SpellLevel

instance Traversable SpellLevel where
    traverse = itraverse . const

instance TraversableWithIndex Int SpellLevel where
    itraverse ƒ (SL a b c d e f g h i j) =
        SL <$> ƒ 0 a <*> ƒ 1 b <*> ƒ 2 c <*> ƒ 3 d <*> ƒ 4 e <*> ƒ 5 f <*> ƒ 6 g <*> ƒ 7 h <*> ƒ 8 i <*> ƒ 9 j

data SpellBook b a = SB {known :: SpellLevel [b], perday :: SpellLevel a} deriving (Show, Eq, Generic, Ord)

instance Functor (SpellBook b) where
    fmap f (SB a b) = SB a (f <$> b)

instance Applicative (SpellBook b) where
    pure a = SB (pure []) (pure a)
    (<*>) (SB f1 f2) (SB _ a2) = SB f1 (f2 <*> a2)

data Feats = Feats { deadlyaim :: Bool
                   , weaponfinesse :: Bool
                   , arcanestrike :: Bool
                   , improvedinitiative :: Bool
                   , improveddisarm :: Bool
                   , improvedtrip :: Bool
                   , improvedgrapple :: Bool} deriving (Show, Eq, Generic, Ord)

nofeats :: Feats
nofeats = Feats False False False False False False False