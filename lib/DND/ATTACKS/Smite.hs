{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DND.ATTACKS.Smite
where

import Control.Applicative
import Data.Generics.Labels ()
import GHC.Generics (Generic)
import Control.Lens.Indexed
import Data.Traversable

import DND.TemporaryValue (temporary, Temporary)

data Lawfulness = Lawful | Neutral | Chaotic deriving (Show, Eq, Generic, Ord)

data Benificence = Evil | NeutraL | Good deriving (Show, Eq, Generic, Ord)

data Alignment = Al { lawfullness :: Lawfulness
                           , benificence :: Benificence} deriving (Show, Eq, Generic, Ord)

data Race = Human | Elf | Gnome | Orc | Halfling | Animal | Outsider | Abnormal | Plant | Elemental | Dragon | UnDead | Tengu | Kitsune | Construct | Fey |
            Kasatha | Lashunta | Android | Shirren | Vesk | Ysoki deriving (Show, Eq, Generic, Ord)

data Smite = Smite { condition :: (Either Lawfulness Benificence)
                   , smiteattack :: Int
                   , smitedamage :: Int} deriving (Show, Eq, Generic, Ord)

data FavouredEnemy a = FE { human :: a
                          , elf :: a
                          , gnome :: a
                          , orc :: a
                          , halfling :: a
                          , animal :: a
                          , outsider :: a
                          , abnormal :: a
                          , plant :: a
                          , elemental :: a
                          , dragon :: a
                          , undead :: a
                          , tengu :: a
                          , kitsune :: a
                          , construct :: a
                          , fey :: a} deriving (Show, Eq, Generic, Ord)

instance Functor FavouredEnemy where
    fmap = fmapDefault

instance FunctorWithIndex Race FavouredEnemy

instance Applicative FavouredEnemy where
    pure a = FE a a a a a a a a a a a a a a a a
    (<*>) (FE f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16) (FE a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16)
        = FE (f1 a1) (f2 a2) (f3 a3) (f4 a4) (f5 a5) (f6 a6) (f7 a7) (f8 a8) (f9 a9) (f10 a10) (f11 a11) (f12 a12) (f13 a13) (f14 a14) (f15 a15) (f16 a16)

instance Foldable FavouredEnemy where
    foldMap = foldMapDefault
    length _ = 16

instance FoldableWithIndex Race FavouredEnemy

instance Traversable FavouredEnemy where
    traverse = itraverse . const

instance TraversableWithIndex Race FavouredEnemy where
    itraverse ƒ (FE a b c d e f g h i j k l m n o p) =
        FE <$> ƒ Human a <*> ƒ Elf b <*> ƒ Gnome c <*> ƒ Orc d <*> ƒ Halfling e <*> ƒ Animal f
        <*> ƒ Outsider g <*> ƒ Abnormal h <*> ƒ Plant i <*> ƒ Elemental j <*> ƒ Dragon k <*> ƒ UnDead l
        <*> ƒ Tengu m <*> ƒ Kitsune n <*> ƒ Construct o <*> ƒ Fey p


instance Num a => Num (FavouredEnemy a) where
    (+) = liftA2 (+) 
    (*) = liftA2 (*)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger a = pure $ fromInteger a

instance Num a => Semigroup (FavouredEnemy a) where
    (<>) a b = a + b

instance Num a => Monoid (FavouredEnemy a) where
    mempty = 0

rix :: Race -> (FavouredEnemy a -> a)
rix Human = human
rix Elf = elf
rix Gnome = gnome
rix Orc = orc
rix Halfling = halfling
rix Animal = animal
rix Outsider = outsider
rix Abnormal = abnormal
rix Plant = plant
rix Elemental = elemental
rix Dragon = dragon
rix UnDead = undead
rix Tengu = tengu
rix Kitsune = kitsune
rix Construct = construct
rix Fey = fey

raceandfe2bonus :: Num a => Race -> FavouredEnemy (Temporary a) -> a
raceandfe2bonus rce fe = temporary $ ifoldMap go fe
    where
        go :: Num a => Race -> Temporary a -> Temporary a
        go a b 
            | a == rce = b
            | otherwise = 0