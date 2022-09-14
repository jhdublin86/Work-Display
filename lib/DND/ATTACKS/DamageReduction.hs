{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}


module DND.ATTACKS.DamageReduction
where

import Control.Lens (Lens', set, over, view)
import Data.Generics.Labels ()
import GHC.Generics (Generic)
import Control.Lens.Indexed
import Data.Traversable

import DND.ATTACKS.Smite (Lawfulness (Lawful, Chaotic), Benificence (Good, Evil))

import DND.TemporaryValue (Temporary)

data DamageType = Reduced | Bludgeoning | Slashing | Piercing | Normal deriving (Generic, Show, Eq, Ord)

data Material =  Reduc | GooD | EviL | ChaoS | LaW | Bludgeon | Slash | Pierce | Magical | Norm deriving (Generic, Show, Eq, Ord)

instance Enum Material where
    toEnum 0 = Reduc
    toEnum 1 = GooD
    toEnum 2 = EviL
    toEnum 3 = ChaoS
    toEnum 4 = LaW
    toEnum 5 = Bludgeon
    toEnum 6 = Slash
    toEnum 7 = Pierce
    toEnum 8 = Magical
    toEnum _ = Norm
    fromEnum Reduc = 0
    fromEnum GooD = 1
    fromEnum EviL = 2
    fromEnum ChaoS = 3
    fromEnum LaW = 4
    fromEnum Bludgeon = 5
    fromEnum Slash = 6
    fromEnum Pierce = 7
    fromEnum Magical = 8
    fromEnum Norm = 9

data DamageReduction a = DR { reduc :: a
                            , good :: a
                            , evil :: a
                            , chaos :: a
                            , law :: a
                            , bludgeon :: a
                            , slash ::a 
                            , pierc :: a
                            , magical :: a 
                            , norm :: a} deriving (Generic, Show, Eq, Ord)

instance Functor DamageReduction where
    fmap = fmapDefault

instance Applicative DamageReduction where
    pure a = DR a a a a a a a a a a
    (<*>) (DR f1 f2 f3 f4 f5 f6 f7 f8 f9 f10) (DR a1 a2 a3 a4 a5 a6 a7 a8 a9 a10) = DR (f1 a1) (f2 a2) (f3 a3) (f4 a4) (f5 a5) (f6 a6) (f7 a7) (f8 a8) (f9 a9) (f10 a10)

instance FunctorWithIndex Material DamageReduction

instance Foldable DamageReduction where
    foldMap = foldMapDefault
    length _ = 9

instance FoldableWithIndex Material DamageReduction

instance Traversable DamageReduction where
    traverse = itraverse . const

instance TraversableWithIndex Material DamageReduction where
    itraverse ƒ (DR alpha a b c d e f g h i) =
        DR  <$> ƒ (Reduc) alpha <*>  ƒ (GooD) a <*> ƒ (EviL) b <*> ƒ (ChaoS) c <*> ƒ (LaW) d <*> ƒ (Bludgeon) e 
        <*> ƒ (Slash) f <*> ƒ (Pierce) g <*> ƒ (Magical) h <*> ƒ (Norm) i

drix :: Material -> Lens' (DamageReduction a) a
drix Norm = #norm
drix Reduc = #reduc
drix Magical = #magical
drix Bludgeon = #bludgeon
drix Slash = #slash
drix Pierce = #pierc
drix GooD = #good
drix EviL = #evil
drix ChaoS = #chaos
drix LaW = #law

dt2mat :: DamageType -> Material
dt2mat Normal = Norm
dt2mat Piercing = Pierce
dt2mat Slashing = Slash
dt2mat Bludgeoning = Bludgeon
dt2mat Reduced = Reduc

b2mat :: Benificence -> Material
b2mat Good = GooD
b2mat Evil = EviL
b2mat _ = Reduc

l2mat :: Lawfulness -> Material
l2mat Chaotic = ChaoS
l2mat Lawful = LaW
l2mat _ = Reduc

dredux :: ((Maybe Int) -> Temporary (Maybe Int) -> Temporary (Maybe Int)) -> Material -> Int -> DamageReduction (Temporary (Maybe Int)) -> DamageReduction (Temporary (Maybe Int))
dredux f m a dr
    | m == Norm = set (#norm) (view (#norm) dr) $ (f (Just a)) <$> dr
    | m == Pierce = set (#norm) (view (#norm) dr) $ set (#pierc) (view (#pierc) dr) $ (f (Just a)) <$> dr
    | m == Slash= set (#norm) (view (#norm) dr) $ set (#slash) (view (#slash) dr) $ (f (Just a)) <$> dr
    | m == Bludgeon = set (#norm) (view (#norm) dr) $ set (#bludgeon) (view (#bludgeon) dr) $ (f (Just a)) <$> dr
    | m == GooD = set (#norm) (view (#norm) dr) $ set (#good) (view (#good) dr) $ (f (Just a)) <$> dr
    | m == EviL = set (#norm) (view (#norm) dr) $ set (#evil) (view (#evil) dr) $ (f (Just a)) <$> dr
    | m == ChaoS = set (#norm) (view (#norm) dr) $ set (#chaos) (view (#chaos) dr) $ (f (Just a)) <$> dr
    | m == LaW = set (#norm) (view (#norm) dr) $ set (#law) (view (#law) dr) $ (f (Just a)) <$> dr
    | m == Magical = set (#norm) (view (#norm) dr) $ set (#magical) (view (#magical) dr) $ (f (Just a)) <$> dr
    | otherwise = dr

noReductions :: DamageReduction (Temporary (Maybe a))
noReductions = DR (pure Nothing) (pure Nothing) (pure Nothing) (pure Nothing) (pure Nothing) (pure Nothing) (pure Nothing) (pure Nothing) (pure Nothing) (pure Nothing)