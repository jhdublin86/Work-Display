{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module DND.ATTACKS.Attack
where

import Control.Lens (set, over, view)
import Data.Generics.Labels ()
import GHC.Generics (Generic)

import DND.ATTACKS.DamageReduction (DamageType (Bludgeoning), Material (Reduc))
import DND.ATTACKS.Smite (Smite, FavouredEnemy, Alignment (Al), Lawfulness (Neutral), Benificence (NeutraL))
import DND.ATTACKS.Weapon

import DND.CHARACTER.AbilityScores (Ability)

import DND.DAMAGEDICE.DamageDice (DamageDice (damagedice))

import DND.DEFENSE.Defense (Defense (Defense))

import DND.STATDAMAGE.StatDamage (StatDamage)

import DND.STATUSEFFECTS.Effect (Effect)
import DND.STATUSEFFECTS.Effects (Temporal, setmodifyer, viewtempmodifyier)

data Result a = CriticalMiss | Miss { missby :: Maybe Int, defensetype :: Defense} | Hit { hitdamage :: a}
                | CriticalHit { critdamage :: a, vitdamage :: a} deriving (Show, Eq, Generic)

showresult :: Result (Maybe Int) -> String
showresult CriticalMiss = "CriticalMiss"
showresult (Miss a b) = "Miss by " ++ showmaybeint a ++ ", " ++ show b
showresult (Hit a) = "Hit " ++ showmaybeint a ++ " dam"
showresult (CriticalHit a b) = "C.Hit "  ++ showmaybeint a ++ " dam " ++ showmaybeint b ++ " inj"

showresults :: [Result (Maybe Int)] -> String
showresults [] = []
showresults (a : as) = showresult a ++ ", " ++ (showresults as)

showmaybeint :: Maybe Int -> String
showmaybeint mi = case mi of
    Nothing -> "Nothing"
    Just y -> show y

instance Functor Result where
    fmap _ CriticalMiss = CriticalMiss
    fmap _ (Miss a b) = Miss a b
    fmap f (Hit a) = Hit $ f a
    fmap f (CriticalHit a1 a2) = CriticalHit (f a1) (f a2)

data AttackInfo a = AInfo { attackname :: String
                        , range :: Range
                        , increment :: Int
                        , damagetype :: DamageType
                        , material :: Material
                        , attackbonus :: Maybe a
                        , versus :: Defense
                        , critical :: Critical
                        , dambonus :: Maybe Ability
                        , alignment :: Alignment
                        , preparedagainstcharge :: Bool
                        , spellproxy :: (Bool,Bool)} deriving (Show, Eq, Generic, Ord)

data Attack a b = Attack { info :: AttackInfo a
                         , rolls :: AttackRoll b
                         , ddice :: Maybe (DamageDice b)
                         , mddice :: [DamageDice b]
                         , mstatdamage :: Maybe (StatDamage b)
                         , meffect :: Maybe (Effect (Temporal b))
                         , msingddice :: Maybe (DamageDice b)
                         , msingeffect :: [(Effect (Temporal b))]} deriving (Generic, Eq, Show, Ord)

instance Functor (Attack b) where
    fmap f (Attack s ar dd mdd mstd meff msdd mseff) = 
        Attack s (fmap f ar) (fmap (f <$>) dd) ((f <$>) <$> mdd) ((f <$>) <$> mstd) (((f <$>) <$>) <$> meff) ((f <$>) <$> msdd) (((f <$>) <$>) <$> mseff)

instance Applicative (Attack b) where
    pure a = 
        Attack (AInfo "Attack" Melee 0 Bludgeoning Reduc Nothing Defense (C 20 1 0) Nothing (Al Neutral NeutraL) False (False,False)) 
        (pure a) (pure $ pure a) (pure $ pure a) (pure $ pure a) (pure $ pure $ pure a) (pure $ pure a) (pure $ pure $ pure a)
    (<*>) (Attack s f1 f2 f3 f4 f5 f6 f8) (Attack _ ar dd mdd mstd meff msdd mseff) = 
        Attack s (f1 <*> ar) ((<*>) <$> f2 <*> dd) (zipWith (<*>) f3 mdd) ((<*>) <$> f4 <*> mstd) (((<*>) <$> (((<*>) <$>) <$> f5)) <*> meff) ((<*>) <$> f6 <*> msdd) 
        (zipWith ((<*>) . ((<*>) <$>)) f8 mseff)

instance Foldable (Attack b) where
    foldMap f (Attack _ ar dd mdd mstd meff msdd mseff) = 
        (foldMap f ar) <> (foldMap (foldMap f) dd) <> (foldMap (foldMap f) mdd) <> (foldMap (foldMap f) mstd) <> (foldMap (foldMap (foldMap f)) meff) <> (foldMap (foldMap f) msdd)  
        <> (foldMap (foldMap (foldMap f)) mseff)

instance Traversable (Attack b) where 
    traverse f (Attack s ar dd mdd mstd meff msdd mseff) = 
        (Attack s) <$> (sequenceA $ fmap f ar) <*> (sequenceA $ (sequenceA . (fmap f)) <$> dd) <*> (sequenceA $ (sequenceA . (fmap f)) <$> mdd) <*> (sequenceA $ (sequenceA . (fmap f)) <$> mstd)
        <*> (sequenceA $ (sequenceA . (sequenceA <$>) . (fmap (f <$>))) <$> meff) <*> (sequenceA $ (sequenceA . (fmap f)) <$> msdd) <*> (sequenceA $ (sequenceA . (sequenceA <$>) . (fmap (f <$>))) <$> mseff)

instance Monad (Attack b) where
    return a = pure a
    (>>=) (Attack s ar dd mdd mstd meff msdd mseff) f = 
      Attack s (ar >>= (rolls . f)) (goo3 >>= goo4) (go3 >>= go4) (go3' >>= go4') (go3'' >>= go4'') (go3''' >>= go4''') (go3''''' >>= go4''''')
       where
        goo = (fmap damagedice dd)
        goo1 = (ddice . f)
        goo2 = (goo >>= goo1)
        goo3 = damagedice <$> goo2
        goo4 b = (set (#damagedice) b) <$> dd
        go = (fmap damagedice mdd)
        go1 = (mddice . f)
        go2 = (go >>= go1)
        go3 = damagedice <$> go2
        go4 b = (set (#damagedice) b) <$> mdd
        go' = view (#dice) <$> mstd
        go1' = (mstatdamage . f)
        go2' = (go' >>= go1')
        go3' = view (#dice) <$> go2'
        go4' b = (set (#dice) b) <$> mstd
        go'' = ((viewtempmodifyier <$>)) (view (#temporal) <$> meff)
        go1'' = (meffect . f)
        go2'' = (go'' >>= go1'')
        go3'' = (viewtempmodifyier . (view (#temporal))) <$> go2''
        go4'' b = (over (#temporal) (setmodifyer b)) <$> meff
        go''' = (fmap damagedice msdd)
        go1''' = (msingddice . f)
        go2''' = (go''' >>= go1''')
        go3''' = damagedice <$> go2'''
        go4''' b = (set (#damagedice) b) <$> msdd
        go''''' = (viewtempmodifyier <$>) (view (#temporal) <$> mseff)
        go1''''' = (msingeffect . f)
        go2''''' = (go''''' >>= go1''''')
        go3''''' = (viewtempmodifyier . (view (#temporal))) <$> go2'''''
        go4''''' b = (over (#temporal) (setmodifyer b)) <$> mseff

data AttackRoll a = AttackRoll { tohitroll :: a
                             , hitpercentage :: a
                             , criticalpercentage :: a
                             , mIpercentage :: a
                             , stealthroll :: a
                             , effectroll :: a
                             , statdamageroll :: a
                             , msddroll :: a} deriving (Show, Eq, Generic, Ord)

instance Functor AttackRoll where
    fmap f (AttackRoll a1 a2 a3 a4 a5 a6 a7 a8) = AttackRoll (f a1) (f a2) (f a3) (f a4) (f a5) (f a6) (f a7) (f a8)

instance Applicative AttackRoll where
    pure a = AttackRoll a a a a a a a a
    (<*>) (AttackRoll f1 f2 f3 f4 f5 f6 f7 f8) (AttackRoll a1 a2 a3 a4 a5 a6 a7 a8) = AttackRoll (f1 a1) (f2 a2) (f3 a3) (f4 a4) (f5 a5) (f6 a6) (f7 a7) (f8 a8)

instance Foldable AttackRoll where
    foldMap f (AttackRoll a1 a2 a3 a4 a5 a6 a7 a8 ) = (f a1) <> (f a2) <> (f a3) <> (f a4) <> (f a5) <> (f a6) <> (f a7) <> (f a8)

instance Traversable AttackRoll where
    traverse f (AttackRoll a1 a2 a3 a4 a5 a6 a7 a8) = AttackRoll <$> f a1 <*> f a2 <*> f a3 <*> f a4 <*> f a5 <*> f a6 <*> f a7 <*> f a8

instance Monad AttackRoll where
    (>>=) (AttackRoll a1 a3 a4 a5 a6 a7 a8 a9) f = 
        AttackRoll ((tohitroll . f) a1) ((hitpercentage . f) a3) ((criticalpercentage . f) a4) 
        ((mIpercentage . f) a5) ((stealthroll . f) a6) ((effectroll . f) a7) ((msddroll . f) a8) ((statdamageroll . f) a9)

shortattack :: Attack a b -> String
shortattack a = view (#info . #attackname) a