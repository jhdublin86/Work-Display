{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module DND.DAMAGEDICE.Elemental
where

import Control.Lens (Lens')
import Data.Generics.Labels ()
import GHC.Generics (Generic)
import Control.Lens.Indexed
import Data.Traversable
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty ((:|)))

import DND.TemporaryValue (Temporary (T), addtemporary, removetemporary)

data Elemental = Fire | Earth | Ice | Wind | Lightning | Acid | Negative | Positive deriving (Generic, Show, Eq, Ord, Enum, Bounded)

data Profile a = NormalDamage {runA :: a} | Immune | Cures | Vulnerable deriving (Generic, Show, Eq, Ord)

instance Functor Profile where
    fmap f (NormalDamage a) = NormalDamage $ f a
    fmap _ (Immune) = Immune
    fmap _ (Cures) = Cures
    fmap _ (Vulnerable) = Vulnerable

instance Applicative Profile where
    pure a = NormalDamage a
    (<*>) Immune _ = Immune
    (<*>) Cures _ = Cures
    (<*>) Vulnerable _ = Vulnerable
    (<*>) (NormalDamage _) Cures = Cures
    (<*>) (NormalDamage _) Immune = Immune
    (<*>) (NormalDamage _) Vulnerable = Vulnerable
    (<*>) (NormalDamage f) (NormalDamage a) = NormalDamage $ f a

data ElementalResistance a = ER { fire :: a
                                , earth :: a
                                , ice :: a
                                , wind :: a
                                , lightning :: a
                                , acid :: a
                                , negative :: a
                                , positive :: a}
                                deriving (Generic, Show, Eq, Ord)

instance Functor ElementalResistance where
    fmap = fmapDefault

instance FunctorWithIndex Elemental ElementalResistance

instance Applicative ElementalResistance where
    pure a = ER a a a a a a a a
    (<*>) (ER f1 f2 f3 f4 f5 f6 f7 f8) (ER a1 a2 a3 a4 a5 a6 a7 a8) = ER (f1 a1) (f2 a2) (f3 a3) (f4 a4) (f5 a5) (f6 a6) (f7 a7) (f8 a8)

instance Foldable ElementalResistance where
    foldMap = foldMapDefault
    length _ = 8

instance FoldableWithIndex Elemental ElementalResistance

instance Traversable ElementalResistance where
    traverse = itraverse . const

instance TraversableWithIndex Elemental ElementalResistance where
    itraverse ƒ (ER a b c d e f g h) =
        ER <$> ƒ Fire a <*> ƒ Earth b <*> ƒ Ice c <*> ƒ Wind d <*> ƒ Lightning e <*> ƒ Acid f <*> ƒ Negative g <*> ƒ Positive h

eix :: Elemental -> Lens' (ElementalResistance a) a
eix Fire = #fire
eix Earth = #earth
eix Ice = #ice
eix Wind = #wind
eix Acid = #acid
eix Lightning = #lightning
eix Negative = #negative
eix Positive = #positive

i2me :: Maybe Int -> Maybe Elemental
i2me Nothing = Nothing
i2me (Just int4)
    | 1 <= int4 && int4 <= 8 = Just $ toEnum (int4 - 1)
    | otherwise = Nothing

isImmune :: Profile a -> Bool
isImmune (Immune) = True
isImmune _ = False

isJustImmune :: Maybe (Profile a) -> Bool
isJustImmune (Just (Immune)) = True
isJustImmune _ = False

isCures :: Profile a -> Bool
isCures (Cures) = True
isCures _ = False

isJustCures :: Maybe (Profile a) -> Bool
isJustCures (Just (Cures)) = True
isJustCures _ = False

isVulnerable :: Profile a -> Bool
isVulnerable (Vulnerable) = True
isVulnerable _ = False

isJustVulnerable :: Maybe (Profile a) -> Bool
isJustVulnerable (Just (Vulnerable)) = True
isJustVulnerable _ = False

elementalDamage :: Int -> Profile a -> Maybe Int
elementalDamage a b
        | isImmune b = Nothing
        | isCures b = Just (-a)
        | isVulnerable b = Just (2 * a)
        | otherwise = Just a

elementalIndex :: Elemental -> (ElementalResistance a -> a)
elementalIndex Fire = fire
elementalIndex Earth = earth
elementalIndex Ice = ice
elementalIndex Wind = wind
elementalIndex Positive = positive
elementalIndex Negative = negative
elementalIndex Acid = acid
elementalIndex Lightning = lightning

normalelementalresistance :: ElementalResistance (Temporary (Profile (Maybe Int)))
normalelementalresistance = ER (pure $ NormalDamage Nothing) (pure $ NormalDamage Nothing) (pure $ NormalDamage Nothing) (pure $ NormalDamage Nothing)
                                (pure $ NormalDamage Nothing) (pure $ NormalDamage Nothing) (pure $ NormalDamage Nothing) (pure $ Cures)

undeadelementalresistance :: ElementalResistance (Temporary (Profile (Maybe Int)))
undeadelementalresistance = ER (pure $ NormalDamage Nothing) (pure $ NormalDamage Nothing) (pure $ NormalDamage Nothing) (pure $ NormalDamage Nothing)
                                (pure $ NormalDamage Nothing) (pure $ NormalDamage Nothing) (pure $ Cures) (pure $ NormalDamage Nothing)

modifyprofile :: Ord a => Profile (Maybe a) -> (Temporary (Profile (Maybe a))) -> (Temporary (Profile (Maybe a)))
modifyprofile (NormalDamage Nothing) (T ( p :| ps) pp) = T ((NormalDamage Nothing) :| ((reverse . sort) $ p : ps)) pp
modifyprofile p' (T (p :| ps) pp) = T ((max p p') :| ((reverse . sort) $ (min p p') : ps)) pp

modifyprofile' :: Ord a => Profile (Maybe a) -> (Temporary (Profile (Maybe a))) -> (Temporary (Profile (Maybe a)))
modifyprofile' (NormalDamage Nothing) (T ( _ :| []) pp) = T ((NormalDamage Nothing) :| []) pp
modifyprofile' (NormalDamage Nothing) (T ( p :| ps) pp) = T (p :| ((reverse . ((NormalDamage Nothing) :) . (drop 1) . sort) ps)) pp
modifyprofile' p' t = addtemporary p' t

removeprofile' :: Ord a => Profile (Maybe a) -> (Temporary (Profile (Maybe a))) -> (Temporary (Profile (Maybe a)))
removeprofile' p' t = removetemporary p' t

removeprofile :: Ord a => Profile (Maybe a) -> (Temporary (Profile (Maybe a))) -> (Temporary (Profile (Maybe a)))
removeprofile p' (T (p :| []) pp)
    | p' >= p = T (pp :| []) pp
    | otherwise = (T (p :| []) pp)
removeprofile p' (T (p :| ps) pp)
    | p' >= p = T ( (head ps) :| (tail ps)) pp
    | p' > pp = T (p :| go) pp
    | otherwise = (T (p :| ps) pp)
        where
            go = takeWhile ((\x -> x > p')) ps ++ (drop 1 (dropWhile (\x -> x > p') ps))

