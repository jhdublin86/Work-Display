{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DND.DAMAGEDICE.DamageDice
where

import Data.Generics.Labels ()
import GHC.Generics (Generic)
import Control.Applicative (liftA2)

import DND.DAMAGEDICE.Elemental (Elemental)

import DND.DEFENSE.Defense (Target (Target), Defense (Defense) )

data SaveProfile = Negates | Half | Special {damage :: Int} deriving (Show, Eq, Generic, Ord)

data DamageDice a = D { damagedice :: a
                      , damagebonus :: Int
                      , elemental :: Maybe Elemental
                      , ddtarget :: Maybe Target
                      , saveprofile :: SaveProfile}
                      deriving (Ord, Show, Eq, Generic)

getalterdamage :: DamageDice a -> Int
getalterdamage (D _ _ _ _ e) = case e of
    Special y -> y
    _ -> 0

defaultDamageDice :: DamageDice [Int]
defaultDamageDice = D [8] 0 Nothing (Just $ Target Defense 0) Negates

instance Functor DamageDice where
    fmap f (D a b c d e) = D (f a) b c d e

instance Applicative DamageDice where
    pure a = D a 0 Nothing Nothing Negates
    (<*>) (D f fb fc fd fe) (D a _ _ _ _)
        {-| (fc == Nothing) && (fd == Nothing) = D (f a) fb c d
        | (fc == Nothing) = D (f a) fb c fd
        | (fd == Nothing) = D (f a) fb fc d
        | otherwise -}= D (f a) fb fc fd fe
        
instance Num a => Num (DamageDice a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    abs = fmap abs
    negate = fmap negate
    signum = fmap signum
    fromInteger = pure . fromInteger

instance Foldable DamageDice where
    foldMap f (D a _ _ _ _) = f a

instance Traversable DamageDice where
    traverse f (D a b c d e) = (\x -> (D x b c d e)) <$> (f a)

instance Monad DamageDice where
    return = pure
    (>>=) (D a b c d e) f = D (damagedice $ f a) b c d e
