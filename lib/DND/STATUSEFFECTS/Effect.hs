{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DND.STATUSEFFECTS.Effect
where

import Data.Generics.Labels ()
import GHC.Generics (Generic)

import DND.DEFENSE.Defense (Target (Target), Defense (Defense, CMD))

import DND.STATUSEFFECTS.Effects (StatusEffect (Undead))

data Effect a = Effect {statuseffect :: StatusEffect, temporal :: a, temporalmodifierbonus :: Maybe Int, efftarget :: Maybe Target} deriving (Generic, Show, Eq, Ord)

instance Functor Effect where
  fmap f (Effect se temp tmb tar) = Effect se (f temp) tmb tar

instance Applicative Effect where
  pure a = Effect Undead a Nothing Nothing
  (<*>) (Effect fa fb fc fd) (Effect _ b _ _)
    {-| fc == Nothing = Effect fa (fb b) c
    | otherwise -}= Effect fa (fb b) fc fd

instance Monad Effect where
  return = pure
  (>>=) (Effect a b c d) f = Effect a ((temporal . f) b) c d

instance Foldable Effect where
  foldMap f (Effect _ a _ _) = f a

instance Traversable Effect where
  traverse f (Effect a b c d) = (\x -> (Effect a x c d)) <$> (f b)

testeffect :: Effect Int
testeffect = Effect Undead 0 Nothing (Just $ Target CMD 0)

testeffect2 :: Effect Int
testeffect2 = Effect Undead 0 Nothing Nothing

testeffect3 :: Effect Int
testeffect3 = Effect Undead 0 Nothing (Just $ Target Defense 0)

cmdeffect :: Int -> Effect a -> Effect a
cmdeffect b (Effect se temp tmb mtarg) = case mtarg of
  Nothing -> eff
  (Just (Target CMD a)) -> Effect se temp tmb (Just (Target CMD (a + b)))
  _ -> eff
  where
    eff = (Effect se temp tmb mtarg)