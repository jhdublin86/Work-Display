{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

module DND.Action
where

import Data.Generics.Labels ()
import GHC.Generics (Generic)

import DND.Roll (Dice, Roll, sumdice, (~>))

data Action' f g h a = Action' { runFA :: f a
                                 , runGA :: g a
                                 , runHA :: h a} deriving (Show, Eq, Generic)

instance (Functor f, Functor g, Functor h) => Functor (Action' f g h) where
    fmap f (Action' g h i) = Action' (f <$> g) (f <$> h) (f <$> i)

instance (Applicative f, Applicative g, Applicative h) => Applicative (Action' f g h) where
    pure a = Action' (pure a) (pure a) (pure a)
    (<*>) (Action' f1 f2 f3) (Action' a1 a2 a3) = Action' (f1 <*> a1) (f2 <*> a2) (f3 <*> a3)

instance (Foldable f, Foldable g, Foldable h) => Foldable (Action' f g h) where
    foldMap f (Action' a1 a2 a3) = (foldMap f a1) <> (foldMap f a2) <> (foldMap f a3)

instance (Traversable f, Traversable g, Traversable h) => Traversable (Action' f g h) where
    traverse f (Action' a1 a2 a3) = Action' <$> (sequenceA $ f <$> a1) <*> (sequenceA $ f <$> a2) <*> (sequenceA $ f <$> a3)

instance (Monad f, Monad g, Monad h) => Monad (Action' f g h) where
    return a = pure a
    (>>=) (Action' a1 a2 a3) f = Action' (a1 >>= (runFA . f)) (a2 >>= (runGA . f)) (a3 >>= (runHA . f))

action' :: (Monad f, Traversable f, Monad g, Traversable g, Monad h, Traversable h)  => (Action' f g h [Int]) -> Dice (Action' f g h Roll)
action' a = do
    b <- sumdice ~> a
    return b

data Action f a = Action { runF :: f a} deriving (Show, Eq, Generic)

instance (Functor f) => Functor (Action f) where
    fmap f (Action g) = Action (f <$> g)

instance (Applicative f) => Applicative (Action f) where
    pure a = Action (pure a)
    (<*>) (Action f1) (Action a1) = Action (f1 <*> a1)

instance (Foldable f) => Foldable (Action f) where
    foldMap f (Action a1) = (foldMap f a1)

instance (Traversable f) => Traversable (Action f) where
    traverse f (Action a1) = Action <$> (sequenceA $ f <$> a1)

instance (Monad f) => Monad (Action f) where
    return a = pure a
    (>>=) (Action a1) f = Action (a1 >>= (runF . f))

action :: (Monad f, Traversable f)  => (Action f [Int]) -> Dice (Action f Roll)
action a = do
    b <- sumdice ~> a
    return b

action'' :: (Monad f, Traversable f)  => (f [Int]) -> Dice (f Roll)
action'' a = do
    b <- sumdice ~> a
    return b