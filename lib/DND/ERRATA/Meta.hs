{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

module DND.Meta
where

import Data.Generics.Labels ()
import GHC.Generics (Generic)

data Meta f g a = M { runM :: f (g a)} deriving (Generic, Show, Eq)

instance (Functor f, Functor g) => Functor (Meta f g) where
    fmap f (M a) = M $ (f <$>) <$> a

instance (Applicative f, Applicative g) => Applicative (Meta f g) where
    pure a = M (pure $ pure a)
    (<*>) (M f) (M a) = M $ (<*>) <$> f <*> a

instance (Foldable f, Foldable g) => Foldable (Meta f g) where
    foldMap f (M a) = (foldMap (foldMap f) a)

instance (Traversable f, Traversable g) => Traversable (Meta f g) where
    traverse f (M a) = M <$> (sequenceA $ (sequenceA . (fmap f)) <$> a)