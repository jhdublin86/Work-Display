{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

module DND.DEFENSE.Defense
where

import Data.Generics.Labels ()
import GHC.Generics (Generic)


data Defense = Defense | Flatfooted | Touch | Reflex | Fortitude | Will | CMD | Perception deriving (Show, Eq, Generic, Ord, Enum, Bounded)

data Target = Target { defensetype :: Defense, bonus :: Int} deriving (Show, Eq, Generic, Ord)