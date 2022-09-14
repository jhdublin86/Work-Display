{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module DND.Time
where

import Data.Generics.Labels ()
import GHC.Generics (Generic)

data Time a = Time { rounds :: a
                   , minutes :: a
                   , hours :: a
                   , days :: a } deriving (Show, Ord, Eq, Generic)

addrounds :: Int -> Time Int -> Time Int
addrounds int (Time r m h d) = (Time newr newm newh newd)
        where
            (summ, newr) = quotRem (r + int) 10
            (sumh, newm) = quotRem (m + summ) 60
            (sumd, newh) = quotRem (h + sumh) 24
            newd = d + sumd

instance Functor Time where
    fmap f (Time r m h d) = Time (f r) (f m) (f h) (f d)

instance Applicative Time where
    pure a = Time a a a a
    (<*>) (Time f1 f2 f3 f4) (Time a1 a2 a3 a4) = Time (f1 a1) (f2 a2) (f3 a3) (f4 a4)
