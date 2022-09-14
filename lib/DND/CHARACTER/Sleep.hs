{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module DND.CHARACTER.Sleep
where

import Control.Lens (view, set, over)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import GHC.Float (int2Float)

import DND.CHARACTER.AbilityScores (constitution, charisma)
import DND.CHARACTER.ClassUpdate
import DND.CHARACTER.ClassUpdate2
import DND.CHARACTER.Status (Status, Hitpoints(HP), abilitybonuses, abilitybonuses, channelenergy', turnundead, damager', currenthitpoints, resolvepoints)

import DND.DAMAGEDICE.Elemental (Elemental (Fire))

import DND.SPELLS.SpellFunctions (darkheal)

import DND.STATUSEFFECTS.Effects (Temporal (Absent))

import DND.TemporaryValue (Temporary (T), revert)

sleep :: Status -> Status
sleep = mystsleep . arcsleep . over (#resolve) revert

clean :: Temporary (Temporal (Maybe Int))
clean = pure $ Absent Nothing [] Nothing

mystsleep :: Status -> Status
mystsleep s 
    | view (#playerclasses . #myst) s >= 1 = allmysticspells $ 
        set (#classfeatures . #mysticspells . #perday) (pure 0) s
    | otherwise = s

arcsleep :: Status -> Status
arcsleep s 
    | view (#playerclasses . #techno) s >= 1 = allarcanespells $ 
        set (#classfeatures . #arcanespells . #perday) (pure 0) s
    | otherwise = s

regainstamina :: Status -> Status
regainstamina s = if (int2Float curhp) / (int2Float maxhp) <= 0.5 && ((fst $ resolvepoints s) >= 1) then over (#damage) revert . damager' (#resolve) (Just 1) $ s else s
        where
          HP (T (curhp :| []) maxhp) (T (curvp :| []) maxvp) = currenthitpoints s