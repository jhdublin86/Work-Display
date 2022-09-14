{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module DND.CHARACTER.ClassUpdate2
where

import Control.Lens (view, set, over)
import Control.Lens.Indexed (imap)

import DND.ATTACKS.DamageReduction (Material (EviL), dredux)

import DND.CHARACTER.ClassUpdate
import DND.CHARACTER.Status (Status, abilitybonuses, SpellTemplate, abilitybonuses, turnundead, gatherbonuses)

import DND.DAMAGEDICE.DamageDice (DamageDice (D), SaveProfile (Negates, Half))
import DND.DAMAGEDICE.Elemental (Elemental (Acid, Positive, Negative), Profile (NormalDamage))

import DND.DEFENSE.Defense (Defense (Touch, Will), Target (Target))

import DND.SPELLS.Spell (Spell (Spell), SpellInfo (SpellInfo), SpellSchool (Evocation, Abjuration, Enchantment, Transmutation), emptyspellroll, 
    SpellTarget (Caster, SingleTarget, AllInRange), SpellArea (Bursty))
import DND.SPELLS.SpellFunctions (close)

import DND.STATUSEFFECTS.Effect (Effect (Effect))
import DND.STATUSEFFECTS.Effects (Temporal (Present, On))

import DND.TemporaryValue (temporary, addtemporary)

classupdate :: Status -> Status
classupdate = classinfoupdate . featupdate . allmysticspells . allarcanespells

deadlyaim :: Status -> Status
deadlyaim s
    | view (#classfeatures . #feats . #deadlyaim) s = set (#effects . #deadlyaim) (pure $ On Nothing [] Nothing) s
    | otherwise = s

weaponfinesse :: Status -> Status
weaponfinesse s
    | view (#classfeatures . #feats . #weaponfinesse) s = set (#effects . #weaponfinesse) (pure $ Present Nothing [] Nothing) s
    | otherwise = s

arcanestrike :: Status -> Status
arcanestrike s
    | view (#classfeatures . #feats . #arcanestrike) s = set (#effects . #arcanestrike) (pure $ Present Nothing [] Nothing) s
    | otherwise = s

improvedgrapple :: Status -> Status
improvedgrapple s
    | view (#classfeatures . #feats . #improvedgrapple) s = set (#effects . #improvedgrapple) (pure $ Present Nothing [] Nothing) s
    | otherwise = s

improveddisarm :: Status -> Status
improveddisarm s
    | view (#classfeatures . #feats . #improveddisarm) s = set (#effects . #improveddisarm) (pure $ Present Nothing [] Nothing) s
    | otherwise = s

improvedtrip :: Status -> Status
improvedtrip s
    | view (#classfeatures . #feats . #improvedtrip) s = set (#effects . #improvedtrip) (pure $ Present Nothing [] Nothing) s
    | otherwise = s

turnundeadfeat :: Status -> Status
turnundeadfeat s = s

featupdate :: Status -> Status
featupdate = deadlyaim . weaponfinesse . arcanestrike . turnundeadfeat .
    improvedgrapple . improveddisarm . improvedtrip