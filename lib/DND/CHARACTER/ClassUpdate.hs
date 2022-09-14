{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module DND.CHARACTER.ClassUpdate
where

import Control.Lens (Lens', view, set, over)
import Control.Lens.Indexed (imap)
import Control.Applicative (liftA2)
import Data.List.NonEmpty (NonEmpty ((:|)))

import DND.ATTACKS.DamageReduction (Material (Norm, ChaoS), dredux)
import DND.ATTACKS.Smite (Race (Outsider))
import DND.ATTACKS.Weapon (Weapon (Weapon), elemental2enchantment)

import DND.CHARACTER.AbilityScores (constitution, charisma)
import DND.CHARACTER.Class (ClassInfo, collapseclasses)
import DND.CHARACTER.Status (Status, abilitybonuses, SpellTemplate, gatherbonuses, effectivespelllevel)

import DND.DAMAGEDICE.DamageDice (DamageDice (D), SaveProfile (Negates, Half))
import DND.DAMAGEDICE.Elemental (Elemental (Acid, Earth, Fire, Wind, Positive, Negative, Ice, Lightning), Profile (NormalDamage, Immune), ElementalResistance)

import DND.DEFENSE.Defense (Defense (Touch, Reflex), Target (Target))

import DND.SPELLS.Spell (Spell (Spell), SpellInfo (SpellInfo), SpellSchool (Evocation, Abjuration, Enchantment, Necromancy), emptyspellroll, 
    SpellTarget (SingleTarget, AllInRange), SpellArea (Sphere, Cone))
import DND.SPELLS.SpellFunctions (close)

import DND.STATUSEFFECTS.Effect (Effect (Effect))

import DND.TemporaryValue (Temporary, temporary, addtemporary, revert)

bonusSpells :: Int -> Int -> Int
bonusSpells 0 _ = 0
bonusSpells spelllevel i = max 0 $ (((i * 2) + 10) - (2 + (2 * spelllevel))) `div` 8

elementalIndex :: Elemental -> Lens' (ElementalResistance a) a
elementalIndex Fire = #fire
elementalIndex Earth = #earth
elementalIndex Ice = #ice
elementalIndex Wind = #wind
elementalIndex Lightning = #lightning
elementalIndex Acid = #acid
elementalIndex Positive = #positive
elementalIndex Negative = #negative

classify :: Status -> ClassInfo (Temporary Int)
classify s = collapseclasses $ (pure <$>) . view (#playerclasses) $ s

classinfoupdate :: Status -> Status
classinfoupdate s = over (#primclass) (go1 . go newinfo) s
    where
        newinfo = classify s
        oldinfo = view (#primclass) s
        go :: ClassInfo (Temporary Int) -> ClassInfo (Temporary Int) -> ClassInfo (Temporary Int)
        go ci1 ci2 = go1 . fmap pure $ liftA2 (+) (fmap temporary ci1) (fmap temporary ci2)
        go1 :: ClassInfo (Temporary Int) -> ClassInfo (Temporary Int)
        go1 ci = set (#level . #temporaries) (0 :| []) $
                 set (#level . #permanent) (view (#level . #permanent) newinfo + view (#level . #permanent) oldinfo) ci 

classspells :: Int -> Int -> Int
classspells 0 _ = 0
classspells classlevel spelllevel
    | spelllevel >= 7 = 0
    | spelllevel == 0 = 100
    | classlevel == 20 && spelllevel == 6 = 5
    | classlevel >= 19 && spelllevel == 5 = 5
    | classlevel >= (9 + (3 * (spelllevel - 1))) = 5
    | classlevel >= (5 + (3 * (spelllevel - 1))) = 4
    | classlevel >= (3 + (3 * (spelllevel - 1))) = 3
    | classlevel >= (1 + (3 * (spelllevel - 1))) = 2
    | otherwise = 0

bonusspells :: (Int -> Int -> Int) -> Int -> Int -> Int -> Int
bonusspells a level spelllevel abilitybonus
    | a level spelllevel > 0 = bonusSpells spelllevel abilitybonus
    | otherwise = 0

allmysticspells :: Status -> Status
allmysticspells s = over (#classfeatures . #mysticspells . #perday) (imap (\x y -> y + (bonusspells classspells effectivelevel x (view (#wisdom) $ abilitybonuses s)))) $
    over (#classfeatures . #mysticspells . #perday) (imap (\x y -> (classspells effectivelevel x) + y)) s
            where
                effectivelevel = view (#playerclasses . #myst) s

allarcanespells :: Status -> Status
allarcanespells s = over (#classfeatures . #arcanespells . #perday) (imap (\x y -> y + (bonusspells classspells effectivelevel x (view (#intelligence) $ abilitybonuses s)))) $
    over (#classfeatures . #arcanespells . #perday) (imap (\x y -> (classspells effectivelevel x) + y)) s
            where
                effectivelevel = view (#playerclasses . #techno) s
