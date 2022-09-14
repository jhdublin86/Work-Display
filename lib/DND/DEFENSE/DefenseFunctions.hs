{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

module DND.DEFENSE.DefenseFunctions
where

import Data.Generics.Labels ()
import Control.Lens (view)
import Data.Maybe (isJust, fromJust)

import DND.ATTACKS.Smite (Race (Halfling, Construct))

import DND.CHARACTER.AbilityScores (charisma, dexterity, constitution, wisdom, strength)
import DND.CHARACTER.Bonuses (Bonuses (maxdex, miscclass, defense))
import DND.CHARACTER.Class (ClassInfo (ref, fort, wil, bab))
import DND.CHARACTER.Status (Status (primclass, bonuses, equipedarmour), Size (Tiny, Small, Medium, Large, Huge, Gargantuan, Diminutive), abilitybonuses, size, gatherbonuses, limitdexterity)

import DND.DEFENSE.Armour (NonArmour (saves, attacks), Armour)
import DND.DEFENSE.Defense (Defense (Reflex, Will, Fortitude, Defense, CMD, Touch, Flatfooted, Perception), Target (Target))

import DND.STATUSEFFECTS.Effects ((&), isActive)

import DND.TemporaryValue (temporary)

attackdefense :: Armour Int -> Int
attackdefense armour = view (#physical) armour

touchdefense :: Armour Int -> Int
touchdefense armour = view (#energy) armour

maybeint2int :: Maybe Int -> Int
maybeint2int mi
  | isJust mi = fromJust mi
  | otherwise = 0

i2md :: Maybe Int -> Maybe Int -> Maybe Target
i2md Nothing _ = Nothing
i2md (Just int4) int5
    | 1 <= int4 && int4 <= 6 = Just $ Target (toEnum (int4 - 1)) (maybeint2int int5)
    | otherwise = Nothing

sized :: Size -> Int
sized Diminutive = 3
sized Tiny = 2
sized Small = 1
sized Medium = 0
sized Large = (-1)
sized Huge = (-2)
sized Gargantuan = (-3)

normaldefense :: Status -> Maybe Int
normaldefense s = (pure $ (attackdefense . equipedarmour) s) & (pure $ limitdexterity s) & (pure $ (sized . size) s) & (Just 10) 
    & (pure $ (attacks . ((temporary . sum) <$>) . defense . gatherbonuses) s) 
        where
        dragonform = (isActive . temporary) (view (#effects . #formofthedragon) s)
        elementalformiii = ((isActive . temporary) (view (#effects . #elementalbodyiii) s))
        elementalformiv = ((isActive . temporary) (view (#effects . #elementalbodyiv) s))
        beastform = ((isActive . temporary) (view (#effects . #beastshape) s))       
        {-go3
            | view (#playerclasses . #mon) s >= 1 =  Just $ (view (#playerclasses . #mon) s `div` 4) + (view (#wisdom) $ abilitybonuses s)
            | otherwise = Nothing-}

flatfooted :: Status -> Maybe Int
flatfooted s = (pure $ (attackdefense . equipedarmour) s) & (pure $ (sized . size) s) & (Just 10)
    & (pure $ (attacks . ((temporary . sum) <$>) . defense . gatherbonuses) s) 
        where
        dragonform = (isActive . temporary) (view (#effects . #formofthedragon) s)
        elementalformiii = ((isActive . temporary) (view (#effects . #elementalbodyiii) s))
        elementalformiv = ((isActive . temporary) (view (#effects . #elementalbodyiv) s))
        beastform = ((isActive . temporary) (view (#effects . #beastshape) s))         
        {-go3
            | view (#playerclasses . #mon) s >= 1 =  Just $ (view (#playerclasses . #mon) s `div` 4) + (view (#wisdom) $ abilitybonuses s)
            | otherwise = Nothing-}

touch :: Status -> Maybe Int
touch s = (pure $ (touchdefense . equipedarmour) s) & (pure $ limitdexterity s) & (pure $ (sized . size) s) & (Just 10)
    & (pure $ (attacks . ((temporary . sum) <$>) . defense . gatherbonuses) s) 
        where
        dragonform = (isActive . temporary) (view (#effects . #formofthedragon) s)
        elementalformiii = ((isActive . temporary) (view (#effects . #elementalbodyiii) s))
        elementalformiv = ((isActive . temporary) (view (#effects . #elementalbodyiv) s))
        beastform = ((isActive . temporary) (view (#effects . #beastshape) s))         
        {-go3
            | view (#playerclasses . #mon) s >= 1 =  Just $ (view (#playerclasses . #mon) s `div` 4) + (view (#wisdom) $ abilitybonuses s)
            | otherwise = Nothing-}

fftouch :: Status -> Maybe Int
fftouch s = (pure $ (touchdefense . equipedarmour) s) & (pure $ (sized . size) s) & (Just 10)
    & (pure $ (attacks . ((temporary . sum) <$>) . defense . gatherbonuses) s) 
        where
        dragonform = (isActive . temporary) (view (#effects . #formofthedragon) s)
        elementalformiii = ((isActive . temporary) (view (#effects . #elementalbodyiii) s))
        elementalformiv = ((isActive . temporary) (view (#effects . #elementalbodyiv) s))
        beastform = ((isActive . temporary) (view (#effects . #beastshape) s))      
        {-go3
            | view (#playerclasses . #mon) s >= 1 =  Just $ (view (#playerclasses . #mon) s `div` 4) + (view (#wisdom) $ abilitybonuses s)
            | otherwise = Nothing-}

reflex :: Status -> Maybe Int
reflex s = (pure $ (temporary . ref . primclass) s) & (pure $ limitdexterity s) & (pure $ (temporary . sum . ref . miscclass . gatherbonuses) s) 
    & (Just 10) & (pure $ (temporary . sum . saves . defense . gatherbonuses) s) & go3 & fearpenalty
        where
        dragonform = (isActive . temporary) (view (#effects . #formofthedragon) s)
        elementalformiii = ((isActive . temporary) (view (#effects . #elementalbodyiii) s))
        elementalformiv = ((isActive . temporary) (view (#effects . #elementalbodyiv) s))
        beastform = ((isActive . temporary) (view (#effects . #beastshape) s))         
        {-go2
            | view (#playerclasses . #pala) s >= 3 = pure $ (charisma . abilitybonuses) s
            | otherwise = Nothing-}
        go3 
            | temporary (view (#race) s) == Halfling = Just 1
            | otherwise = Nothing
        fearpenalty
            | (isActive . temporary) (view (#effects . #fear) s) = Just (-4)
            | (isActive . temporary) (view (#effects . #shaken) s) = Just (-2)
            | otherwise = Nothing

ffreflex :: Status -> Maybe Int
ffreflex s = (pure $ (temporary . ref . primclass) s) & (pure $ (temporary . sum . ref . miscclass . gatherbonuses) s) 
    & (Just 10) & (pure $ (temporary . sum . saves . defense . gatherbonuses) s) & go3 & fearpenalty
        where
        dragonform = (isActive . temporary) (view (#effects . #formofthedragon) s)
        elementalformiii = ((isActive . temporary) (view (#effects . #elementalbodyiii) s))
        elementalformiv = ((isActive . temporary) (view (#effects . #elementalbodyiv) s))
        beastform = ((isActive . temporary) (view (#effects . #beastshape) s))
        {-go2
            | view (#playerclasses . #pala) s >= 3 = pure $ (charisma . abilitybonuses) s
            | otherwise = Nothing-}
        go3 
            | temporary (view (#race) s) == Halfling = Just 1
            | otherwise = Nothing
        fearpenalty
            | (isActive . temporary) (view (#effects . #fear) s) = Just (-4)
            | (isActive . temporary) (view (#effects . #shaken) s) = Just (-2)
            | otherwise = Nothing

fortitude :: Status -> Maybe Int
fortitude s = (pure $ (temporary . fort . primclass) s) & zerocon & (pure $ (temporary . sum . fort . miscclass . gatherbonuses) s) & 
    (Just 10) & (pure $ (temporary . sum . saves . defense . gatherbonuses) s) & go3 & fearpenalty
        where
        dragonform = (isActive . temporary) (view (#effects . #formofthedragon) s)
        elementalformiii = ((isActive . temporary) (view (#effects . #elementalbodyiii) s))
        elementalformiv = ((isActive . temporary) (view (#effects . #elementalbodyiv) s))
        beastform = ((isActive . temporary) (view (#effects . #beastshape) s))      
        {-go2
            | view (#playerclasses . #pala) s >= 3 = pure $ (charisma . abilitybonuses) s
            | otherwise = Nothing-}
        go3 
            | temporary (view (#race) s) == Halfling = Just 1
            | otherwise = Nothing
        fearpenalty
            | (isActive . temporary) (view (#effects . #fear) s) = Just (-4)
            | (isActive . temporary) (view (#effects . #shaken) s) = Just (-2)
            | otherwise = Nothing
        zerocon 
            | (isActive . temporary $ view (#effects . #undead) s) || ((Construct ==) . temporary $ view (#race) s) = Nothing
            | otherwise = (pure $ (constitution . abilitybonuses) s)

                     
will :: Status -> Maybe Int
will s = (pure $ (temporary . wil . primclass) s) & (pure $ (wisdom . abilitybonuses) s) & (pure $ (temporary . sum . wil . miscclass . gatherbonuses) s) & 
    (Just 10) & (pure $ (temporary . sum . saves . defense . gatherbonuses) s) & go3 & fearpenalty
        where
        dragonform = (isActive . temporary) (view (#effects . #formofthedragon) s)
        elementalformiii = ((isActive . temporary) (view (#effects . #elementalbodyiii) s))
        elementalformiv = ((isActive . temporary) (view (#effects . #elementalbodyiv) s))
        beastform = ((isActive . temporary) (view (#effects . #beastshape) s))         
        {-go2
            | view (#playerclasses . #pala) s >= 3 = pure $ (charisma . abilitybonuses) s
            | otherwise = Nothing-}
        go3 
            | temporary (view (#race) s) == Halfling = Just 1
            | otherwise = Nothing
        fearpenalty
            | (isActive . temporary) (view (#effects . #fear) s) = Just (-4)
            | (isActive . temporary) (view (#effects . #shaken) s) = Just (-2)
            | otherwise = Nothing

cmd :: Status -> Maybe Int
cmd s = (pure . negate) (sized $ size s) & (pure $ (strength . abilitybonuses) s) & (pure $ (dexterity . abilitybonuses) s) & babbonus & miscbonus & (Just 10) 
    where
        babbonus = (pure $ (temporary . bab . primclass) s)
        miscbonus = (pure $ (temporary . sum . bab . miscclass . gatherbonuses) s)
        {-go3
            | view (#playerclasses . #mon) s >= 1 =  Just $ (view (#playerclasses . #mon) s `div` 4) + (view (#wisdom) $ abilitybonuses s)
            | otherwise = Nothing-}

ffcmd :: Status -> Maybe Int
ffcmd s = (pure . negate) (sized $ size s) & (pure $ (strength . abilitybonuses) s) & babbonus & miscbonus & (Just 10) 
    where
        babbonus = (pure $ (temporary . bab . primclass) s)
        miscbonus = (pure $ (temporary . sum . bab . miscclass . gatherbonuses) s)
        {-go3
            | view (#playerclasses . #mon) s >= 1 =  Just $ (view (#playerclasses . #mon) s `div` 4) + (view (#wisdom) $ abilitybonuses s)
            | otherwise = Nothing-}


perception :: Status -> Maybe Int
perception s = (Just 10) & (pure . temporary . sum . view (#skills . #perception) . gatherbonuses $ s) & (pure $ (wisdom . abilitybonuses) s) & (pure $ view (#skills . #perception) s)

defense2defensetype :: Defense -> (Status -> Maybe Int)
defense2defensetype a
        | a == Defense = normaldefense
        | a == Flatfooted = flatfooted
        | a == Touch = touch
        | a == Reflex = reflex
        | a == Fortitude = fortitude
        | a == Will = will
        | a == Perception = perception
        | otherwise = cmd

ffdefensetype :: Defense -> Status -> Maybe Int
ffdefensetype a status
        | uncannybool && a == Defense = flatfooted status
        | uncannybool && a == Touch = fftouch status
        | uncannybool && a == CMD = ffcmd status
        | uncannybool && a == Reflex = ffreflex status
        | otherwise = defense2defensetype a status
        where
            uncannybool = not . isActive . temporary $ view (#effects . #uncannydodge) status
ffdefensetype' :: Defense -> Status -> Defense
ffdefensetype' a status
        | uncannybool && a == Defense = Flatfooted
        | uncannybool && a == Touch = Flatfooted
        | uncannybool && a == CMD = Flatfooted
        | uncannybool && a == Reflex = Flatfooted
        | otherwise = a
        where
            uncannybool = not . isActive . temporary $ view (#effects . #uncannydodge) status