{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DND.DAMAGEDICE.DamageDiceFunctions
where

import Control.Lens (view, over, set)
import Control.Monad (join)
import Data.Maybe (fromJust)

import DND.DAMAGEDICE.DamageDice (DamageDice (D, ddtarget), SaveProfile (Half), getalterdamage)
import DND.DAMAGEDICE.Elemental (Elemental, Profile (Cures), ElementalResistance, elementalIndex, runA, elementalDamage, isJustImmune, isJustCures, eix, isJustVulnerable)

import DND.CHARACTER.Status (Status (elementalresistance), damager, healthupdate)

import DND.DEFENSE.Defense ( Defense (Reflex))
import DND.DEFENSE.DefenseFunctions (defense2defensetype, ffdefensetype)

import DND.STATUSEFFECTS.Effects ((&), isActive)

import DND.TemporaryValue (Temporary, temporary)

dDEP2MI :: DamageDice Int -> ElementalResistance (Temporary (Profile (Maybe Int))) -> Maybe Int 
dDEP2MI (D a b c _ _) er'
    | c == Nothing = Just $ a + b
    | otherwise = x
        where
            go :: Int -> ElementalResistance (Profile (Maybe Int)) -> Elemental -> Maybe Int
            go r p e = elementalDamage r (elementalIndex e p)
            x :: Maybe Int
            x
              | isJustCures z = (c >>= (go a er)) & ((pure . negate) b)
              | isJustImmune z = Nothing
              | isJustVulnerable z = (c >>= (go a er)) & ((pure . (2 *)) b)
              | otherwise = max (Just 0) ((c >>= (go a er)) & (join $ ((negate <$>) . runA) <$> z) & (pure b))
                where
                    z = (\y -> elementalIndex y er) <$> c
                    er = fmap temporary er'

mddstatus :: Int -> Int -> Maybe (DamageDice Int) -> Status -> Status
mddstatus _ _ Nothing s = s
mddstatus i i2 (Just dd) s
    | ddtarget dd == Nothing = fulldamageds
    | ddheals = fulldamageds
    | i2 == 1 = go
    | i2 == 20 = damageds
    | saveFails = damageds
    | otherwise = go
        where
            save = view (#saveprofile) dd
            melement = view (#elemental) dd
            ddheals
                | melement == Nothing = False
                | (temporary . (view (#elementalresistance . (eix (fromJust melement))))) s == Cures = True
                | otherwise = False
            fulldamageds = healthupdate $ damager (dDEP2MI dd $ elementalresistance s) s
            halfdamageds = healthupdate $ damager (fmap (\x -> x `div` 2) $ dDEP2MI dd (elementalresistance s)) s
            specialdamageds = healthupdate $ damager (dDEP2MI (set (#damagedice) (getalterdamage dd) $ set (#damagebonus) 0 dd) (elementalresistance s)) s
            saveFails = (pure $ i + i2 + (view (#bonus) (fromJust $ ddtarget dd))) > target
            damageds
                | (isActive . temporary) (view (#effects . #greaterEvasion) s) && (save == Half) && ((view (#defensetype) . fromJust . ddtarget) dd == Reflex) = halfdamageds
                | otherwise = fulldamageds
            go :: Status
            go
                | (isActive . temporary) (view (#effects . #evasion) s) && (save == Half) && ((view (#defensetype) . fromJust . ddtarget) dd == Reflex) = s
                | save == Half = halfdamageds
                | getalterdamage dd > 0 = specialdamageds
                | otherwise = s
            ishelpless = 
                (isActive . temporary) (view (#effects . #held) s) || ((isActive . temporary) (view (#effects . #paralyzed) s)) || ((isActive . temporary) (view (#effects . #sleep) s))
            target
                | ishelpless = ffdefensetype (view (#defensetype) (fromJust $ ddtarget dd)) $
                    (set (#abilityscores . #wisdom) (pure 10 :: Temporary Int) $ 
                    set (#abilityscores . #dexterity) (pure 0 :: Temporary Int) $ 
                    over (#bonuses . #abilityscores . #wisdom) (fmap (\_ -> pure 0 :: Temporary Int)) $
                    over (#bonuses . #abilityscores . #dexterity) (fmap (\_ -> pure 0 :: Temporary Int)) s)
                | (isActive . temporary) (view (#effects . #prone) s) = ffdefensetype (view (#defensetype) (fromJust $ ddtarget dd)) s
                | (isActive . temporary) (view (#effects . #stunned) s) = ffdefensetype (view (#defensetype) (fromJust $ ddtarget dd)) s
                | (isActive . temporary) (view (#effects . #grappled) s) = ffdefensetype (view (#defensetype) (fromJust $ ddtarget dd)) s
                | (isActive . temporary) (view (#effects . #laughter) s) = ffdefensetype (view (#defensetype) (fromJust $ ddtarget dd)) s   
                | (isActive . temporary) (view (#effects . #flatfooted) s) = ffdefensetype (view (#defensetype) (fromJust $ ddtarget dd)) s       
                | otherwise = defense2defensetype (view (#defensetype) (fromJust $ ddtarget dd)) s