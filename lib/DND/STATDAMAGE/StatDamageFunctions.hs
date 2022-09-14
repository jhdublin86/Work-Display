{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}

module DND.STATDAMAGE.StatDamageFunctions
where

import Control.Lens (Lens', view, set, over)
import Data.Generics.Labels ()
import Data.Maybe (fromJust)
import System.Random
import Control.Monad.Trans.State.Strict (runState, state)

import DND.CHARACTER.AbilityScores (Ability (Strength, Dexterity, Constitution, Intelligence, Charisma, Wisdom))
import DND.CHARACTER.Class (updateClassInfo)
import DND.CHARACTER.Status (Status, damager', healthupdate)

import DND.DEFENSE.DefenseFunctions (defense2defensetype, ffdefensetype)

import DND.STATDAMAGE.StatDamage (StatDamage (SD), StatDamageType (Disease, Poisen, Magic))

import DND.STATUSEFFECTS.Effects (StatusEffect (Poisened), Temporal (Present), isActive)
import DND.STATUSEFFECTS.StatusEffectFunctions2 (t2ix)

import DND.Roll (runRoll, sumdice, Dice, (~>), roll)
import DND.TemporaryValue (Temporary, temporary)

ability2lens :: Ability -> Lens' Status (Temporary Int)
ability2lens Strength = #abilityscores . #strength
ability2lens Dexterity = #abilityscores . #dexterity
ability2lens Constitution = #abilityscores . #constitution
ability2lens Intelligence = #abilityscores . #intelligence
ability2lens Charisma = #abilityscores . #charisma
ability2lens Wisdom = #abilityscores . #wisdom

mability2lens :: Maybe Ability -> Lens' Status (Temporary Int)
mability2lens Nothing = (#primclass . #level)
mability2lens (Just abi) = ability2lens abi

sdtcheck :: Maybe StatDamageType -> Status -> Bool
sdtcheck msdt s
    | msdt == Nothing = True
    | msdt == (Just Magic) && ((not . isActive . temporary) (view (#effects . #immunetomagic) s)) = True
    | msdt == (Just Poisen) && ((not . isActive . temporary) (view (#effects . #immunetopoisen) s)) = True
    | msdt == (Just Disease) && ((not . isActive . temporary) (view (#effects . #immunetodisease) s)) = True
    | otherwise = False

statdamage :: Int -> Int -> Maybe (StatDamage Int) -> Status -> Status
statdamage _ _ Nothing s = s
statdamage int int2 (Just (SD msdt mtar mabil pdice diceroll dicebon durroll durationbon)) s
    | (isActive . temporary . view (#effects . #undead) $ s) = s
    | sdtcheck msdt s && mtar == Nothing = healthupdate $ t2ix Poisened (Present Nothing [] Nothing) $ over (#statdamage) (sdamforlist ++) $ over (#primclass) (updateClassInfo) $ damager' (mability2lens mabil) damageamount s
    | sdtcheck msdt s && hitstarget = healthupdate $ t2ix Poisened (Present Nothing [] Nothing) $ over (#statdamage) (sdamforlist ++) $ over (#primclass) (updateClassInfo) $ damager' (mability2lens mabil) damageamount s
    | otherwise = s
        where
            sdam = SD msdt mtar mabil pdice diceroll dicebon durroll durationbon
            sdamforlist
                | view (#duration) sdam == [0] && (view (#durationbonus) sdam == 0) = []
                | otherwise = [statdamroll2statdamlistint sdam]
            damageamount = pure $ diceroll + dicebon
            hitstarget
                | int2 == 1 = False
                | int2 == 20 = True
                | otherwise = (pure $ int + int2 + (view (#bonus) (fromJust mtar))) > target {-(defense2defensetype (view (#defensetype) (fromJust mtar)) s)-}
            ishelpless = 
                (isActive . temporary) (view (#effects . #held) s) || ((isActive . temporary) (view (#effects . #paralyzed) s)) || ((isActive . temporary) (view (#effects . #sleep) s))
            target
                | ishelpless = ffdefensetype (view (#defensetype) (fromJust mtar)) $
                    (set (#abilityscores . #wisdom) (pure 10 :: Temporary Int) $ 
                    set (#abilityscores . #dexterity) (pure 0 :: Temporary Int) $ 
                    over (#bonuses . #abilityscores . #wisdom) (fmap (\_ -> pure 0 :: Temporary Int)) $
                    over (#bonuses . #abilityscores . #dexterity) (fmap (\_ -> pure 0 :: Temporary Int)) s)
                | (isActive . temporary) (view (#effects . #prone) s) = ffdefensetype (view (#defensetype) (fromJust mtar)) s
                | (isActive . temporary) (view (#effects . #stunned) s) = ffdefensetype (view (#defensetype) (fromJust mtar)) s
                | (isActive . temporary) (view (#effects . #grappled) s) = ffdefensetype (view (#defensetype) (fromJust mtar)) s
                | (isActive . temporary) (view (#effects . #laughter) s) = ffdefensetype (view (#defensetype) (fromJust mtar)) s          
                | (isActive . temporary) (view (#effects . #flatfooted) s) = ffdefensetype (view (#defensetype) (fromJust mtar)) s   
                | otherwise = defense2defensetype (view (#defensetype) (fromJust mtar)) s

statdamroll2statdamlistint :: StatDamage a -> StatDamage [Int]
statdamroll2statdamlistint (SD msdt mtar mabil pdice _ dicebon durroll durationbon) = SD msdt mtar mabil pdice pdice dicebon durroll durationbon

durationroll :: StatDamage [Int] -> StdGen -> (StatDamage [Int], StdGen)
durationroll statdam g = (newstatdam, g')
        where
            (int, g') = runState (sumdice (view (#duration) statdam)) g
            newstatdam
                | view (#duration) statdam == [0] = statdam
                | otherwise = over (#durationbonus) (+ (runRoll int)) $ set (#duration) [0] statdam

durationdiceroll :: StatDamage [Int] -> Dice (StatDamage [Int])
durationdiceroll a = state $ durationroll a

rolldurations :: StdGen -> [StatDamage [Int]] -> ([StatDamage [Int]], StdGen)
rolldurations g a = runState (durationdiceroll ~> a) g

reducedurations :: [StatDamage a] -> [StatDamage a]
reducedurations [] = []
reducedurations (a : as) = go ++ (reducedurations as)
    where
      go
        | view (#durationbonus) a == 1 = []
        | otherwise = [over (#durationbonus) (\x -> x - 1) a]

storedpoisens :: [StatDamage [Int]] -> StdGen -> Status -> Status
storedpoisens [] _ s = s
storedpoisens (a : as) g s = storedpoisens as g'' (statdamage 0 (runRoll rollresult) (Just (runRoll <$> newa)) s)
    where
        (newa, g') = runState (sumdice ~> a) g
        (rollresult, g'') = roll 20 g'
