{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}


module DND.STATUSEFFECTS.StatusEffectFunctions1
where

import Control.Lens (view, over, set)
import Data.Generics.Labels ()
import Data.Maybe (fromJust)
import Data.List (delete)

import DND.ATTACKS.DamageReduction (Material (Norm), dredux)
import DND.ATTACKS.Smite (Race (Animal, Dragon))

import DND.CHARACTER.AbilityScores (revertas)
import DND.CHARACTER.Class (revertClassInfo)
import DND.CHARACTER.Status (Status, penalty, Size (Huge, Large, Medium, Tiny), channelenergy, layonhands, healthupdate)

import DND.DEFENSE.DefenseFunctions (maybeint2int)

import DND.STATUSEFFECTS.Effects (Temporal (Permanent, Absent, NA), viewtempmodifyier, addtemporal, removepermanent)

import DND.TemporaryValue (addtemporary, removetemporary, add2temporary, removefromtemporary)


beastshapeoff :: Temporal (Maybe Int) -> Status -> Status
beastshapeoff temp s = ({-extraattacks . updateweapons . removesecondhand . removeprimaryhand .-} chooseshape) modifyer
    where
      {-extraattacks
        | view (#classfeatures . #feats . #mutatedform)  s = over (#effects . #twoweaponfighting) revert
        | otherwise = id
      removeprimaryhand s' = set (#primaryhand) firsthand s'
      firsthand 
          | view (#otherweapons) s == [] = Weapon Iron UnArmed Nothing []
          | otherwise = either (\_ -> Weapon Iron UnArmed Nothing []) id (head $ view (#otherweapons) s)
      updateweapons :: Status -> Status
      updateweapons s'
          | view (#otherweapons) s == [] = s'
          | (isLeft . head) (view (#otherweapons) s) = s'
          | otherwise = over (#otherweapons) tail s'
      removesecondhand s' = set (#secondaryhand) Nothing s'-}
      modifyer = viewtempmodifyier temp
      chooseshape mi
        | mi == (Just 1) = over (#tempsize) (removefromtemporary Medium) $
                            over (#race) (removefromtemporary Animal) $
                            over (#bonuses . #abilityscores . #strength . #size) (removetemporary 2) $ 
                            over (#bonuses . #defense . #attacks . #size) (removetemporary 2) s
        | mi == (Just 2) = over (#tempsize) (removefromtemporary Large) $
                            over (#race) (removefromtemporary Animal) $
                            over (#bonuses . #abilityscores . #strength . #size) (removetemporary 4) $ 
                            penalty (#bonuses . #abilityscores . #dexterity . #penalty) (-2) $ 
                            over (#bonuses . #defense . #attacks . #size) (removetemporary 4) s
        | mi == (Just 3) = over (#tempsize) (removefromtemporary Medium) $
                            over (#race) (removefromtemporary Animal) $
                            over (#bonuses . #abilityscores . #strength . #size) (removetemporary 4) $ 
                            over (#bonuses . #defense . #attacks . #size) (removetemporary 4) s
        | mi == (Just 4) = over (#tempsize) (removefromtemporary Large) $
                            over (#race) (removefromtemporary Animal) $
                            over (#bonuses . #abilityscores . #strength . #size) (removetemporary 6) $ 
                            penalty (#bonuses . #abilityscores . #dexterity . #penalty) (-2) $ 
                            over (#bonuses . #abilityscores . #constitution . #size) (removetemporary 2) $
                            over (#bonuses . #defense . #attacks . #size) (removetemporary 6) s                                                        
        | otherwise = over (#tempsize) (removefromtemporary Medium) $
                            over (#race) (removefromtemporary Animal) $
                            over (#bonuses . #abilityscores . #strength . #size) (removetemporary 2) $ 
                            over (#bonuses . #defense . #attacks . #size) (removetemporary 2) s

beastshapeon :: Temporal (Maybe Int) -> Status -> Status
beastshapeon temp s = ({-extraattacks . removeprimaryhand . removesecondhand .-} chooseshape) modifyer
    where
      {-extraattacks
        | view (#classfeatures . #feats . #mutatedform)  s = over (#effects . #twoweaponfighting) (addtemporal $ Present2 Nothing (Just 4))
        | otherwise = id
      firsthand = view (#primaryhand) s
      secondhand = view (#secondaryhand) s
      removeprimaryhand s'
        | firsthand == (Weapon Iron UnArmed Nothing []) = set (#primaryhand) (Weapon Iron Claws ((\x -> x `div` 2) <$> modifyer) []) s'
        | otherwise = set (#primaryhand) (Weapon Iron Claws ((\x -> x `div` 2) <$> modifyer) []) $ over (#otherweapons) (\x -> [Right $ firsthand] ++ x) s'
      removesecondhand s'
        | (secondhand == Nothing) && (view (#classfeatures . #feats . #mutatedform) s) =
          set (#secondaryhand) (Just $ Right (Weapon Iron Claws ((\x -> x `div` 2) <$> modifyer) [])) s'
        | view (#classfeatures . #feats . #mutatedform) s = 
          set (#secondaryhand) (Just $ Right (Weapon Iron Claws ((\x -> x `div` 2) <$> modifyer) [])) $ over (#otherweapons) (\x -> x ++ [fromJust $ secondhand]) s'
        | secondhand == Nothing = s'
        | otherwise = set (#secondaryhand) Nothing $ over (#otherweapons) (\x -> x ++ [fromJust $ secondhand]) s'-}
      modifyer = viewtempmodifyier temp
      chooseshape mi
        | mi == (Just 1) = over (#tempsize) (add2temporary Medium) $
                            over (#race) (add2temporary Animal) $
                            over (#bonuses . #abilityscores . #strength . #size) (addtemporary 2) $ 
                            over (#bonuses . #defense . #attacks . #size) (addtemporary 2) s
        | mi == (Just 2) = over (#tempsize) (add2temporary Large) $
                            over (#race) (add2temporary Animal) $
                            over (#bonuses . #abilityscores . #strength . #size) (addtemporary 4) $ 
                            penalty (#bonuses . #abilityscores . #dexterity . #penalty) 2 $ 
                            over (#bonuses . #defense . #attacks . #size) (addtemporary 4) s
        | mi == (Just 3) = over (#tempsize) (add2temporary Medium) $
                            over (#race) (add2temporary Animal) $
                            over (#bonuses . #abilityscores . #strength . #size) (addtemporary 4) $ 
                            over (#bonuses . #defense . #attacks . #size) (addtemporary 4) s
        | mi == (Just 4) = over (#tempsize) (add2temporary Large) $
                            over (#race) (add2temporary Animal) $
                            over (#bonuses . #abilityscores . #strength . #size) (addtemporary 6) $ 
                            penalty (#bonuses . #abilityscores . #dexterity . #penalty) 2 $ 
                            over (#bonuses . #abilityscores . #constitution . #size) (addtemporary 2) $
                            over (#bonuses . #defense . #attacks . #size) (addtemporary 6) s                                                       
        | otherwise = over (#tempsize) (add2temporary Medium) $
                            over (#race) (add2temporary Animal) $
                            over (#bonuses . #abilityscores . #strength . #size) (addtemporary 2) $ 
                            over (#bonuses . #defense . #attacks . #size) (addtemporary 2) s

formofthedragonon :: Temporal (Maybe Int) -> Status -> Status
formofthedragonon temp s = choosesize modifyer
      where
        modifyer = viewtempmodifyier temp
        choosesize mi
          | mi == (Just 1) = over (#tempsize) (add2temporary Medium) $
                            over (#race) (add2temporary Dragon) $
                            over (#bonuses . #abilityscores . #strength . #size) (addtemporary 4) $ 
                            over (#bonuses . #abilityscores . #constitution . #size) (addtemporary 2) $ 
                            over (#bonuses . #defense . #attacks . #size) (addtemporary 4) s
          | mi == (Just 2) = over (#tempsize) (add2temporary Large) $
                            over (#race) (add2temporary Dragon) $
                            over (#bonuses . #abilityscores . #strength . #size) (addtemporary 6) $ 
                            over (#bonuses . #abilityscores . #constitution . #size) (addtemporary 4) $ 
                            over (#bonuses . #defense . #attacks . #size) (addtemporary 6) $
                            over (#damagereduction) (dredux addtemporary Norm 5) s
          | mi == (Just 3) = over (#tempsize) (add2temporary Huge) $
                            over (#race) (add2temporary Dragon) $
                            over (#bonuses . #abilityscores . #strength . #size) (addtemporary 10) $ 
                            over (#bonuses . #abilityscores . #constitution . #size) (addtemporary 8) $ 
                            over (#bonuses . #defense . #attacks . #size) (addtemporary 8) $
                            over (#damagereduction) (dredux addtemporary Norm 10) s
          | otherwise =     over (#tempsize) (add2temporary Medium) $
                            over (#race) (add2temporary Dragon) $
                            over (#bonuses . #abilityscores . #strength . #size) (addtemporary 4) $ 
                            over (#bonuses . #abilityscores . #constitution . #size) (addtemporary 2) $ 
                            over (#bonuses . #defense . #attacks . #size) (addtemporary 4) s

formofthedragonoff :: Temporal (Maybe Int) -> Status -> Status
formofthedragonoff temp s = choosesize modifyer
      where
        modifyer = viewtempmodifyier temp
        choosesize mi
          | mi == (Just 1) = over (#tempsize) (removefromtemporary Medium) $
                            over (#race) (removefromtemporary Dragon) $
                            over (#bonuses . #abilityscores . #strength . #size) (removetemporary 4) $ 
                            over (#bonuses . #abilityscores . #constitution . #size) (removetemporary 2) $ 
                            over (#bonuses . #defense . #attacks . #size) (removetemporary 4) s
          | mi == (Just 2) = over (#tempsize) (removefromtemporary Large) $
                            over (#race) (removefromtemporary Dragon) $
                            over (#bonuses . #abilityscores . #strength . #size) (removetemporary 6) $ 
                            over (#bonuses . #abilityscores . #constitution . #size) (removetemporary 4) $ 
                            over (#bonuses . #defense . #attacks . #size) (removetemporary 6) $
                            over (#damagereduction) (dredux removetemporary Norm 5) s
          | mi == (Just 3) = over (#tempsize) (removefromtemporary Huge) $
                            over (#race) (removefromtemporary Dragon) $
                            over (#bonuses . #abilityscores . #strength . #size) (removetemporary 10) $ 
                            over (#bonuses . #abilityscores . #constitution . #size) (removetemporary 8) $ 
                            over (#bonuses . #defense . #attacks . #size) (removetemporary 8) $
                            over (#damagereduction) (dredux removetemporary Norm 10) s
          | otherwise =     over (#tempsize) (removefromtemporary Medium) $
                            over (#race) (removefromtemporary Dragon) $
                            over (#bonuses . #abilityscores . #strength . #size) (removetemporary 4) $ 
                            over (#bonuses . #abilityscores . #constitution . #size) (removetemporary 2) $ 
                            over (#bonuses . #defense . #attacks . #size) (removetemporary 4) s


stigmataoff :: Temporal (Maybe Int) -> Status -> Status
stigmataoff temp s = over (#effects . #bleeding) removepermanent $ choosebonus modifyer
    where
      level = sum $ view (#playerclasses) s
      bonus = (1 + level) `div` 2
      modifyer = viewtempmodifyier temp
      choosebonus mi
          | mi == (Just 1) = over (#bonuses . #miscclass . #bab . #deflection) (removetemporary bonus) s
          | mi == (Just 2) = over (#bonuses . #damage . #deflection) (removetemporary bonus) s
          | mi == (Just 3) = over (#bonuses . #defense . #attacks . #deflection) (removetemporary bonus) s
          | mi == (Just 4) = s
          | mi == (Just 5) = over (#bonuses . #defense . #saves . #deflection) (removetemporary bonus) s
          | otherwise =  over (#bonuses . #miscclass . #bab . #deflection) (removetemporary bonus) s

stigmataon :: Temporal (Maybe Int) -> Status -> Status
stigmataon temp s = over (#effects . #bleeding) (addtemporal $ Permanent Nothing [] (Just bonus)) $ choosebonus modifyer
    where
      level = sum $ view (#playerclasses) s
      bonus = (1 + level) `div` 2
      modifyer = viewtempmodifyier temp
      choosebonus mi
          | mi == (Just 1) = over (#bonuses . #miscclass . #bab . #deflection) (addtemporary bonus) s
          | mi == (Just 2) = over (#bonuses . #damage . #deflection) (addtemporary bonus) s
          | mi == (Just 3) = over (#bonuses . #defense . #attacks . #deflection) (addtemporary bonus) s
          | mi == (Just 4) = s
          | mi == (Just 5) = over (#bonuses . #defense . #saves . #deflection) (addtemporary bonus) s
          | otherwise =  over (#bonuses . #miscclass . #bab . #deflection) (addtemporary bonus) s

channelshieldon :: Temporal (Maybe Int) -> Status -> Status
channelshieldon temp s
  | ((\x -> x >= 1) . length) (filter (\x -> x == channelenergy) (view (#specialabilities) s)) =
    over (#bonuses . #defense . #attacks . #deflection) (addtemporary int) removechannelenergy
  | otherwise = s
      where
        int
          | (viewtempmodifyier) temp == Nothing = 0
          | otherwise = fromJust ((viewtempmodifyier) temp)
        removechannelenergy :: Status
        removechannelenergy
          {-| ((\x -> x == 1) . length) (filter (\x -> x == channelenergy) (view (#specialabilities) s)) &&
            (view (#playerclasses . #pala) s >= 4) &&
            ((\x -> x > 3) . length) (filter (\x -> x == layonhands) (view (#specialabilities) s)) = 
            over (#specialabilities) ((delete layonhands) . (delete layonhands)) s
          | ((\x -> x == 1) . length) (filter (\x -> x == channelenergy) (view (#specialabilities) s)) &&
            (view (#playerclasses . #pala) s >= 4) = 
            over (#specialabilities) ((delete channelenergy) . (delete layonhands) . (delete layonhands)) s-}
          | otherwise = over (#specialabilities) (delete channelenergy) s

channelshieldoff :: Temporal (Maybe Int) -> Status -> Status
channelshieldoff temp s = over (#bonuses . #defense . #attacks . #deflection) (removetemporary int) s
      where
        int
          | (viewtempmodifyier) temp == Nothing = 0
          | otherwise = fromJust ((viewtempmodifyier) temp)

aidoff :: Temporal (Maybe Int) -> Status -> Status
aidoff temp s = over (#bonuses . #miscclass . #hp . #holy) (removetemporary int) $ 
  over (#bonuses . #miscclass . #bab . #holy) (removetemporary 1) s
  where
    int
      | (viewtempmodifyier) temp == Nothing = 0
      | otherwise = fromJust ((viewtempmodifyier) temp)

aidon :: Temporal (Maybe Int) -> Status -> Status
aidon temp s = over (#bonuses . #miscclass . #hp . #holy) (addtemporary int) $ 
  over (#bonuses . #miscclass . #bab . #holy) (addtemporary 1) s
  where
    int
      | (viewtempmodifyier) temp == Nothing = 0
      | otherwise = fromJust ((viewtempmodifyier) temp)

uncannydodgeoff :: Temporal (Maybe Int) -> Status -> Status
uncannydodgeoff _ s = set (#effects . #flatfooted) (pure $ Absent Nothing [] Nothing) s

uncannydodgeon :: Temporal (Maybe Int) -> Status -> Status
uncannydodgeon _ s = set (#effects . #flatfooted) (pure $ NA Nothing [] Nothing) s

blessoff ::  (Temporal (Maybe Int)) -> Status -> Status 
blessoff _ s = over (#bonuses . #miscclass . #bab . #holy) (removetemporary 1) s

blesson ::  (Temporal (Maybe Int)) -> Status -> Status 
blesson _ s = over (#bonuses . #miscclass . #bab . #holy) (addtemporary 1) s

wereraptoroff ::  (Temporal (Maybe Int)) -> Status -> Status 
wereraptoroff _ s = over (#bonuses . #defense . #attacks . #size) (removetemporary 4) $
                   over (#bonuses . #abilityscores . #constitution . #size) (removetemporary 3) $
                   over (#bonuses . #abilityscores . #strength . #size) (removetemporary 2) $
                   over (#bonuses . #abilityscores . #dexterity . #size) (removetemporary 2) s

wereraptoron ::  (Temporal (Maybe Int)) -> Status -> Status 
wereraptoron _ s = over (#bonuses . #defense . #attacks . #size) (addtemporary 4) $
                   over (#bonuses . #abilityscores . #constitution . #size) (addtemporary 3) $
                   over (#bonuses . #abilityscores . #strength . #size) (addtemporary 2) $
                   over (#bonuses . #abilityscores . #dexterity . #size) (addtemporary 2) s

shieldoff ::  (Temporal (Maybe Int)) -> Status -> Status 
shieldoff _ s = over (#bonuses . #defense . #attacks . #magic) (removetemporary 4) s

shieldon ::  (Temporal (Maybe Int)) -> Status -> Status 
shieldon _ s = over (#bonuses . #defense . #attacks . #magic) (addtemporary 4) s

magearmouroff ::  (Temporal (Maybe Int)) -> Status -> Status 
magearmouroff _ s = over (#bonuses . #defense . #attacks . #misc) (removetemporary 4) s

magearmouron ::  (Temporal (Maybe Int)) -> Status -> Status 
magearmouron _ s = over (#bonuses . #defense . #attacks . #misc) (addtemporary 4) s

bluroff ::  (Temporal (Maybe Int)) -> Status -> Status 
bluroff _ s = over (#concealment) (removetemporary 20) s

bluron ::  (Temporal (Maybe Int)) -> Status -> Status 
bluron _ s = over (#concealment) (addtemporary 20) s

heroismoff ::  (Temporal (Maybe Int)) -> Status -> Status 
heroismoff _ s = over (#bonuses . #defense . #saves . #magic) (removetemporary 2) $
                over (#bonuses . #damage. #magic) (removetemporary 2) $
                over (#bonuses . #miscclass . #bab . #magic) (removetemporary 2) s

heroismon ::  (Temporal (Maybe Int)) -> Status -> Status 
heroismon _ s = over (#bonuses . #defense . #saves . #magic) (addtemporary 2) $
                over (#bonuses . #damage. #magic) (addtemporary 2) $
                over (#bonuses . #miscclass . #bab . #magic) (addtemporary 2) s

growclawsoff ::  (Temporal (Maybe Int)) -> Status -> Status 
growclawsoff _ s = {-(updateweapons . removeprimaryhand)-} s
  {-where
    removeprimaryhand s' = set (#primaryhand) firsthand s'
    firsthand 
      | view (#otherweapons) s == [] = Weapon Iron UnArmed Nothing []
      | otherwise = either (\_ -> Weapon Iron UnArmed Nothing []) id (head $ view (#otherweapons) s )
    updateweapons :: Status -> Status
    updateweapons s'
      | view (#otherweapons) s == [] = s'
      | (isLeft . head) (view (#otherweapons) s) = s'
      | otherwise = over (#otherweapons) tail s'-}

growclawson ::  (Temporal (Maybe Int)) -> Status -> Status 
growclawson _ s = {-removeprimaryhand-} s
  {-where
    firsthand = view (#primaryhand) s
    removeprimaryhand s'
      | firsthand == (Weapon Iron UnArmed Nothing []) = set (#primaryhand) claws s'
      | otherwise = set (#primaryhand) claws $ over (#otherweapons) (\x -> (Right firsthand) : x) s'
    clawfunction :: Elemental -> Int -> Weapon
    clawfunction elemen i
      | i >= 11 = Weapon Magical1 Claws Nothing [elemental2enchantment elemen]
      | i >= 5 = Weapon Magical1 Claws Nothing []
      | otherwise = Weapon Iron Claws Nothing []
    element = snd $ view (#classfeatures . #bloodline . #draconic) s
    claws = clawfunction element (view (#playerclasses . #sorc) s + (view (#playerclasses . #drag) s))-}

entangledoff ::  (Temporal (Maybe Int)) -> Status -> Status 
entangledoff _ s = penalty (#bonuses . #abilityscores . #dexterity . #penalty) (-4) $
                   penalty (#bonuses . #miscclass . #bab . #penalty) (-2) s

entangledon ::  (Temporal (Maybe Int)) -> Status -> Status 
entangledon _ s = penalty (#bonuses . #abilityscores . #dexterity . #penalty) 4 $
                  penalty (#bonuses . #miscclass . #bab . #penalty) 2 s

barkskinoff ::  (Temporal (Maybe Int)) -> Status -> Status 
barkskinoff temp s = over (#bonuses . #defense . #attacks . #holy) (removetemporary int) s
    where
      int = maybeint2int $ viewtempmodifyier temp

barkskinon ::  (Temporal (Maybe Int)) -> Status -> Status 
barkskinon temp s = over (#bonuses . #defense . #attacks . #holy) (addtemporary int) s
    where
      int = maybeint2int $ viewtempmodifyier temp

polymorthoff ::  (Temporal (Maybe Int)) -> Status -> Status 
polymorthoff _ s = penalty (#bonuses . #abilityscores . #strength . #penalty) (-4) $
                   penalty (#bonuses . #miscclass . #bab . #penalty) (-2) $
                   over (#tempsize) (removefromtemporary Tiny) $
                   over (#race) (removefromtemporary Animal) s

polymorthon ::  (Temporal (Maybe Int)) -> Status -> Status 
polymorthon _ s = penalty (#bonuses . #abilityscores . #strength . #penalty) 4 $
                  penalty (#bonuses . #miscclass . #bab . #penalty) 2 $
                  over (#tempsize) (add2temporary Tiny) $
                  over (#race) (add2temporary Animal) s

divinefavouroff ::  (Temporal (Maybe Int)) -> Status -> Status 
divinefavouroff temp s = over (#bonuses . #miscclass . #bab . #holy) (removetemporary modif) $
                      over (#bonuses . #damage . #holy) (removetemporary modif) s
                      where
                        modif = maybeint2int . viewtempmodifyier $ temp

divinefavouron ::  (Temporal (Maybe Int)) -> Status -> Status 
divinefavouron temp s = over (#bonuses . #miscclass . #bab . #holy) (addtemporary modif) $
                      over (#bonuses . #damage . #holy) (addtemporary modif) s
                      where
                        modif = maybeint2int . viewtempmodifyier $ temp                      

poisenedoff :: (Temporal (Maybe Int)) -> Status -> Status 
poisenedoff _ s = healthupdate $ set (#statdamage) [] $ over (#abilityscores) revertas $ over (#primclass) revertClassInfo  s

stunnedon :: (Temporal (Maybe Int)) -> Status -> Status
stunnedon _ s = penalty (#bonuses . #defense . #attacks . #penalty) (-2) s

stunnedoff :: (Temporal (Maybe Int)) -> Status -> Status
stunnedoff _ s = penalty (#bonuses . #defense . #attacks . #penalty) 2 s

kion :: (Temporal (Maybe Int)) -> Status -> Status
kion temp s
  | (Just 2 ==) . viewtempmodifyier $ temp = over (#bonuses . #defense . #attacks . #deflection) (addtemporary 4) s
  | otherwise = s

kioff :: (Temporal (Maybe Int)) -> Status -> Status
kioff temp s
  | (Just 2 ==) . viewtempmodifyier $ temp = over (#bonuses . #defense . #attacks . #deflection) (removetemporary 4) s
  | otherwise = s
