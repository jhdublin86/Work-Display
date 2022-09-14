{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}


module DND.STATUSEFFECTS.StatusEffectFunctions2
where

import Control.Lens (view, over)
import Data.Generics.Labels ()

import DND.ATTACKS.DamageReduction (Material (Norm), dredux)
import DND.ATTACKS.Smite (Race (UnDead, Outsider))

import DND.CHARACTER.Status (Status, penalty, Size (Huge, Large))

import DND.DAMAGEDICE.Elemental (modifyprofile', removeprofile', undeadelementalresistance)

import DND.STATUSEFFECTS.Effects (StatusEffect (..), Temporal (Present, NA, Absent, Permanent, Off, On), removena, viewtempmodifyier, removepermanent, (&), isOn, isOff, setmodifyer)
import DND.STATUSEFFECTS.StatusEffectFunctions 
import DND.STATUSEFFECTS.StatusEffectFunctions1

import DND.TemporaryValue (temporary, addtemporary, removetemporary, revert, add2temporary, removefromtemporary, modifytemporary)

rageoff ::  (Temporal (Maybe Int)) -> Status -> Status 
rageoff _ s = go
  where
    go = fatigued $
        over (#bonuses . #abilityscores . #strength . #deflection) (removetemporary (2 * go1)) $ 
        penalty (#bonuses . #defense . #attacks . #penalty) (-2) $
        over (#bonuses . #abilityscores . #constitution . #deflection) (removetemporary (2 * go1)) $
        over (#bonuses . #miscclass . #wil . #deflection) (removetemporary (go1 + go2 )) $ s
    go1
      {-| view (#playerclasses . #barb) s >= 20 = 4
      | view (#playerclasses . #barb) s >= 11 = 3-}
      | otherwise = 2
    go2
      {-| view (#playerclasses . #barb) s >= 14 = 4-}
      | otherwise = 0
    fatigued
      {-| view (#playerclasses . #barb) s >= 17 = id-}
      | otherwise = t2ix Fatigued (Present (Just 5) [] Nothing)

rageon ::  (Temporal (Maybe Int)) -> Status -> Status 
rageon _ s = go
  where
    go = over (#bonuses . #abilityscores . #strength . #deflection) (addtemporary (2 * go1)) $ 
         penalty (#bonuses . #defense . #attacks . #penalty) 2 $
         over (#bonuses . #abilityscores . #constitution . #deflection) (addtemporary (2 * go1)) $
         over (#bonuses . #miscclass . #wil . #deflection) (addtemporary (go1 + go2)) $ s
    go1
      {-| view (#playerclasses . #barb) s >= 20 = 4
      | view (#playerclasses . #barb) s >= 11 = 3-}
      | otherwise = 2
    go2
      {-| view (#playerclasses . #barb) s >= 14 = 4-}
      | otherwise = 0

fatigueoff ::  (Temporal (Maybe Int)) -> Status -> Status 
fatigueoff _ s = go
      where
        go = penalty (#bonuses . #abilityscores . #strength . #penalty) (-2) $
          penalty (#bonuses . #abilityscores . #constitution . #penalty) (-2) $
          over (#effects . #rage) removena s

fatigueon ::  (Temporal (Maybe Int)) -> Status -> Status 
fatigueon _ s = go
      where
        go = penalty (#bonuses . #abilityscores . #strength . #penalty) 2 $
          penalty (#bonuses . #abilityscores . #constitution . #penalty) 2 $
          t2ix Rage (NA Nothing [] Nothing) s

photonoff ::  (Temporal (Maybe Int)) -> Status -> Status 
photonoff _ s = go
      where
        go = over (#bonuses . #damage . #deflection) (removetemporary bonusamount) $
          over (#effects . #photonmode) (modifytemporary (setmodifyer Nothing)) s
        bonusamount = level `div` 6 + 1
        level = view (#playerclasses . #solar) s

photonon ::  (Temporal (Maybe Int)) -> Status -> Status 
photonon _ s = go
      where
        go = t2ix GravitonMode (Off Nothing [] Nothing) $
          over (#bonuses . #damage . #deflection) (addtemporary bonusamount) s
        bonusamount = level `div` 6 + 1
        level = view (#playerclasses . #solar) s

gravitonoff ::  (Temporal (Maybe Int)) -> Status -> Status 
gravitonoff _ s = go
      where
        go = darkmattert2 $
          over (#bonuses . #miscclass . #ref . #deflection) (removetemporary bonusamount) $
          over (#effects . #gravitonmode) (modifytemporary (setmodifyer Nothing)) s
        bonusamount = level `div` 9 + 1
        level = view (#playerclasses . #solar) s
        darkmattert2 = if isOn (temporary $ view (#effects . #darkmatter) s) then t2ix DarkMatter (Off Nothing [] Nothing) else id

gravitonon ::  (Temporal (Maybe Int)) -> Status -> Status 
gravitonon _ s = go
      where
        go = darkmattert2 $ t2ix PhotonMode (Off Nothing [] Nothing) $
          over (#bonuses . #miscclass . #ref . #deflection) (addtemporary bonusamount) s
        darkmattert2 = if isOff (temporary $ view (#effects . #darkmatter) s) then t2ix DarkMatter (On Nothing [] Nothing) else id
        bonusamount = level `div` 9 + 1
        level = view (#playerclasses . #solar) s

exhaustedoff ::  (Temporal (Maybe Int)) -> Status -> Status 
exhaustedoff _ s = go
      where
        go = penalty (#bonuses . #abilityscores . #strength . #penalty) (-6) $
          penalty (#bonuses . #abilityscores . #constitution . #penalty) (-6) $
          over (#effects . #rage) removena s

exhaustedon ::  (Temporal (Maybe Int)) -> Status -> Status 
exhaustedon _ s = go
      where
        go = penalty (#bonuses . #abilityscores . #strength . #penalty) 6 $
          penalty (#bonuses . #abilityscores . #constitution . #penalty) 6 $
          t2ix Rage (NA Nothing [] Nothing) s

undeadon ::  (Temporal (Maybe Int)) -> Status -> Status 
undeadon _ s =  over (#elementalresistance) (modifyprofile' <$> (temporary <$> undeadelementalresistance) <*>) $
  over (#race) (add2temporary UnDead) $
  t2ix ImmunetoCriticals (Permanent Nothing [] Nothing) $
  t2ix ImmunetoDiseae (Permanent Nothing [] Nothing) $
  t2ix ImmunetoSneakAttack (Permanent Nothing [] Nothing) $
  t2ix ImmunetoPoisen (Permanent Nothing [] Nothing) $
  t2ix ImmuneMindInfluencingEffects (Permanent Nothing [] Nothing) $
  changestatus (#effects . #turned) (\_ -> id) (\_ -> id) (Absent Nothing [] Nothing) $
  changestatus (#effects . #commanded) (\_ -> id) (\_ -> id) (Absent Nothing [] Nothing) s

undeadoff ::  (Temporal (Maybe Int)) -> Status -> Status 
undeadoff _ s =  over (#elementalresistance) (removeprofile' <$> (temporary <$> undeadelementalresistance) <*>) $
  over (#race) (removefromtemporary UnDead) $
  over (#effects . #immunetocriticals) removepermanent $
  over (#effects . #immunetodisease) removepermanent $
  over (#effects . #immunetosneakattack) removepermanent $
  over (#effects . #immunetopoisen) removepermanent $
  changestatus (#effects . #immuneMindInfluencingEffects) immuneMindInfluencingEffectson immuneMindInfluencingEffectsoff (Absent Nothing [] Nothing) $
  over (#effects . #turned) removena $
  over (#effects . #commanded) removena s

immuneMindInfluencingEffectson ::  (Temporal (Maybe Int)) -> Status -> Status
immuneMindInfluencingEffectson _ s = t2ix Sleep (NA Nothing [] Nothing) $
  t2ix Fatigued (NA Nothing [] Nothing) $
  t2ix Exhausted (NA Nothing [] Nothing) $
  t2ix Bleeding (NA Nothing [] Nothing) $
  t2ix Shaken (NA Nothing [] Nothing) $
  t2ix Fear (NA Nothing [] Nothing) $
  t2ix Sickened (NA Nothing [] Nothing) $
  t2ix Confused (NA Nothing [] Nothing) $
  t2ix Laughter (NA Nothing [] Nothing) $
  t2ix Dazed (NA Nothing [] Nothing) $
  t2ix Despair (NA Nothing [] Nothing) s

invisibleoff ::  (Temporal (Maybe Int)) -> Status -> Status 
invisibleoff _ s = go5
  where
    go1 = over (#concealment) (removetemporary 50) s
    go2 = over (#bonuses . #defense . #attacks . #misc) (removetemporary 5) go1
    go3 = over (#bonuses . #miscclass . #bab . #misc) (removetemporary 3) go2
    go4 = over (#bonuses . #skills . #stealth . #misc) (removetemporary 20) go3
    go5 = t2ix Stealth (Absent Nothing [] Nothing) go4
    
invisibleon ::  (Temporal (Maybe Int)) -> Status -> Status 
invisibleon _ s = go5
  where
    go1 = over (#concealment) (addtemporary 50) s
    go2 = over (#bonuses . #defense . #attacks . #misc) (addtemporary 5) go1
    go3 = over (#bonuses . #miscclass . #bab . #misc) (addtemporary 3) go2
    go4 = over (#bonuses . #skills . #stealth . #misc) (addtemporary 20) go3
    go5 = t2ix Stealth (Present Nothing [] Nothing) go4

elementalbodyiiioff :: Temporal (Maybe Int) -> Status -> Status
elementalbodyiiioff temp s = elementalbonuses $ chooseshape modifyer
    where
      {-extraattacks
        | view (#classfeatures . #feats . #mutatedform)  s = over (#effects . #twoweaponfighting) revert
        | otherwise = id
      removeprimaryhand s' = set (#primaryhand) firsthand s'
      firsthand 
          | view (#otherweapons) s == [] = Weapon Iron UnArmed Nothing []
          | otherwise = either (\_ -> Weapon Iron UnArmed Nothing []) id (head $ view (#otherweapons) s )
      updateweapons :: Status -> Status
      updateweapons s'
          | view (#otherweapons) s == [] = s'
          | (isLeft . head) (view (#otherweapons) s) = s'
          | otherwise = over (#otherweapons) tail s'
      removesecondhand s'= set (#secondaryhand) Nothing  s'-}
      modifyer = viewtempmodifyier temp
      elementalbonuses :: Status -> Status
      elementalbonuses s' = over (#tempsize) (removefromtemporary Large) $
                          over (#race) (removefromtemporary Outsider) $
                          t2ix ImmunetoCriticals (Absent Nothing [] Nothing) $
                          t2ix ImmunetoSneakAttack (Absent Nothing [] Nothing) $
                          over (#effects . #bleeding) removena {-$
                          (extraattacks . updateweapons . removesecondhand . removeprimaryhand)-} s'
      chooseshape mi
        | mi == (Just 1) = over (#bonuses . #abilityscores . #strength . #size) (removetemporary 2) $ 
                            over (#bonuses . #abilityscores . #dexterity . #size) (removetemporary 4) $ 
                            over (#bonuses . #defense . #attacks . #size) (removetemporary 4) s
        | mi == (Just 2) = over (#bonuses . #abilityscores . #strength . #size) (removetemporary 6) $ 
                            penalty (#bonuses . #abilityscores . #dexterity . #penalty) (-2) $
                            over (#bonuses . #abilityscores . #constitution . #size) (removetemporary 2) $ 
                            over (#bonuses . #defense . #attacks . #size) (removetemporary 6) s
        | mi == (Just 3) = over (#bonuses . #abilityscores . #constitution . #size) (removetemporary 4) $ 
                            over (#bonuses . #abilityscores . #dexterity . #size) (removetemporary 2) $ 
                            over (#bonuses . #defense . #attacks . #size) (removetemporary 4) s
        | mi == (Just 4) = over (#bonuses . #abilityscores . #strength . #size) (removetemporary 4) $ 
                            over (#bonuses . #abilityscores . #dexterity . #size) (removetemporary 6) $ 
                            over (#bonuses . #defense . #attacks . #size) (removetemporary 6) s
        | otherwise = over (#bonuses . #abilityscores . #strength . #size) (removetemporary 4) $ 
                            over (#bonuses . #abilityscores . #dexterity . #size) (removetemporary 6) $ 
                            over (#bonuses . #defense . #attacks . #size) (removetemporary 6) s

elementalbodyiiion :: Temporal (Maybe Int) -> Status -> Status
elementalbodyiiion temp s = elementalbonuses $ chooseshape modifyer
    where
      {-extraattacks
        | view (#classfeatures . #feats . #mutatedform)  s = over (#effects . #twoweaponfighting) (addtemporal2 $ Present Nothing [] (Just 4))
        | otherwise = id
      firsthand = view (#primaryhand) s
      secondhand = view (#secondaryhand) s
      removeprimaryhand s'
        | firsthand == (Weapon Iron UnArmed Nothing []) = set (#primaryhand) (Weapon Iron Claws (Just 2) []) s'
        | otherwise = set (#primaryhand) (Weapon Iron Claws (Just 2) []) $ over (#otherweapons) (\x -> [Right $ firsthand] ++ x) s'
      removesecondhand s'
        | (secondhand == Nothing) && (view (#classfeatures . #feats . #mutatedform) s) = 
          set (#secondaryhand) (Just $ Right (Weapon Iron Claws (Just 2) [])) s'
        | view (#classfeatures . #feats . #mutatedform) s = 
          set (#secondaryhand) (Just $ Right (Weapon Iron Claws (Just 2) [])) $ over (#otherweapons) (\x -> x ++ [fromJust $ secondhand]) s'
        | (secondhand == Nothing) = s'
        | otherwise = set (#secondaryhand) Nothing $ over (#otherweapons) (\x -> x ++ [fromJust $ secondhand]) s'-}
      modifyer = viewtempmodifyier temp
      elementalbonuses :: Status -> Status
      elementalbonuses s' = over (#tempsize) (add2temporary Large) $
                          over (#race) (add2temporary Outsider) $
                          t2ix ImmunetoCriticals (Present Nothing [] Nothing) $
                          t2ix ImmunetoSneakAttack (Present Nothing [] Nothing) $
                          t2ix Bleeding (NA Nothing [] Nothing) {-$
                          (extraattacks . removeprimaryhand . removesecondhand)-} s'
      chooseshape mi
        | mi == (Just 1) = over (#bonuses . #abilityscores . #strength . #size) (addtemporary 2) $ 
                            over (#bonuses . #abilityscores . #dexterity . #size) (addtemporary 4) $ 
                            over (#bonuses . #defense . #attacks . #size) (addtemporary 4) s
        | mi == (Just 2) = over (#bonuses . #abilityscores . #strength . #size) (addtemporary 6) $ 
                            penalty (#bonuses . #abilityscores . #dexterity . #penalty) 2 $
                            over (#bonuses . #abilityscores . #constitution . #size) (addtemporary 2) $ 
                            over (#bonuses . #defense . #attacks . #size) (addtemporary 6) s
        | mi == (Just 3) = over (#bonuses . #abilityscores . #constitution . #size) (addtemporary 4) $ 
                            over (#bonuses . #abilityscores . #dexterity . #size) (addtemporary 2) $ 
                            over (#bonuses . #defense . #attacks . #size) (addtemporary 4) s
        | mi == (Just 4) = over (#bonuses . #abilityscores . #strength . #size) (addtemporary 4) $ 
                            over (#bonuses . #abilityscores . #dexterity . #size) (addtemporary 6) $ 
                            over (#bonuses . #defense . #attacks . #size) (addtemporary 6) s
        | otherwise =  over (#bonuses . #abilityscores . #strength . #size) (addtemporary 4) $ 
                            over (#bonuses . #abilityscores . #dexterity . #size) (addtemporary 6) $ 
                            over (#bonuses . #defense . #attacks . #size) (addtemporary 6) s

elementalbodyivoff :: Temporal (Maybe Int) -> Status -> Status
elementalbodyivoff temp s = elementalbonuses $ chooseshape modifyer
    where
      {-extraattacks
        | view (#classfeatures . #feats . #mutatedform)  s = over (#effects . #twoweaponfighting) revert
        | otherwise = id
      removeprimaryhand s' = set (#primaryhand) firsthand s'
      firsthand 
          | view (#otherweapons) s == [] = Weapon Iron UnArmed Nothing []
          | otherwise = either (\_ -> Weapon Iron UnArmed Nothing []) id (head $ view (#otherweapons) s )
      updateweapons :: Status -> Status
      updateweapons s'
          | view (#otherweapons) s == [] = s'
          | (isLeft . head) (view (#otherweapons) s) = s'
          | otherwise = over (#otherweapons) tail s'
      removesecondhand s'= set (#secondaryhand) Nothing  s'-}
      modifyer = viewtempmodifyier temp
      elementalbonuses :: Status -> Status
      elementalbonuses s' = over (#tempsize) (removefromtemporary Huge) $
                          over (#race) (removefromtemporary Outsider) $
                          t2ix ImmunetoCriticals (Absent Nothing [] Nothing) $
                          t2ix ImmunetoSneakAttack (Absent Nothing [] Nothing) $
                          over (#effects . #bleeding) removena {-$
                          (extraattacks . updateweapons . removesecondhand . removeprimaryhand)-} s'
      chooseshape mi
        | mi == (Just 1) = over (#bonuses . #abilityscores . #strength . #size) (removetemporary 4) $ 
                            over (#bonuses . #abilityscores . #dexterity . #size) (removetemporary 6) $ 
                            over (#bonuses . #defense . #attacks . #size) (removetemporary 4) s
        | mi == (Just 2) = over (#bonuses . #abilityscores . #strength . #size) (removetemporary 8) $ 
                            penalty (#bonuses . #abilityscores . #dexterity . #penalty) (-2) $
                            over (#bonuses . #abilityscores . #constitution . #size) (removetemporary 4) $ 
                            over (#bonuses . #defense . #attacks . #size) (removetemporary 6) s
        | mi == (Just 3) = over (#bonuses . #abilityscores . #constitution . #size) (removetemporary 4) $ 
                            over (#bonuses . #abilityscores . #dexterity . #size) (removetemporary 6) $ 
                            over (#bonuses . #defense . #attacks . #size) (removetemporary 4) s
        | mi == (Just 4) = over (#bonuses . #abilityscores . #strength . #size) (removetemporary 4) $
                            penalty (#bonuses . #abilityscores . #dexterity . #penalty) (-2) $
                            over (#bonuses . #abilityscores . #constitution . #size) (removetemporary 8) $ 
                            over (#bonuses . #defense . #attacks . #size) (removetemporary 6) s
        | otherwise = over (#bonuses . #abilityscores . #strength . #size) (removetemporary 4) $
                            penalty (#bonuses . #abilityscores . #dexterity . #penalty) (-2) $
                            over (#bonuses . #abilityscores . #constitution . #size) (removetemporary 8) $ 
                            over (#bonuses . #defense . #attacks . #size) (removetemporary 6) s

elementalbodyivon :: Temporal (Maybe Int) -> Status -> Status
elementalbodyivon temp s = elementalbonuses $ chooseshape modifyer
    where
      {-extraattacks
        | view (#classfeatures . #feats . #mutatedform)  s = over (#effects . #twoweaponfighting) (addtemporal2 $ Present Nothing [] (Just 4))
        | otherwise = id
      firsthand = view (#primaryhand) s
      secondhand = view (#secondaryhand) s
      removeprimaryhand s'
        | firsthand == (Weapon Iron UnArmed Nothing []) = set (#primaryhand) (Weapon Iron Claws (Just 2) [elementalchoice modifyer]) s'
        | otherwise = set (#primaryhand) (Weapon Iron Claws (Just 2) [elementalchoice modifyer]) $ over (#otherweapons) (\x -> [Right $ firsthand] ++ x) s'
      removesecondhand s'
        | (secondhand == Nothing) && (view (#classfeatures . #feats . #mutatedform) s) = 
          set (#secondaryhand) (Just $ Right (Weapon Iron Claws (Just 2) [elementalchoice modifyer])) s'
        | view (#classfeatures . #feats . #mutatedform) s = 
          set (#secondaryhand) (Just $ Right (Weapon Iron Claws (Just 2) [elementalchoice modifyer])) $ over (#otherweapons) (\x -> x ++ [fromJust $ secondhand]) s'
        | (secondhand == Nothing) = s'
        | otherwise = set (#secondaryhand) Nothing $ over (#otherweapons) (\x -> x ++ [fromJust $ secondhand]) s'-}
      modifyer = viewtempmodifyier temp
      elementalbonuses :: Status -> Status
      elementalbonuses s' = over (#tempsize) (add2temporary Huge) $
                          over (#race) (add2temporary Outsider) $
                          t2ix ImmunetoCriticals (Present Nothing [] Nothing) $
                          t2ix ImmunetoSneakAttack (Present Nothing [] Nothing) $
                          t2ix Bleeding (NA Nothing [] Nothing) $
                          over (#damagereduction) (dredux addtemporary Norm 5) {-$
                          (extraattacks . removeprimaryhand . removesecondhand)-} s'
      {-elementalchoice mi
        | mi == (Just 1) = Thundering   
        | mi == (Just 2) = Icy
        | mi == (Just 3) = Fiery
        | mi == (Just 4) = Acidic
        | otherwise = Acidic-}
      chooseshape mi
        | mi == (Just 1) = over (#bonuses . #abilityscores . #strength . #size) (addtemporary 4) $ 
                            over (#bonuses . #abilityscores . #dexterity . #size) (addtemporary 6) $ 
                            over (#bonuses . #defense . #attacks . #size) (addtemporary 4) s
        | mi == (Just 2) = over (#bonuses . #abilityscores . #strength . #size) (addtemporary 8) $ 
                            penalty (#bonuses . #abilityscores . #dexterity . #penalty) 2 $
                            over (#bonuses . #abilityscores . #constitution . #size) (addtemporary 4) $ 
                            over (#bonuses . #defense . #attacks . #size) (addtemporary 6) s
        | mi == (Just 3) = over (#bonuses . #abilityscores . #constitution . #size) (addtemporary 4) $ 
                            over (#bonuses . #abilityscores . #dexterity . #size) (addtemporary 6) $ 
                            over (#bonuses . #defense . #attacks . #size) (addtemporary 4) s
        | mi == (Just 4) = over (#bonuses . #abilityscores . #strength . #size) (addtemporary 4) $
                            penalty (#bonuses . #abilityscores . #dexterity . #penalty) 2 $
                            over (#bonuses . #abilityscores . #constitution . #size) (addtemporary 8) $ 
                            over (#bonuses . #defense . #attacks . #size) (addtemporary 6) s
        | otherwise = over (#bonuses . #abilityscores . #strength . #size) (addtemporary 4) $
                            penalty (#bonuses . #abilityscores . #dexterity . #penalty) 2 $
                            over (#bonuses . #abilityscores . #constitution . #size) (addtemporary 8) $ 
                            over (#bonuses . #defense . #attacks . #size) (addtemporary 6) s

t2ix :: StatusEffect -> Temporal (Maybe Int) -> Status -> Status
t2ix Undead = updatestatus (#effects . #undead) undeadon undeadoff
t2ix Invisible = updatestatus (#effects . #invisible) invisibleon invisibleoff
t2ix ImmunetoCriticals = updatestatus (#effects . #immunetocriticals) (\_ -> id) (\_ -> id)
t2ix MirrorImage = updatestatus (#effects . #mirrorImage) (\_ -> id) (\_ -> id)
t2ix TrueStrike = updatestatus (#effects . #trueStrike) (\_ -> id) (\_ -> id)
t2ix Evasion = updatestatus (#effects . #evasion) (\_ -> id) (\_ -> id)
t2ix GreaterEvasion = updatestatus (#effects . #greaterEvasion) (\_ -> id) (\_ -> id)
t2ix Sing = updatestatus (#effects . #sing) singon singoff
t2ix Rage = updatestatus (#effects . #rage) rageon rageoff
t2ix BearStrength = updatestatus (#effects . #bearStrength) bearstrengthon bearstrengthoff
t2ix StoneSkin = updatestatus (#effects . #stoneSkin) (\_ -> id) (\_ -> id)
t2ix Repelled = updatestatus (#effects . #repelled) (\_ -> id) (\_ -> id)
t2ix Blind = updatestatus (#effects . #blind) blindon blindoff
t2ix Stealth = updatestatus (#effects . #stealth) (\_ -> id) (\_ -> id)
t2ix Bleeding = updatestatus (#effects . #bleeding) (\_ -> id) (\_ -> id)
t2ix Prone = updatestatus (#effects . #prone) proneon proneoff
t2ix UncannyDodge = updatestatus (#effects . #uncannydodge) uncannydodgeon uncannydodgeoff
t2ix Disarmed = updatestatus (#effects . #disarmed) disarmon (\_ -> id)
t2ix Getem = updatestatus (#effects . #getem) getemon getemoff
t2ix ExpertAttack = updatestatus (#effects . #expertattack) (\_ -> id) (\_ -> id)
t2ix ImprovedGetem = updatestatus (#effects . #improvedgetem) (\_ -> id) (\_ -> id)
t2ix ForceField = updatestatus (#effects . #forcefield) (\_ -> id) (\_ -> id)
t2ix Mobility = updatestatus (#effects . #mobility) (\_ -> id) (\_ -> id)
t2ix ArcanStrike = updatestatus (#effects . #arcanestrike) (\_ -> id) (\_ -> id)
t2ix Fatigued = updatestatus (#effects . #fatigued) fatigueon fatigueoff
t2ix Exhausted = updatestatus (#effects . #exhausted) exhaustedon exhaustedoff
t2ix Shaken = updatestatus (#effects . #shaken) shakenon shakenoff
t2ix Fear = updatestatus (#effects . #fear) fearon fearoff
t2ix Sickened = updatestatus (#effects . #sickened) sickenedon sickenedoff
t2ix Nauseated = updatestatus (#effects . #nauseated) (\_ -> id) (\_ -> id)
t2ix ImmunetoDiseae = updatestatus (#effects . #immunetodisease) (\_ -> id) (\_ -> id)
t2ix ImmunetoMagic = updatestatus (#effects . #immunetomagic) (\_ -> id) (\_ -> id)
t2ix ImmunetoPoisen = updatestatus (#effects . #immunetopoisen) (\_ -> id) (\_ -> id)
t2ix Sleep = updatestatus (#effects . #sleep) (\_ -> id) (\_ -> id)
t2ix KeenSenses = updatestatus (#effects . #keensenses) keensenseson keensensesoff
t2ix ImmunetoSneakAttack = updatestatus (#effects . #immunetosneakattack) (\_ -> id) (\_ -> id)
t2ix SpellFocus = updatestatus (#effects . #spellfocus) (\_ -> id) (\_ -> id)
t2ix Burning = updatestatus (#effects . #burning) (\_ -> id) (\_ -> id)
t2ix Paralyzed = updatestatus (#effects . #paralyzed) (\_ -> id) (\_ -> id)
t2ix Confused = updatestatus (#effects . #confused) (\_ -> id) (\_ -> id)
t2ix Petrified = updatestatus (#effects . #petrified) (\_ -> id) (\_ -> id)
t2ix Laughter = updatestatus (#effects . #laughter) (\_ -> id) (\_ -> id)
t2ix ImprovedInvisibility = updatestatus (#effects . #improvedinvisibility) invisibleon invisibleoff
t2ix ProtectiveWard = updatestatus (#effects . #protectiveward) protectivewardon protectivewardoff
t2ix Dazed = updatestatus (#effects . #dazed) (\_ -> id) (\_ -> id)
t2ix Despair = updatestatus (#effects . #despair) sickenedon sickenedoff
t2ix AuraofDespair = updatestatus (#effects . #auraofdespair) (\_ -> id) (\_ -> id)
t2ix SpellTurning = updatestatus (#effects . #spellturning) (\_ -> id) (\_ -> id)
t2ix Dominated = updatestatus (#effects . #dominated) dominatedon dominatedoff
t2ix BeastShape = updatestatus (#effects . #beastshape) beastshapeon beastshapeoff
t2ix ElementalBodyIII = updatestatus (#effects . #elementalbodyiii) elementalbodyiiion elementalbodyiiioff
t2ix ElementalBodyIV = updatestatus (#effects . #elementalbodyiv) elementalbodyivon elementalbodyivoff
t2ix SmiteEvil = updatestatus (#effects . #smiteevil) (\_ -> id) (\_ -> id)
t2ix DivineBond = updatestatus (#effects . #divinebond) (\_ -> id) (\_ -> id)
t2ix WeaponFinesse = updatestatus (#effects . #weaponfinesse) (\_ -> id) (\_ -> id)
t2ix FormoftheDragon = updatestatus (#effects . #formofthedragon) formofthedragonon formofthedragonoff
t2ix Stigmata = updatestatus (#effects . #stigmata) stigmataon stigmataoff
t2ix ChannelSmite = updatestatus (#effects . #channelsmite) (\_ -> id) (\_ -> id)
t2ix MagicFang = updatestatus (#effects . #magicfang) (\_ -> id) (\_ -> id)
t2ix ChannelShield = updatestatus (#effects . #channelshield) channelshieldon channelshieldoff
t2ix Aid = updatestatus (#effects . #aid) aidon aidoff
t2ix Bless = updatestatus (#effects . #bless) blesson blessoff
t2ix DeadlyAim = updatestatus (#effects . #deadlyaim) (\_ -> id) (\_ -> id)
t2ix WereRaptor = updatestatus (#effects . #wereraptor) wereraptoron wereraptoroff
t2ix Shield = updatestatus (#effects . #shield) shieldon shieldoff
t2ix Blur = updatestatus (#effects . #blur) bluron bluroff
t2ix Heroism = updatestatus (#effects . #heroism) heroismon heroismoff
t2ix FireShield = updatestatus (#effects . #fireshield) (\_ -> id) (\_ -> id)
t2ix CatsGrace = updatestatus (#effects . #catsgrace) catsgraceon catsgraceoff
t2ix Held = updatestatus (#effects . #held) (\_ -> id) (\_ -> id)
t2ix BullsEndurance = updatestatus (#effects . #bullsendurance) bullsenduranceon bullsenduranceoff
t2ix GrowClaws = updatestatus (#effects . #growclaws) growclawson growclawsoff
t2ix Entangled = updatestatus (#effects . #entangled) entangledon entangledoff
t2ix BarkSkin = updatestatus (#effects . #barkskin) barkskinon barkskinoff
t2ix Polymorth = updatestatus (#effects . #polymorth) polymorthon polymorthoff
t2ix Summoned = updatestatus (#effects . #summoned) (\_ -> id) (\_ -> id)
t2ix CombatShot = updatestatus (#effects . #combatShot) (\_ -> id) (\_ -> id)
t2ix SeeInvisibility = updatestatus (#effects . #seeinvisibility) (\_ -> id) (\_ -> id)
t2ix DivineFavour = updatestatus (#effects . #divinefavour) divinefavouron divinefavouroff
t2ix Poisened = updatestatus (#effects . #poisened) (\_ -> id) poisenedoff
t2ix ImmuneMindInfluencingEffects = updatestatus (#effects . #immuneMindInfluencingEffects) immuneMindInfluencingEffectson immuneMindInfluencingEffectsoff
t2ix Stunned = updatestatus (#effects . #stunned) stunnedon stunnedoff
t2ix StunningFist = updatestatus (#effects . #stunningfist) (\_ -> id) (\_ -> id)
t2ix Ki = updatestatus (#effects . #ki) kion kioff
t2ix Staggered = updatestatus (#effects . #staggered) (\_ -> id) (\_ -> id)
t2ix DarkHeal = updatestatus (#effects . #darkheal) darkhealon darkhealoff
t2ix Grappled = updatestatus (#effects . #grappled) grappledon grappledoff
t2ix Cover = updatestatus (#effects . #cover) coveron coveroff
t2ix Charged = updatestatus (#effects . #charged) chargedon chargedoff
t2ix Incorporal = updatestatus (#effects . #incorporal) (\_ -> id) (\_ -> id)
t2ix Pounce = updatestatus (#effects . #pounce) (\_ -> id) (\_ -> id)
t2ix Turned = updatestatus (#effects . #turned) (\_ -> id) (\_ -> id)
t2ix Commanded = updatestatus (#effects . #commanded) commandedon commandedoff
t2ix ChannelResistance = updatestatus (#effects . #channelresistance) (\_ -> id) (\_ -> id)
t2ix ShieldofFaith = updatestatus (#effects . #shieldoffaith) shieldoffaithon shieldoffaithoff
t2ix ProtectionFrom = updatestatus (#effects . #protectionfrom) (\_ -> id) (\_ -> id)
t2ix DefensiveStance = updatestatus (#effects . #defensivestance) defensivestanceon defensivestanceoff
t2ix ImprovedGrapple = updatestatus (#effects . #improvedgrapple) (\_ -> id) (\_ -> id)
t2ix ImprovedDisarm = updatestatus (#effects . #improveddisarm) (\_ -> id) (\_ -> id)
t2ix ImprovedTrip = updatestatus (#effects . #improvedtrip) (\_ -> id) (\_ -> id)
t2ix MageArmour = updatestatus (#effects . #magearmour) magearmouron magearmouroff
t2ix Regeneration = updatestatus (#effects . #regeneration) (\_ -> id) (\_ -> id)
t2ix Haste = updatestatus (#effects . #haste) (\_ -> id) (\_ -> id)
t2ix Slow = updatestatus (#effects . #slow) (\_ -> id) (\_ -> id)
t2ix Cleave = updatestatus (#effects . #cleave) (\_ -> id) (\_ -> id)
t2ix ImprovedDoubleAttack = updatestatus (#effects . #improveddoubleattack) (\_ -> id) (\_ -> id)
t2ix TripleAttack = updatestatus (#effects . #tripleattack) (\_ -> id) (\_ -> id)
t2ix PhotonMode = updatestatus (#effects . #photonmode) photonon photonoff
t2ix GravitonMode = updatestatus (#effects . #gravitonmode) gravitonon gravitonoff
t2ix HIPS = updatestatus (#effects . #hips) (\_ -> id) (\_ -> id)
t2ix TrickAttack = updatestatus (#effects . #trickattack) (\_ -> id) (\_ -> id)
t2ix FlatFooted = updatestatus (#effects . #flatfooted) (\_ -> id) (\_ -> id)
t2ix DarkMatter = updatestatus (#effects . #darkmatter) darkmatteron darkmatteroff