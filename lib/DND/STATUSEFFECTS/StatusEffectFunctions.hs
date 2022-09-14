{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}


module DND.STATUSEFFECTS.StatusEffectFunctions
where

import Control.Applicative (liftA2)
import Control.Lens (Lens', view, set, over)
import Data.Generics.Labels ()
import Data.Maybe (fromJust, isJust)

import DND.ATTACKS.Weapon (Weapon (Weapon), defaultWeapon)

import DND.CHARACTER.Status (Status, penalty, Team (Ally, Enemy, Enemy2, Bystander))

import DND.DAMAGEDICE.Elemental (removeprofile', modifyprofile', undeadelementalresistance)

import DND.STATUSEFFECTS.Effects ((&), Temporal, viewtempmodifyier, isNA, isPermanent, isActive, removena, replacetemporal, settemporal, addtemporal)

import DND.TemporaryValue (Temporary , temporary, addtemporary, removetemporary, revert)

updatestatus :: Lens' Status (Temporary (Temporal (Maybe Int))) -> ((Temporal (Maybe Int)) -> Status -> Status) -> ((Temporal (Maybe Int)) -> Status -> Status) -> Temporal (Maybe Int) -> Status -> Status
updatestatus lens f1' f2' temp s
    | isNA (temporary $ view lens s) = s
    | isPermanent (temporary $ view lens s) = s
    | isActive (temporary $ view lens s) && (isActive temp) && equalspermanentvalue = back2normal $ over lens (addtemporal temp) s
    | (not . isActive) (temporary $ view lens s) && ((not . isActive) temp) && equalspermanentvalue = back2normal $ over lens (addtemporal temp) s
    | isActive temp && ((not . isActive) (temporary $ view lens s)) && equalspermanentvalue = (back2normal . f1) $ over lens (addtemporal temp) s
    | (not . isActive) temp && (isActive (temporary $ view lens s)) && equalspermanentvalue = (back2normal . f2) $ over lens (addtemporal temp) s
    | isActive (temporary $ view lens s) && isActive temp = over lens (addtemporal temp) s
    | (not . isActive) (temporary $ view lens s) && (not . isActive) temp = over lens (addtemporal temp) s
    | isActive temp = f1 $ over lens (addtemporal temp) s
    | otherwise = f2 $ over lens (addtemporal temp) s
      where
        equalspermanentvalue = view (lens . #permanent) s == temp
        back2normal :: Status -> Status
        back2normal s' = over lens revert s'
        t
          | (not . isActive) temp = replacetemporal (temporary $ view (lens) s) temp
          | otherwise = temp
        f1 = f1' t
        f2 = f2' t

changestatus :: Lens' Status (Temporary (Temporal (Maybe Int))) -> ((Temporal (Maybe Int)) -> Status -> Status) -> ((Temporal (Maybe Int)) -> Status -> Status) -> Temporal (Maybe Int) -> Status -> Status
changestatus lens f1' f2' temp s
    | isActive (temporary $ view lens s) && isActive t = over lens (settemporal t) s
    | (not . isActive) (temporary $ view lens s) && (not . isActive) t = over lens (settemporal t) s
    | isActive t = f1 $ over lens (settemporal t) s
    | otherwise = f2 $ over lens (settemporal t) s
        where
          t
            | (not . isActive) temp = replacetemporal (temporary $ view (lens) s) temp
            | otherwise = temp
          f1 = f1' t
          f2 = f2' t

immuneMindInfluencingEffectsoff ::  (Temporal (Maybe Int)) -> Status -> Status
immuneMindInfluencingEffectsoff _ s = over (#effects . #sleep) removena $
  over (#effects . #fatigued) removena $
  over (#effects . #exhausted) removena $
  over (#effects . #bleeding) removena $
  over (#effects . #shaken) removena $
  over (#effects . #fear) removena $
  over (#effects . #sickened) removena $
  over (#effects . #nauseated) removena $
  over (#effects . #confused) removena $
  over (#effects . #laughter) removena $
  over (#effects . #dazed) removena $
  over (#effects . #despair) removena s

singoff :: Temporal (Maybe Int) -> Status -> Status
singoff t s
  | isActive t = id s
  | otherwise = over (#bonuses . #damage . #luck) (removetemporary go) $ over (#bonuses . #miscclass . #bab . #luck) (removetemporary go) s
    where
      go
        | ( viewtempmodifyier) t == Nothing = 0
        | otherwise = (fromJust . viewtempmodifyier) t

singon :: Temporal (Maybe Int) -> Status -> Status
singon t s
  | (not . isActive) t = id s
  | otherwise = over (#bonuses . #damage . #luck) (addtemporary go) $ over (#bonuses . #miscclass . #bab . #luck) (addtemporary go) s
    where
      go
        | (viewtempmodifyier) t == Nothing = 0
        | otherwise = (fromJust . viewtempmodifyier) t

bearstrengthoff ::  (Temporal (Maybe Int)) -> Status -> Status 
bearstrengthoff _ s = go
    where
        go = over (#bonuses . #abilityscores . #strength . #magic) (removetemporary $ 4) s

bearstrengthon ::  (Temporal (Maybe Int)) -> Status -> Status 
bearstrengthon _ s = go
  where
    go = over (#bonuses . #abilityscores . #strength . #magic) (addtemporary 4) s

keensensesoff ::  (Temporal (Maybe Int)) -> Status -> Status 
keensensesoff _ s = go
    where
        go = over (#bonuses . #skills . #perception . #magic) (removetemporary $ 2) s

keensenseson ::  (Temporal (Maybe Int)) -> Status -> Status 
keensenseson _ s = go
  where
    go = over (#bonuses . #skills . #perception . #magic) (addtemporary 2) s

catsgraceoff ::  (Temporal (Maybe Int)) -> Status -> Status 
catsgraceoff _ s = go
    where
        go = over (#bonuses . #abilityscores . #dexterity . #magic) (removetemporary $ 4) s

catsgraceon ::  (Temporal (Maybe Int)) -> Status -> Status 
catsgraceon _ s = go
  where
    go = over (#bonuses . #abilityscores . #dexterity . #magic) (addtemporary 4) s

bullsenduranceoff ::  (Temporal (Maybe Int)) -> Status -> Status 
bullsenduranceoff _ s = go
    where
        go = over (#bonuses . #abilityscores . #constitution . #magic) (removetemporary $ 4) s

bullsenduranceon ::  (Temporal (Maybe Int)) -> Status -> Status 
bullsenduranceon _ s = go
  where
    go = over (#bonuses . #abilityscores . #constitution . #magic) (addtemporary 4) s

blindoff ::  (Temporal (Maybe Int)) -> Status -> Status 
blindoff _ s = go
    where
        go =  penalty (#bonuses . #defense . #attacks . #penalty) (-2) s

blindon ::  (Temporal (Maybe Int)) -> Status -> Status 
blindon _ s = go
    where
        go =  penalty (#bonuses . #defense . #attacks . #penalty) 2 s

proneoff ::  (Temporal (Maybe Int)) -> Status -> Status 
proneoff _ s = go
    where
        go =  penalty (#bonuses . #miscclass . #bab . #penalty) (-4) s

proneon ::  (Temporal (Maybe Int)) -> Status -> Status 
proneon _ s = go
    where
        go =  penalty (#bonuses . #miscclass . #bab . #penalty) 4 s

disarmon ::  (Temporal (Maybe Int)) -> Status -> Status 
disarmon _ s = go
    where
        go = set (#primaryhand) defaultWeapon $ over (#otherweapons) ([Right $ Right (view (#primaryhand) s)] ++) s

shakenoff ::  (Temporal (Maybe Int)) -> Status -> Status 
shakenoff _ s = go
  where
    go = penalty (#bonuses . #miscclass . #bab . #penalty) (-2) $
          penalty (#bonuses . #defense . #saves . #penalty) (-2) s

getemon ::  (Temporal (Maybe Int)) -> Status -> Status 
getemon _ s = go
  where
    go = penalty (#bonuses . #defense . #attacks . #penalty) 2 s

getemoff ::  (Temporal (Maybe Int)) -> Status -> Status 
getemoff _ s = go
  where
    go = penalty (#bonuses . #defense . #attacks . #penalty) (-2) s

shakenon ::  (Temporal (Maybe Int)) -> Status -> Status 
shakenon _ s = go
  where
    go = penalty (#bonuses . #miscclass . #bab . #penalty) 2 $
          penalty (#bonuses . #defense . #saves . #penalty) 2 s

fearoff ::  (Temporal (Maybe Int)) -> Status -> Status 
fearoff _ s = go
  where
    go = penalty (#bonuses . #miscclass . #bab . #penalty) (-2) $
          penalty (#bonuses . #defense . #saves . #penalty) (-2) s

fearon ::  (Temporal (Maybe Int)) -> Status -> Status 
fearon _ s = go
  where
    go = penalty (#bonuses . #miscclass . #bab . #penalty) 2 $
          penalty (#bonuses . #defense . #saves . #penalty) 2 s

sickenedoff ::  (Temporal (Maybe Int)) -> Status -> Status 
sickenedoff _ s = go
  where
    go = penalty (#bonuses . #miscclass . #bab . #penalty) (-2) $
          penalty (#bonuses . #defense . #saves . #penalty) (-2) $
          penalty (#bonuses . #damage . #penalty) (-2) s

sickenedon ::  (Temporal (Maybe Int)) -> Status -> Status 
sickenedon _ s = go
  where
    go = penalty (#bonuses . #miscclass . #bab . #penalty) 2 $
          penalty (#bonuses . #defense . #saves . #penalty) 2 $
          penalty (#bonuses . #damage . #penalty) 2 s

maybeint2int :: Maybe Int -> Int
maybeint2int mi
  | isJust mi = fromJust mi
  | otherwise = 0

darkmatteroff :: Temporal (Maybe Int) -> Status -> Status
darkmatteroff temp s = over (#damagereduction) (fmap $ fmap $  (\x -> x & (Just (-2)))) s

darkmatteron :: Temporal (Maybe Int) -> Status -> Status
darkmatteron temp s = over (#damagereduction) (fmap $ fmap $  (\x -> x & (Just 2))) s
    
protectivewardoff :: Temporal (Maybe Int) -> Status -> Status
protectivewardoff temp s = over (#bonuses . #defense . #attacks . #deflection) (removetemporary modifyer) s
    where
      modifyer = (maybeint2int) $ viewtempmodifyier temp

protectivewardon :: Temporal (Maybe Int) -> Status -> Status
protectivewardon temp s = over (#bonuses . #defense . #attacks . #deflection) (addtemporary modifyer) s
  where
    modifyer = (maybeint2int) $ viewtempmodifyier temp

dominatedoff :: Temporal (Maybe Int) -> Status -> Status
dominatedoff temp s = over (#team) (removetemporary $ newteam modifyer) s
    where
      modifyer = viewtempmodifyier temp
      newteam mi
        | mi == (Just 1) = Ally
        | mi == (Just 2) = Enemy
        | mi == (Just 3) = Enemy2
        | mi == (Just 4) = Bystander
        | otherwise = Bystander

dominatedon :: Temporal (Maybe Int) -> Status -> Status
dominatedon temp s = over (#team) (addtemporary $ newteam modifyer) s
    where
      modifyer = viewtempmodifyier temp
      newteam mi
        | mi == (Just 1) = Ally
        | mi == (Just 2) = Enemy
        | mi == (Just 3) = Enemy2
        | mi == (Just 4) = Bystander
        | otherwise = Bystander

commandedoff :: Temporal (Maybe Int) -> Status -> Status
commandedoff temp s = over (#team) (removetemporary $ newteam modifyer) s
    where
      modifyer = viewtempmodifyier temp
      newteam mi
        | mi == (Just 1) = Ally
        | mi == (Just 2) = Enemy
        | mi == (Just 3) = Enemy2
        | mi == (Just 4) = Bystander
        | otherwise = Bystander

commandedon :: Temporal (Maybe Int) -> Status -> Status
commandedon temp s = over (#team) (addtemporary $ newteam modifyer) s
    where
      modifyer = viewtempmodifyier temp
      newteam mi
        | mi == (Just 1) = Ally
        | mi == (Just 2) = Enemy
        | mi == (Just 3) = Enemy2
        | mi == (Just 4) = Bystander
        | otherwise = Bystander

darkhealon :: (Temporal (Maybe Int)) -> Status -> Status
darkhealon _ s = over (#elementalresistance) (liftA2 modifyprofile' $ temporary <$> undeadelementalresistance) s

darkhealoff :: (Temporal (Maybe Int)) -> Status -> Status
darkhealoff _ s = over (#elementalresistance) (liftA2 removeprofile' $ temporary <$> undeadelementalresistance) s

coveron :: (Temporal (Maybe Int)) -> Status -> Status
coveron temp s = over (#concealment) (addtemporary int) s
  where
    int = maybeint2int $ viewtempmodifyier temp

coveroff :: (Temporal (Maybe Int)) -> Status -> Status
coveroff temp s = over (#concealment) (removetemporary int) s
  where
    int = maybeint2int $ viewtempmodifyier temp

chargedon :: (Temporal (Maybe Int)) -> Status -> Status
chargedon _ s = penalty (#bonuses . #defense . #attacks . #penalty) (-2) s

chargedoff :: (Temporal (Maybe Int)) -> Status -> Status
chargedoff _ s = penalty (#bonuses . #defense . #attacks . #penalty) 2 s

grappledon :: (Temporal (Maybe Int)) -> Status -> Status
grappledon _ s = penalty (#bonuses . #abilityscores . #dexterity . #penalty) 4 $
                penalty (#bonuses . #miscclass . #bab . #penalty) 2 s

grappledoff :: (Temporal (Maybe Int)) -> Status -> Status
grappledoff _ s = penalty (#bonuses . #abilityscores . #dexterity . #penalty) (-4) $
                penalty (#bonuses . #miscclass . #bab . #penalty) (-2) s

shieldoffaithoff :: Temporal (Maybe Int) -> Status -> Status
shieldoffaithoff temp s = penalty (#bonuses . #defense . #attacks . #penalty) (modifyer) s
    where
      modifyer = (maybeint2int) $ viewtempmodifyier temp

shieldoffaithon :: Temporal (Maybe Int) -> Status -> Status
shieldoffaithon temp s = penalty (#bonuses . #defense . #attacks . #penalty) (modifyer) s
  where
    modifyer = (maybeint2int) $ viewtempmodifyier temp

defensivestanceoff :: Temporal (Maybe Int) -> Status -> Status
defensivestanceoff _ s = over (#bonuses . #defense . #attacks . #deflection) (removetemporary 4) s

defensivestanceon :: Temporal (Maybe Int) -> Status -> Status
defensivestanceon _ s = over (#bonuses . #defense . #attacks . #deflection) (addtemporary 4) s