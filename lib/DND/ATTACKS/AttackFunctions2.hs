{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module DND.ATTACKS.AttackFunctions2
where

import Control.Lens (set, over, view)
import Data.Generics.Labels ()
import Control.Applicative
import Data.Maybe (fromJust, catMaybes)
import Data.List (delete)

import DND.Action (Action (Action))
import DND.TemporaryValue (temporary, modifytemporary)

import DND.ATTACKS.Attack
import DND.ATTACKS.DamageReduction (Material (Magical), DamageType (Slashing), dt2mat)
import DND.ATTACKS.Smite (Smite (Smite), Benificence (Evil, NeutraL), Lawfulness (Chaotic, Lawful, Neutral), Alignment (Al))
import DND.ATTACKS.Weapon

import DND.CHARACTER.AbilityScores (AbilityScores (strength, dexterity), abix)
import DND.CHARACTER.Bonuses (Bonuses (miscclass, damage))
import DND.CHARACTER.Class (ClassInfo (bab, bsb, bhb), getLevel)
import DND.CHARACTER.Status (Status (primclass), Size (Large, Small, Medium), gatherbonuses, abilitybonuses, size, emptyAttack, channelenergy, layonhands, effectivespelllevel, resolvepoints, damager')

import DND.DAMAGEDICE.DamageDice (DamageDice (D), SaveProfile (Negates, Half))
import DND.DAMAGEDICE.Elemental (Elemental (Negative, Positive, Fire))

import DND.DEFENSE.Armour (Armour)
import DND.DEFENSE.Defense (Defense (Defense, Will, Perception, CMD), Target (Target))
import DND.DEFENSE.DefenseFunctions (sized)

import DND.STATUSEFFECTS.Effect (Effect(Effect), cmdeffect)
import DND.STATUSEFFECTS.Effects (viewtempmodifyier, (&), isActive, Temporal (Absent, Off, Present), addtemporal, settemporal,
    StatusEffect (Invisible, Stealth, Sickened, Bleeding, GrowClaws, Stunned, StunningFist, Paralyzed, Staggered, Blind, Fatigued, Ki, Grappled, Prone, Disarmed), 
    isActive, isOn, (-$-), temporalduration)
import DND.STATUSEFFECTS.StatusEffectFunctions2 (t2ix)

 
s2a :: Bool -> Status ->  Attack Int [Int] -> (Status, Attack [Int] [Int])
s2a bool s a = ((snd envoyamount) newstatus7, 
  (Attack (AInfo n r inc t newmat tohit d c mabi newalign isprepared spellproxy) att newdam bonusdam mstd newmeff newmsdd mewseff3))
    where
      (Attack (AInfo n r inc t m b d c mabi _ isprepared spellproxy) att dam bonusdam mstd meff msdd mseff) = a
      newalign = temporary $ view (#alignment) s
      rangebonus
        | (isActive . temporary) (view (#effects . #weaponfinesse) s) && (r /= Missile) = 
          max (pure $ (dexterity . abilitybonuses) s) (pure $ (strength . abilitybonuses) s)
        | r == Missile = (pure $ (dexterity . abilitybonuses) s)
        | otherwise = (pure $ (strength . abilitybonuses) s)
      babbonus = (pure $ (temporary . bab . primclass) s)
      misctohitbonus = (pure $ (temporary . sum . bab . miscclass . gatherbonuses) s)
      envoyamount
            | view (#playerclasses . #env) s >= 20 && expertbool = (Just 8, damager' (#resolve) (Just 2))
            | view (#playerclasses . #env) s >= 17 && expertbool  = (Just 7, damager' (#resolve) (Just 2))
            | view (#playerclasses . #env) s >= 13 && expertbool  = (Just 6, damager' (#resolve) (Just 2))
            | view (#playerclasses . #env) s >= 9 && expertbool  = (Just 5, damager' (#resolve) (Just 2))
            | view (#playerclasses . #env) s >= 5 && expertbool  = (Just 4, damager' (#resolve) (Just 2))
            | expertbool = (Just 3, damager' (#resolve) (Just 2))
            | otherwise = (Nothing, id)
              where
                expertbool = (isActive . temporary $ view (#effects . #expertattack) s) && ((fst $ resolvepoints s) >= 2)
      miscdamagebonus = (temporary . sum . damage . gatherbonuses) s
      kistrikeactive = (isActive . temporary . view (#effects . #ki) $ s) && (((Just 1) ==) . viewtempmodifyier . temporary . view (#effects . #ki) $ s)
      tohit0 :: Maybe [Int]
      tohit0
        | (isActive . temporary) (view (#effects . #nauseated) s) = Nothing
        | bool = sequenceA [totalbonus]
        | (isActive . temporary) (view (#effects . #formofthedragon) s) = sequenceA [totalbonus]
        | otherwise = sequenceA $ take attackamount $ repeat (liftA2 (-) totalbonus multipenalty)
            where
                totalbonus = (rangebonus & babbonus & misctohitbonus & b & (pure deadlyaimpenalty) & (pure magicfangbonus) & (fst envoyamount))
                attackamount = if (isActive . temporary $ view (#effects . #tripleattack) s) then 3 else 2
                multipenalty = if (isActive . temporary $ view (#effects . #improveddoubleattack) s) 
                  then (Just 3) & (fmap negate . viewtempmodifyier. temporary $ view (#effects . #improveddoubleattack) s)
                  else (Just 3)
      tohit1 :: (Status, Maybe [Int])
      tohit1
        | (isActive . temporary) $ view (#effects . #trueStrike) s = (news, (++) <$> ((((+20) <$>) . take 1) <$> tohit0) <*> (drop 1 <$> tohit0))
        | otherwise = (s, tohit0)
                where
                    news = over (#effects . #trueStrike) (addtemporal (Absent Nothing [] Nothing)) s
      {-damagesize :: Size -> [Int] -> [Int]
      damagesize currentsize dice
        | currentsize == Medium = dice
        | currentsize <= Small = damagesize (succ currentsize) (shrinkdice dice)
        | currentsize >= Large = damagesize (pred currentsize) (increasedice dice)
        | otherwise = dice
          where
            shrinkdice y = (\x -> 2 * x `div` 3) <$> y
            increasedice y = (\x -> 3 * x `div` 2) <$> y-}
      strengthdamagebonus
        | r == Melee = (strength . abilitybonuses) s
        | otherwise = 0
      featconstant = max 1 (((temporary . bab . primclass) s) `div` 2)
      (deadlyaimbonus, deadlyaimpenalty)
        | (isActive . temporary) $ view (#effects . #deadlyaim) s = (featconstant, (-2))
        | otherwise = (0, 0)
      (arcanedamage, newmat)
        | (isActive . temporary) $ view (#effects . #arcanestrike) s = (max (((temporary . bsb . primclass) s) `div` 4 + 1) (((temporary . bhb . primclass) s) `div` 4 + 1), Magical)
        | otherwise = (0, m)
      abilitybonus
          | mabi == Nothing = 0
          | otherwise = (abix mabi) (abilitybonuses s)
      magicfangbonus
        | (isActive . temporary) (view (#effects . #magicfang) s) && (r == Melee) = 
          (maybeint2int $ (viewtempmodifyier . temporary) (view (#effects . #magicfang) s))
        | otherwise = 0
      totaldamagebonus = strengthdamagebonus + arcanedamage + miscdamagebonus + deadlyaimbonus + magicfangbonus + abilitybonus
      tohit = snd tohit1
      newstatus
        | (isActive . temporary) $ view (#effects . #invisible) s = t2ix Invisible (Absent Nothing [] Nothing) $ fst tohit1
        | otherwise = fst tohit1
      newstatus2
        | (isActive . temporary) $ view (#effects . #stealth) s = t2ix Stealth (Absent Nothing [] Nothing) $ newstatus
        | otherwise = newstatus
      newstatus3
        | (isOn . temporary) (view (#effects . #smiteevil) s) && (((== (Just 1)) . viewtempmodifyier . temporary) $ view (#effects . #smiteevil) s) = 
            over (#effects . #smiteevil) (settemporal $ Absent Nothing [] Nothing) $ newstatus2
        | (isOn . temporary) (view (#effects . #smiteevil) s) =  over (#effects . #smiteevil) (addtemporal $ Off Nothing [] Nothing) $
            over (#effects . #smiteevil) (modifytemporary (fmap (fmap (\x -> x - 1))))  newstatus2
        | otherwise = newstatus2
      newstatus4
        | (isOn . temporary) (view (#effects . #growclaws) s) && (((== (Just 1)) . viewtempmodifyier . temporary) $ view (#effects . #growclaws) s) = 
            over (#effects . #growclaws) (settemporal $ Absent Nothing [] Nothing) $ newstatus3
        | (isOn . temporary) (view (#effects . #growclaws) s) =  t2ix GrowClaws (Off Nothing [] Nothing) $
            over (#effects . #growclaws) (modifytemporary (fmap (fmap (\x -> x - 1))))  newstatus3
        | otherwise = newstatus3
      newstatus5
        | ((\x -> x == 1) . length) (filter (\x -> x == channelenergy) (view (#specialabilities) s)) &&
            ((getLevel $ view (#primclass . #level) s) >= 4) &&
            ((\x -> x > 3) . length) (filter (\x -> x == layonhands) (view (#specialabilities) s)) &&
            (isOn . temporary) (view (#effects . #channelsmite) s) = 
            over (#specialabilities) ((delete layonhands) . (delete layonhands)) $
            over (#effects . #channelsmite) (addtemporal $ Off Nothing [] Nothing) newstatus4
        | ((\x -> x == 1) . length) (filter (\x -> x == channelenergy) (view (#specialabilities) s)) &&
            ((getLevel $ view (#primclass . #level) s) >= 4) &&
            (isOn . temporary) (view (#effects . #channelsmite) s)  = 
            over (#specialabilities) ((delete channelenergy) . (delete layonhands) . (delete layonhands)) $
            over (#effects . #channelsmite) (addtemporal $ Off Nothing [] Nothing) newstatus4
        | (isOn . temporary) (view (#effects . #channelsmite) s) =
            over (#specialabilities) (delete channelenergy) $
            over (#effects . #channelsmite) (addtemporal $ Off Nothing [] Nothing) newstatus4
        | otherwise = newstatus4
      newstatus6
        | (isOn . temporary) (view (#effects . #stunningfist) s) && (((== (Just 1)) . viewtempmodifyier . temporary) $ view (#effects . #stunningfist) s) = 
            over (#effects . #stunningfist) (settemporal $ Absent Nothing [] Nothing) $ newstatus5
        | (isOn . temporary) (view (#effects . #stunningfist) s) =  t2ix StunningFist (Off Nothing [] Nothing) $
            over (#effects . #stunningfist) (modifytemporary (fmap (fmap (\x -> x - 1))))  newstatus5
        | otherwise = newstatus5
      newstatus7
        | kistrikeactive && (((== (Just 1)) . temporalduration . temporary) $ view (#effects . #ki) s) = 
            over (#effects . #ki) (settemporal $ Absent Nothing [] Nothing) $ newstatus6
        | kistrikeactive =  t2ix Ki (Off Nothing [] Nothing) $
            over (#effects . #ki) (modifytemporary ((-$-) (fmap (\x -> x - 1)))) newstatus6
        | otherwise = newstatus6
      newdam = over (#damagebonus) (+ totaldamagebonus) <$> dam
      newmsdd
        | (isActive . temporary) (view (#effects . #channelsmite) s) && 
            (((\x -> x >= 1) . length . (filter (\x -> x == channelenergy))) (view (#specialabilities) s)) =
            Just (D energydice 0 energytype (Just $ Target Will difficulty) Half)
        | otherwise = msdd
                where
                    level = max (effectivespelllevel (Just 2) s) (effectivespelllevel (Just 3) s)
                    energydice
                        | (isActive . temporary) (view (#effects . #stigmata) s) && ((getLevel $ view (#primclass . #level) s) >= 5) = take (((level + 1) `div` 2) + 1) $ cycle [6]
                        | otherwise = take ((level + 1) `div` 2) $ cycle [6]
                    energytype = Just Positive
                    difficulty = view (#charisma) (abilitybonuses s) + ((level + 1) `div` 2)
      newmseff
        | (isActive . temporary) (view (#effects . #stigmata) s) && ((getLevel $ view (#primclass . #level) s) >= 5) && (isActive . temporary) (view (#effects . #channelsmite) s) && 
            (((\x -> x >= 1) . length . (filter (\x -> x == channelenergy))) (view (#specialabilities) s)) =
            [(Effect Bleeding (Present (Just 4) [] [6]) Nothing (Just $ Target Will difficulty)), (Effect Sickened (Present (Just 4) [] [0]) Nothing (Just $ Target Will difficulty))] ++ mseff
        | otherwise = mseff
                where
                    level = (getLevel $ view (#primclass . #level) s) `div` 2
                    difficulty = view (#charisma) (abilitybonuses s) + level
      newmseff2
        | (isActive . temporary) (view (#effects . #stunningfist) s) && (level >= 20) =
            [(Effect Paralyzed (Present (Just 4) [] [0]) Nothing (Just $ Target Will difficulty))] ++ newmseff
        | (isActive . temporary) (view (#effects . #stunningfist) s) && (level >= 20) =
            [(Effect Blind (Present Nothing [] [0]) Nothing (Just $ Target Will difficulty))] ++ newmseff
        | (isActive . temporary) (view (#effects . #stunningfist) s) && (level >= 20) =
            [(Effect Staggered (Present (Just 4) [] [0]) Nothing (Just $ Target Will difficulty))] ++ newmseff
        | (isActive . temporary) (view (#effects . #stunningfist) s) && (level >= 20) =
            [(Effect Sickened (Present (Just 10) [] [0]) Nothing (Just $ Target Will difficulty))] ++ newmseff 
        | (isActive . temporary) (view (#effects . #stunningfist) s) && (level >= 20) =
            [(Effect Fatigued (Present Nothing [] [0]) Nothing (Just $ Target Will difficulty))] ++ newmseff 
        | (isActive . temporary) (view (#effects . #stunningfist) s) =
            [(Effect Stunned (Present (Just 1) [] [0]) Nothing (Just $ Target Will difficulty))] ++ newmseff
        | otherwise = newmseff
                where
                    level = getLevel $ view (#primclass . #level) s
                    difficulty = view (#wisdom) (abilitybonuses s) + (level `div` 2)
      sizebonus = negate . sized . size $ s
      mewseff3 = cmdeffect2 (sizebonus + (maybeint2int $ rangebonus & babbonus)) s <$> newmseff2
      newmeff = cmdeffect2 (sizebonus + (maybeint2int $ rangebonus & babbonus)) s <$> meff

cmdeffect2 :: Int -> Status -> Effect a -> Effect a
cmdeffect2 i s eff
  | se == Grappled && improvedgrapple = cmdeffect (i + 4) eff
  | se == Prone && improvedtrip = cmdeffect (i + 4) eff
  | se == Disarmed && improveddisarm = cmdeffect (i + 4)  eff
  | otherwise = cmdeffect i eff 
  where
    se = view (#statuseffect) eff
    improvedgrapple = isActive . temporary $ view (#effects . #improvedgrapple) s
    improvedtrip = isActive . temporary $ view (#effects . #improvedtrip) s
    improveddisarm = isActive . temporary $ view (#effects . #improveddisarm) s

w2a' :: Bool -> Status -> Weapon -> Attack Int [Int]
w2a' bool s weapon@(Weapon damtype wt (D dice bonus ele mtar sprof) inc en name) = es2a s en $ 
    Attack (AInfo name ran inc damtype (dt2mat damtype) tohit defense crit Nothing (Al Neutral NeutraL) isprepared spellproxybool) emptyAttack 
    (Just $ D dice (maybeint2int bdam1 + bonus) ele mtar sprof) [] Nothing Nothing Nothing []
      where
        wfp = view (#weaponbonus) s
        FP profile focus = wix wt $ wfp
        tohitbonus = if focus then focusbonus else if profile >= Proficient then Nothing else Just (- 4)
        focusbonus = if (getLevel $ view (#primclass . #level) s) - (temporary $ view (#primclass . #bab) s) > 3 then (Just 2) else (Just 1)
        tohit = tohitbonus
        ran = (rangefinder wt)
        crit = C 20 2 0
        bdam1
            | profile == Specialization && wt == SmallArm = Just . (\x -> x `div` 2) . sum $ view (#playerclasses) s
            | profile == Specialization = Just . sum $ view (#playerclasses) s
            | otherwise = Nothing
        isprepared
          | (isActive . temporary $ view (#effects . #defensivestance) s) &&
            ran == Melee = True
          | otherwise = False
        defense = case mtar of
          Nothing -> Defense
          Just y -> view (#defensetype) y
        spellproxybool = if wt == Grenade || Blasting `elem` en then (True,wt == Grenade) else (False,False)

maybeint2int :: Maybe Int -> Int
maybeint2int mi = case mi of
  Nothing -> 0
  Just y -> y

e2a :: Status -> Enchantment -> Attack Int [Int] -> Attack Int [Int]
e2a s enchantment (Attack info roll dam bdam mstd meff msdd mseff)
    | iselementalenchantment enchantment = Attack info roll dam go mstd meff msdd mseff
    | enchantment == Axiomatic = Attack info roll dam bdam mstd meff msdd mseff
    | enchantment == Anarchic = Attack info roll dam bdam mstd meff msdd mseff
    | enchantment == Distance = Attack dist roll dam bdam mstd meff msdd mseff
    | enchantment == Burst = Attack bur roll dam bdam mstd meff msdd mseff
    | enchantment == LongSite = Attack info roll dam bdam mstd meff msdd mseff
    | otherwise = (Attack info roll dam bdam mstd meff msdd mseff)
                where
                    go = bdam ++ [D [6] 0 (enchantment2elemental enchantment) Nothing Negates]
                    dist = over (#increment) (2 *) info
                    bur = over (#critical . #multiplier) (1 +) info

es2a :: Status -> [Enchantment] -> Attack Int [Int] -> Attack Int [Int]
es2a _ [] a = a
es2a s (e : es) a = es2a s es (e2a s e a)

weapon2listattack :: Status -> (Status, Action (Attack [Int]) [Int])
weapon2listattack s
  | beastform = Action <$> (s2a False s $w2a' False s (Weapon Slashing BasicMelee (damagedicefunc $ (\x -> x `div` 2) <$> beastmodifyer) 5 [] "Claws"))
  | elementalformiv = Action <$> (s2a False s $ w2a' False s (Weapon Slashing BasicMelee (damagedicefunc $ Just 2) 5 [elementalchoice modifyer] "Claws"))
  | elementalformiii = Action <$> (s2a False s $ w2a' False s (Weapon Slashing BasicMelee (damagedicefunc $ Just 2) 5 [] "Claws"))
  | dragonform = Action <$> (s2a True s $ w2a' True s (Weapon Slashing BasicMelee (damagedicefunc $ Just 3) 5 [] "Claws"))
  | clawsgrown = Action <$> (s2a False s $ over (#ddice) (set (#damagedice) [4] <$>) (w2a' False s claws))
  | polymorthed = Action <$> (s2a False s (w2a' False s (Weapon Slashing BasicMelee (D [3] 0 Nothing (Just $ Target Defense 0) Negates) 5 [] "Claws")))
  | otherwise = Action <$> (s2a False s $ w2a' False s weapon)
  where
    damagedicefunc mint = (D (take (max 1 $ level `div` 4) (repeat 12)) (maybeint2int $ mint & (Just level)) Nothing (Just $ Target Defense 0) Negates)
    level = getLevel $ view (#primclass . #level) s
    weapon = (view (#primaryhand) s)
    polymorthed = (isActive . temporary) (view (#effects . #polymorth) s) 
    clawsgrown = (isActive . temporary) (view (#effects . #growclaws) s)
    dragonform = (isActive . temporary) (view (#effects . #formofthedragon) s)
    elementalformiii = ((isActive . temporary) (view (#effects . #elementalbodyiii) s))
    elementalformiv = ((isActive . temporary) (view (#effects . #elementalbodyiv) s))
    beastform = ((isActive . temporary) (view (#effects . #beastshape) s))
    clawfunction :: Elemental -> Int -> Weapon
    clawfunction elemen i
      | i >= 11 = Weapon Slashing BasicMelee (damagedicefunc Nothing) 5 [elemental2enchantment elemen] "Claws"
      | i >= 5 = Weapon Slashing BasicMelee  (damagedicefunc Nothing) 5 [] "Claws"
      | otherwise = Weapon Slashing BasicMelee (damagedicefunc Nothing) 5 [] "Claws"
    claws = clawfunction Fire level
    elementalchoice mi
        | mi == (Just 1) = Thundering   
        | mi == (Just 2) = Icy
        | mi == (Just 3) = Fiery
        | mi == (Just 4) = Acidic
        | otherwise = Acidic
    modifyer = (viewtempmodifyier . temporary) (view (#effects . #elementalbodyiv) s)
    beastmodifyer = (viewtempmodifyier . temporary) (view (#effects . #beastshape) s)

weapon2listattacks :: Status -> (Status, [Action (Attack [Int]) [Int]])
weapon2listattacks s = (++ go3) <$> (pure <$> weapon2listattack s)
    where
        damagedicefunc mint = (D (take (max 1 $ level `div` 4) (repeat 12)) (maybeint2int $ mint & (Just level)) Nothing (Just $ Target Defense 0) Negates)
        level = getLevel $ view (#primclass . #level) s
        dragonform = (isActive . temporary) (view (#effects . #formofthedragon) s)
        go3 
            | dragonform =
                [Action (snd (s2a True s (w2a' True s (Weapon Slashing BasicMelee (damagedicefunc $ Just 3) 5 [] "Bite")))),
                Action (snd (s2a True s (w2a' True s (Weapon Slashing BasicMelee (damagedicefunc $ Just 3) 5 [] "Claws")))),
                Action (snd (s2a True s (over (#ddice) (set (#damagedice) [4] <$>) $ w2a' True s (Weapon Slashing BasicMelee (damagedicefunc $ Just 3) 5 [] "Tail")))),
                Action (snd (s2a True s (over (#ddice) (set (#damagedice) [4] <$>) $ w2a' True s (Weapon Slashing BasicMelee (damagedicefunc $ Just 3) 5 [] "Wing Buffet"))))]
            | otherwise = []

weapon21attack :: Status -> (Status, Action (Attack [Int]) [Int])
weapon21attack s = Action <$> (s2a True s $ w2a' True s (view (#primaryhand) s))

weapon21attacks :: Status -> (Status, [Action (Attack [Int]) [Int]])
weapon21attacks s = (pure <$> weapon21listattack s)
  where
    dragonform = (isActive . temporary) (view (#effects . #formofthedragon) s)
    polymorthed = (isActive . temporary) (view (#effects . #polymorth) s)
    elementalformiii = ((isActive . temporary) (view (#effects . #elementalbodyiii) s))
    elementalformiv = ((isActive . temporary) (view (#effects . #elementalbodyiv) s))
    beastform = ((isActive . temporary) (view (#effects . #beastshape) s))

weapon21listattack :: Status -> (Status, Action (Attack [Int]) [Int])
weapon21listattack s
  | beastform = Action <$> (s2a True s $ w2a' True s (Weapon Slashing BasicMelee (damagedicefunc $ (\x -> x `div` 2) <$> beastmodifyer) 5 [] "Claws"))
  | elementalformiv = Action <$> (s2a True s $ w2a' True s (Weapon Slashing BasicMelee (damagedicefunc $ Just 2) 5 [elementalchoice modifyer]"Claws"))
  | elementalformiii = Action <$> (s2a True s $ w2a' True s (Weapon Slashing BasicMelee (damagedicefunc $ Just 2) 5 [] "Claws"))
  | dragonform = Action <$> (s2a True s $ w2a' True s (Weapon Slashing BasicMelee (damagedicefunc $ Just 3) 5 [] "Claws"))
  | clawsgrown = Action <$> (s2a True s $ over (#ddice) (set (#damagedice) [4] <$>) (w2a' True s claws))
  | polymorthed = Action <$> (s2a True s (w2a' True s (Weapon Slashing BasicMelee (damagedicefunc Nothing) 5 [] "Claws")))
  | otherwise = weapon21attack s
  where
    damagedicefunc mint = (D (take (max 1 $ level `div` 4) (repeat 12)) (maybeint2int $ mint & (Just level)) Nothing (Just $ Target Defense 0) Negates)
    level = getLevel $ view (#primclass . #level) s
    polymorthed = (isActive . temporary) (view (#effects . #polymorth) s) 
    clawsgrown = (isActive . temporary) (view (#effects . #growclaws) s)
    dragonform = (isActive . temporary) (view (#effects . #formofthedragon) s)
    elementalformiii = ((isActive . temporary) (view (#effects . #elementalbodyiii) s))
    elementalformiv = ((isActive . temporary) (view (#effects . #elementalbodyiv) s))
    beastform = ((isActive . temporary) (view (#effects . #beastshape) s))
    clawfunction :: Elemental -> Int -> Weapon
    clawfunction elemen i
      | i >= 11 = Weapon Slashing BasicMelee (damagedicefunc Nothing) 5 [elemental2enchantment elemen] "Claws"
      | i >= 5 = Weapon Slashing BasicMelee (damagedicefunc Nothing) 5 [] "Claws"
      | otherwise = Weapon Slashing BasicMelee (damagedicefunc Nothing) 5 [] "Claws"
    claws = clawfunction Fire level
    elementalchoice mi
        | mi == (Just 1) = Thundering   
        | mi == (Just 2) = Icy
        | mi == (Just 3) = Fiery
        | mi == (Just 4) = Acidic
        | otherwise = Acidic
    modifyer = (viewtempmodifyier . temporary) (view (#effects . #elementalbodyiv) s)
    beastmodifyer = (viewtempmodifyier . temporary) (view (#effects . #beastshape) s)

updateattack :: Maybe Int -> Maybe Int -> Action (Attack [Int]) [Int] -> Action (Attack [Int]) [Int]
updateattack mi1 mi2 (Action att) = Action (over (#info . #attackbonus) (fmap (((maybeint2int mi1) +) <$>)) $ over (#ddice) ((over (#damagebonus) ((maybeint2int mi2) +)) <$>) att)

longshot :: Maybe Int -> Action (Attack [Int]) [Int] -> Maybe (Action (Attack [Int]) [Int])
longshot mi a = go
    where
        range = view (#runF . #info . #increment) a
        go =  if fmap (*5) mi <= (Just range) then Just a else Nothing

longshots :: Maybe Int -> [Action (Attack [Int]) [Int]] -> [Action (Attack [Int]) [Int]]
longshots mi as = catMaybes $ fmap (longshot mi) as

chargeupdate :: WeaponType -> Action (Attack [Int]) [Int] -> Action (Attack [Int]) [Int]
chargeupdate wt (Action att)
  | wt == AdvancedMelee = updateattack (Just 2) Nothing (Action $ over (#ddice) (fmap (fmap go)) att)
  | otherwise = updateattack (Just 2) Nothing (Action att)
  where
    go bs = case bs of
      [] -> []
      (c : cs) -> (c : c : go cs)