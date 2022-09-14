{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

module DND.SPELLS.Summons
where

import Control.Lens (view, set, over)
import Data.List.NonEmpty (NonEmpty ((:|)))

import DND.ATTACKS.Attack (Attack (Attack), AttackInfo (AInfo))
import DND.ATTACKS.DamageReduction (DamageReduction , Material (Norm, Slash, Bludgeon, Reduc, Magical, Pierce, GooD), DamageType (Piercing, Slashing, Bludgeoning, Reduced), noReductions, dredux)
import DND.ATTACKS.Smite (Alignment (Al), Lawfulness (Lawful, Neutral, Chaotic), Benificence (Evil, NeutraL), Race (Fey, Dragon, Human, Animal, Outsider, UnDead, Plant, Construct, Ysoki))
import DND.ATTACKS.Weapon (Weapon (Weapon), allproficient, WeaponType (BasicMelee, SmallArm, HeavyWeapon, LongArm, Grenade),
       Range (Missile, Melee), Critical (C), WeaponFeat (Specialization), Enchantment (Fiery))

import DND.CHARACTER.AbilityScores (as, Ability (Strength, Constitution, Intelligence, Charisma, Dexterity))
import DND.CHARACTER.Bonuses (Bonuses, BonusTypes)
import DND.CHARACTER.ClassFeatures (ClassFeatures)
import DND.CHARACTER.ClassUpdate2 (classupdate)
import DND.CHARACTER.Class (ClassInfo (ClassInfo), Classes (Classes), collapseclasses, fighter, rogue, getLevel)
import DND.CHARACTER.Status (Status (Status), emptystatus, Health (Healthy), Team (..), Size (Medium, Small, Large, Huge, Tiny), SpellTemplate, combatmaneuver, 
       emptyAttack, abilitybonuses, emptyclassfeature, baseskills, Vision (Vision), VisionCategory (NormalVision, Dark, Low), effectivespelllevel, 
       MagicItem (MagicItem), EquipmentArea (RightRing, Helmet), Equipment)

import DND.DAMAGEDICE.DamageDice (DamageDice (D), SaveProfile (Negates, Half), defaultDamageDice)
import DND.DAMAGEDICE.Elemental (Elemental (..), undeadelementalresistance, normalelementalresistance, Profile (..), ElementalResistance, modifyprofile')

import DND.DEFENSE.Armour (Armour (A))
import DND.DEFENSE.Defense (Defense (Defense, CMD, Reflex, Fortitude, Touch, Will), Target (Target))

import DND.SPELLS.Spell (SkillFeatProfile (None), Spell (Spell), SpellInfo (SpellInfo), SpellSchool (Conjuration, Evocation, Transmutation, Enchantment), emptyspellroll, Summon (Summon), SpellTarget (..), SpellArea (Line, Sphere, Cone, Bursty))
import DND.SPELLS.SpellFunctions (curewounds, close, flashpowder, abix, coneofcoldwand, improvedinvisibility, confusion, haste,  fireball, curewounds, mirrorimage, catsgrace, 
       burninghands)

import DND.STATDAMAGE.StatDamage (StatDamage (SD), StatDamageType (Disease, Poisen))

import DND.STATUSEFFECTS.Effects (Effects, Temporal (Permanent, Present, NA, Absent, On), 
       StatusEffect (Dominated, Prone, Rage, Paralyzed, Held, Entangled, Confused, Grappled, Staggered, 
       Nauseated, Sleep, Poisened, Fear, TripleAttack, ImprovedDoubleAttack, CombatShot, ImmunetoPoisen, 
       Slow, Cleave, ImmunetoDiseae ), isundead, noEffects, (&))
import DND.STATUSEFFECTS.Effect (Effect (Effect))
import DND.STATUSEFFECTS.StatusEffectFunctions2 (t2ix)

import DND.TemporaryValue (Temporary, temporarize, temporary)

creaturefinder :: String -> Status
creaturefinder name
    | name == "Skeleton" = skeleton
    | name == "Dire Rat" = direrat
    | name == "Wolf" = wolf
    | name == "Wolverine" = wolverine
    | name == "Griffon" = griffon
    | name == "Large Earth Elemental" = largeearthelemental
    | name == "Elephant" = elephant
    | name == "Vulture" = vulture
    | otherwise = emptystatus


undead :: Status -> Status 
undead s =  over (#elementalresistance) (modifyprofile' <$> (temporary <$> undeadelementalresistance) <*>) $
  set (#race) (pure UnDead) $
  set (#effects . #immunetocriticals) (pure $ Permanent Nothing [] Nothing) $
  set (#effects . #immunetodisease) (pure $ Permanent Nothing [] Nothing) $
  set (#effects . #immunetosneakattack) (pure $ Permanent Nothing [] Nothing) $
  set (#effects . #immunetopoisen) (pure $ Permanent Nothing [] Nothing) $
  set (#effects . #sleep) (pure $ NA Nothing [] Nothing) $
  set (#effects . #fatigued) (pure $ NA Nothing [] Nothing) $
  set (#effects . #exhausted) (pure $ NA Nothing [] Nothing) $
  set (#effects . #bleeding) (pure $ NA Nothing [] Nothing) $
  set (#effects . #shaken) (pure $ NA Nothing [] Nothing) $
  set (#effects . #fear) (pure $ NA Nothing [] Nothing) $
  set (#effects . #sickened) (pure $ NA Nothing [] Nothing) $
  set (#effects . #nauseated) (pure $ NA Nothing [] Nothing) $
  set (#effects . #confused) (pure $ NA Nothing [] Nothing) $
  set (#effects . #laughter) (pure $ NA Nothing [] Nothing) $
  set (#effects . #dazed) (pure $ NA Nothing [] Nothing) $
  set (#effects . #despair) (pure $ NA Nothing [] Nothing) $
  set (#effects . #immuneMindInfluencingEffects) (pure $ Permanent Nothing [] Nothing) $
  set (#effects . #undead) (pure $ Permanent Nothing [] Nothing) $
  set (#effects . #turned) (pure $ Absent Nothing [] Nothing) $
  set (#effects . #commanded) (pure $ Absent Nothing [] Nothing) s

construct :: Status -> Status 
construct s =  set (#race) (pure Construct) $
  set (#effects . #immunetocriticals) (pure $ Permanent Nothing [] Nothing) $
  set (#effects . #immunetodisease) (pure $ Permanent Nothing [] Nothing) $
  set (#effects . #immunetosneakattack) (pure $ Permanent Nothing [] Nothing) $
  set (#effects . #immunetopoisen) (pure $ Permanent Nothing [] Nothing) $
  set (#effects . #sleep) (pure $ NA Nothing [] Nothing) $
  set (#effects . #fatigued) (pure $ NA Nothing [] Nothing) $
  set (#effects . #exhausted) (pure $ NA Nothing [] Nothing) $
  set (#effects . #bleeding) (pure $ NA Nothing [] Nothing) $
  set (#effects . #shaken) (pure $ NA Nothing [] Nothing) $
  set (#effects . #fear) (pure $ NA Nothing [] Nothing) $
  set (#effects . #sickened) (pure $ NA Nothing [] Nothing) $
  set (#effects . #nauseated) (pure $ NA Nothing [] Nothing) $
  set (#effects . #confused) (pure $ NA Nothing [] Nothing) $
  set (#effects . #laughter) (pure $ NA Nothing [] Nothing) $
  set (#effects . #dazed) (pure $ NA Nothing [] Nothing) $
  set (#effects . #despair) (pure $ NA Nothing [] Nothing) $
  set (#effects . #immuneMindInfluencingEffects) (pure $ Permanent Nothing [] Nothing) $
  set (#effects . #immunetomagic) (pure $ Permanent Nothing [] Nothing) s

elementaleffects :: Effects (Temporary (Temporal (Maybe Int)))
elementaleffects = set (#immunetocriticals) (pure $ Present Nothing [] Nothing :: Temporary (Temporal (Maybe Int))) $
                    set (#immunetosneakattack) (pure $ Present Nothing [] Nothing :: Temporary (Temporal (Maybe Int))) $
                    set (#bleeding) (pure $ NA Nothing [] Nothing :: Temporary (Temporal (Maybe Int))) noEffects

classify :: Status -> ClassInfo (Temporary Int)
classify a = collapseclasses $ (pure <$>) (view (#playerclasses) a)

skeletonchampion :: Status
skeletonchampion =  undead . classupdate $ Status (pure . pure $ 0 :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision Dark 60)
            (pure Enemy)
            (as 17 14 0 13 8 14)
            (set (#defense . #attacks . #deflection) (pure 2 :: Temporary Int) (((pure . pure . pure) 0) :: Bonuses (BonusTypes (Temporary Int))))
            (A 5 5 (Just 4) Nothing "ChainMail")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Weapon" )
            [Right . Right $ Weapon Slashing BasicMelee defaultDamageDice 5 [] "Weapon"] 
            (set (#grenade . #weaponfeat) Specialization allproficient)
            (pure None)
            0
            0
            0
            skeletonresistance
            (set (#channelresistance) (pure $ Present Nothing [] Nothing :: Temporary (Temporal (Maybe Int))) isundead)
            (dredux (\x _ -> pure x) Bludgeon 5 noReductions)
            (pure (Al Lawful Evil))
            (pure Human)
            cfeatures
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (pure $ combatmaneuver)
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills
            where
                skeletonresistance :: ElementalResistance (Temporary (Profile (Maybe Int)))
                skeletonresistance = set (#ice) (pure Immune :: Temporary (Profile (Maybe Int))) normalelementalresistance
                cfeatures :: ClassFeatures SpellTemplate Int
                cfeatures = set (#feats . #deadlyaim) True $ set (#feats . #improvedinitiative) True $ 
                     set (#feats . #improveddisarm) True emptyclassfeature

pirate :: Status
pirate =  Status (rogue 1)
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Enemy)
            (as 12 14 12 10 10 8)
            ((pure . pure . pure) 0 )
            (A 0 0 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Chaotic Evil))
            (pure Human)
            (pure 0)
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (pure $ combatmaneuver)
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#perception) (3 :: Int) $ baseskills)

skeleton :: Status
skeleton =  undead $ Status (fighter 1)
            Nothing
            (pure $ Vision Dark 60)
            (pure Enemy)
            (as 15 14 0 0 10 10)
            ((pure . pure . pure) 0 )
            (A 4 4 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Weapon" )
            [(Right . Right $ Weapon Slashing SmallArm defaultDamageDice 5 [] "weapon")] 
            allproficient
            (pure None)
            0
            0
            0
            skeletonresistance
            isundead
            (dredux (\x _ -> pure x) Bludgeon 5 noReductions)
            (pure (Al Lawful Evil))
            (pure Human)
            (pure 0)
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (pure $ combatmaneuver)
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills
            where
                skeletonresistance :: ElementalResistance (Temporary (Profile (Maybe Int)))
                skeletonresistance = set (#ice) (pure Immune :: Temporary (Profile (Maybe Int))) normalelementalresistance


goldenskeleton :: Status
goldenskeleton =  undead $ Status (fighter 1)
            Nothing
            (pure $ Vision Dark 60)
            (pure Enemy)
            (as 15 8 0 0 8 18)
            ((pure . pure . pure) 0 )
            (A 7 7 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [Fiery] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            skeletonresistance
            isundead
            (dredux (\x _ -> pure x) Bludgeon 5 noReductions)
            (pure (Al Lawful Evil))
            (pure Human)
            (pure 0)
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (pure $ combatmaneuver)
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills
            where
                skeletonresistance :: ElementalResistance (Temporary (Profile (Maybe Int)))
                skeletonresistance = set (#fire) (pure Immune :: Temporary (Profile (Maybe Int))) $ 
                                     set (#ice) (pure Vulnerable :: Temporary (Profile (Maybe Int))) $
                                     set (#lightning) (pure Vulnerable :: Temporary (Profile (Maybe Int))) normalelementalresistance


summonskeleton :: SpellTemplate
summonskeleton mi _ s = Spell
    (SpellInfo  "Summon Skeleton" 2 Conjuration 0 0 Nothing Caster)
    emptyspellroll (False,False)
    Nothing
    Nothing
    Nothing
    (Just $ Summon "Skeleton" (10 * level) 1)
    Nothing
    True
        where
            level = effectivespelllevel mi s

nixie :: Status
nixie =  Status (ClassInfo (pure 7) (pure 7) (pure 1) (pure 0) (pure 3) (pure 3) (temporarize 1) (pure 1) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision Dark 60)
            (pure Enemy)
            (as 7 16 13 12 13 18)
            (set (#spellresistance . #misc) (pure 12) ((pure . pure . pure) 0 :: Bonuses (BonusTypes (Temporary Int))) :: Bonuses (BonusTypes (Temporary Int)))
            (A 0 0 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Weapon" )
            [Right . Right $ Weapon Slashing SmallArm defaultDamageDice 30 [] "Weapon"] 
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            (dredux (\x _ -> pure x) Pierce 5 noReductions :: DamageReduction (Temporary (Maybe Int)))
            (pure (Al Neutral NeutraL))
            (pure Fey)
            (pure 0)
            []
            [nixiecharmperson]
            (pure Small)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (combatmaneuver:|[])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#stealth) (9 :: Int) . set (#perception) (5 :: Int) $ baseskills)
            where
nixiecharmperson :: SpellTemplate
nixiecharmperson _ _ s = Spell (SpellInfo ("Nixie Charm Person") 2 Enchantment 5 0 Nothing (SingleTarget 9)) emptyspellroll (True,False)
                 Nothing Nothing (Just $ Effect Dominated (Present (Just 10) [] [0]) go2 (Just $ Target Will 0)) Nothing Nothing True
                     where
                      go2
                        | temporary (view (#team) s) == Ally = Just 1
                        | temporary (view (#team) s) == Enemy = Just 2
                        | temporary (view (#team) s) == Enemy2 = Just 3
                        | temporary (view (#team) s) == Bystander = Just 4
                        | otherwise = Just 4

direrat :: Status
direrat =  Status (ClassInfo (pure 4) (pure 4) (pure 0) (pure 2) (pure 2) (pure 0) (temporarize 1) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision Dark 60)
            (pure Enemy)
            (as 10 17 13 2 13 4)
            ((pure . pure . pure) 0 )
            (A 0 0 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing SmallArm defaultDamageDice 30 [] "Basic Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Neutral NeutraL))
            (pure Animal)
            (pure 0)
            []
            []
            (pure Small)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (pure $ bite)
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#stealth) (8 :: Int) . set (#perception) (3 :: Int) $ baseskills)
            where
                bite :: Attack Int [Int]
                bite = Attack (AInfo "Bite" Melee 5 Piercing Reduc (Just 1) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False)) 
                       emptyAttack (Just $ D [4] 0 Nothing Nothing Negates) [] Nothing Nothing Nothing []

{- ClassInfo { vp :: a
            , hp :: a
            , bab :: a
            , fort :: a
            , ref :: a
            , wil :: a
            , level :: a
            , bsb :: a
            , bhb :: a 
            , stealth :: a
            , perception :: a -}

initbonus :: Int -> Bonuses (BonusTypes (Temporary Int)) -> Bonuses (BonusTypes (Temporary Int))
initbonus i bonu = set (#initiative . #size) (pure i) bonu

spellresistancebonus :: Int -> Bonuses (BonusTypes (Temporary Int)) -> Bonuses (BonusTypes (Temporary Int))
spellresistancebonus i bonu = set (#spellresistance . #size) (pure i) bonu

synapticaccel :: MagicItem Int
synapticaccel = MagicItem (set (#abilityscores . #dexterity . #item) (2:: Int)
      ((pure . pure $ 0) :: Bonuses (BonusTypes Int)) :: Bonuses (BonusTypes Int)) 
      [] "Synaptic Accelerator" RightRing

charsihead :: MagicItem Int
charsihead = MagicItem (set (#abilityscores . #charisma . #item) (2:: Int)
      ((pure . pure $ 0) :: Bonuses (BonusTypes Int)) :: Bonuses (BonusTypes Int)) 
      [] "Headband of Charisma" Helmet

hGustweld :: Status
hGustweld = t2ix ImmunetoPoisen (Present Nothing [] Nothing) . 
            t2ix ImmunetoDiseae (Present Nothing [] Nothing) $ 
            Status (ClassInfo (pure 132) (pure 42) (pure 17) (pure 14) (pure 11) (pure 8) (temporarize 15) (pure 0) (pure 0))
            Nothing
            (pure $ Vision Dark 60)
            (pure Enemy)
            (as 31 12 23 8 12 9)
            (initbonus 2 $ spellresistancebonus (26 :: Int) ((pure . pure . pure) 0))
            (A 18 20 (Just 4) (Just 3) "Golemforged plating V")
            (set (#rightring) (Just synapticaccel) $ set (#helmet) (Just charsihead) (pure Nothing :: Equipment (Maybe (MagicItem Int))))
            (Weapon Piercing HeavyWeapon (D [12,12,12,12] 22 Nothing (Just $ Target Defense 0) Negates) 30 [] "Heavy Cannon" )
            [] 
            allproficient
            (pure None)
            0
            0
            0
            (set (#acid) (pure $ NormalDamage (Just 20)) normalelementalresistance)
            noEffects
            (dredux (\x _ -> pure x) Magical 10 noReductions)
            (pure (Al Lawful Evil))
            (pure Human)
            (pure 0)
            []
            [breathweaponmulti Acid [6,6,6,6,6,6,6,6] 11]
            (pure Huge)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (pure $ slam)
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#perception) (23 :: Int) baseskills)
            where
                slam :: Attack Int [Int]
                slam = Attack (AInfo "Slam" Melee 5 Bludgeoning Reduc (Just 0) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False)) 
                       emptyAttack (Just $ D [10,10,10,10,10] 15 Nothing Nothing Negates) [] (Just $ SD (Just Disease) (Just $ Target Fortitude 13) (Just Dexterity) [3] [3] 0 [0] 7) Nothing Nothing []

breathweaponmulti :: Elemental -> [Int] -> Int -> SpellTemplate
breathweaponmulti elemen listint int _ _ _ = Spell
                     (SpellInfo  ("Breath: " ++ (show elemen) ++ " " ++ (show listint) ++ " " ++ (show int)) 2 Evocation int 0 Nothing (AllInRange Cone 40))
                     emptyspellroll (True,False)
                     (Just $ D listint 0 Nothing (Just $ Target Reflex 0) Half)
                     Nothing
                     Nothing
                     Nothing
                     Nothing
                     True

ysokiAlchemist :: Status
ysokiAlchemist =  t2ix ImmunetoPoisen (Present Nothing [] Nothing) . 
            t2ix ImmunetoDiseae (Present Nothing [] Nothing) $ 
            Status (ClassInfo (pure 49) (pure 27) (pure 8) (pure 9) (pure 9) (pure 6) (temporarize 11) (pure 0) (pure 0))
            Nothing
            (pure $ Vision Dark 60)
            (pure Enemy)
            (as 13 19 14 18 10 8)
            (spellresistancebonus 22 ((pure . pure . pure) 0))
            (A 11 13 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Reduced Grenade (D [6,6,6,6] 0 (Just Fire) (Just $ Target Reflex 0) Half) 15 [] "Frag Grenade IV" )
            [Right . Right $ Weapon Slashing BasicMelee (D [6,6,6] 12 Nothing (Just $ Target Reflex 0) Half) 15 [] "Ultra-Thin Dueling Sword"] 
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            (dredux (\x _ -> pure x) Pierce 10 noReductions)
            (pure (Al Lawful Evil))
            (pure Ysoki)
            cfeatures
            []
            []
            (pure Small)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (pure $ combatmaneuver)
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#perception) (14 :: Int) baseskills)
            where
                     cfeatures = over (#mysticspells . #known . #second) (++ [mirrorimage, catsgrace, burninghands]) $
                              set (#mysticspells . #perday . #second) (3 :: Int) $
                              over (#mysticspells . #known . #third) (++ [(curewounds 3), haste,  fireball]) $
                              set (#mysticspells . #perday . #third) (3 :: Int) $
                              over (#mysticspells . #known . #fourth) (++ [improvedinvisibility, confusion]) $
                              set (#mysticspells . #perday . #fourth) (2 :: Int) $ emptyclassfeature

iceDevil :: Status
iceDevil =  t2ix ImmunetoPoisen (Present Nothing [] Nothing) . 
            t2ix Cleave (On Nothing [] Nothing) $ Status (ClassInfo (pure 96) (pure 18) (pure 14) (pure 9) (pure 9) (pure 6) (temporarize 13) (pure 0) (pure 0))
            Nothing
            (pure $ Vision Dark 60)
            (pure Enemy)
            (as 23 21 22 25 22 20)
            (spellresistancebonus 24 $ initbonus 4 ((pure . pure . pure) 0))
            (A 17 19 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Piercing LongArm (D [8,8,8,8] 13 Nothing (Just $ Target Defense 0) Negates) 60 [] "Magnetar rifle; advanced" )
            [] 
            allproficient
            (pure None)
            0
            0
            0
            (set (#fire) (pure Immune :: Temporary (Profile (Maybe Int))) . 
            set (#ice) (pure Immune :: Temporary (Profile (Maybe Int))) . 
            set (#acid) ((pure $ NormalDamage (Just 10)) :: Temporary (Profile (Maybe Int))) . 
            set (#positive) ((pure $ NormalDamage (Just 0)) :: Temporary (Profile (Maybe Int))) $ normalelementalresistance)
            noEffects
            (dredux (\x _ -> pure x) GooD 10 noReductions)
            (pure (Al Lawful Evil))
            (pure Outsider)
            (pure 0)
            []
            [coneofcoldwand 13 10]
            (pure Large)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (pure $ frostspear)
            combatmaneuver
            combatmaneuver
            0
            30
            0
            (Just auraoffear)
            (set (#perception) (22 :: Int) baseskills)
            where
                frostspear :: Attack Int [Int]
                frostspear = Attack (AInfo "Frost Spear" Melee 5 Piercing Reduc (Just 1) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False)) 
                       emptyAttack (Just $ D [10,10,10] 13 Nothing Nothing Negates) [D [6,6] 0 (Just Ice) Nothing Negates] Nothing 
                       (Just $ Effect Slow (Present (Just 3) [] [0]) Nothing (Just $ Target Fortitude 13)) Nothing []

cannongolem :: Status
cannongolem =  construct $ Status (ClassInfo (pure 168) (pure 0) (pure 20) (pure 6) (pure 6) (pure 6) (temporarize 15) (pure 0) (pure 0))
            Nothing
            (pure $ Vision Dark 60)
            (pure Enemy)
            (as 30 24 10 10 15 2)
            ((pure . pure . pure) 0)
            (A 13 15 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Piercing HeavyWeapon (D [6,6,6,6,6,6] 22 Nothing (Just $ Target Defense 0) Negates) 30 [] "FireDrake Blasting FlameThrower" )
            [] 
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            (dredux (\x _ -> pure x) Magical 15 noReductions)
            (pure (Al Lawful NeutraL))
            (pure Construct)
            (pure 0)
            []
            []
            (pure Large)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (pure $ slam)
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#perception) (2 :: Int) baseskills)
            where
                slam :: Attack Int [Int]
                slam = Attack (AInfo "Slam" Melee 5 Bludgeoning Reduc (Just 0) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False)) 
                       emptyAttack (Just $ D [10,10,10,10,10] 15 Nothing Nothing Negates) [] Nothing Nothing Nothing []

palestranger :: Status
palestranger =  t2ix TripleAttack (Present Nothing [] Nothing) . 
            t2ix ImprovedDoubleAttack (Present Nothing [] (Just 1)) . 
            t2ix CombatShot (Present Nothing [] Nothing) .
            undead $ Status (ClassInfo (pure 151) (pure 0) (pure 11) (pure 9) (pure 5) (pure 9) (temporarize 10) (pure 0) (pure 0))
            Nothing
            (pure $ Vision Dark 60)
            (pure Enemy)
            (as 17 21 10 11 18 18)
            (spellresistancebonus 21 $ initbonus 4 ((pure . pure . pure) 0))
            (A 5 7 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Piercing SmallArm (D [6,6,6] 11 Nothing (Just $ Target Defense 0) Negates) 60 [] "Elite Semi-Auto Pistol" )
            [] 
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            isundead
            (set (#magical) (pure Nothing) $ dredux (\x _ -> pure x) Bludgeon 10 noReductions)
            (pure (Al Lawful Evil))
            (pure Human)
            (pure 0)
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (pure $ combatmaneuver)
            combatmaneuver
            combatmaneuver
            0
            30
            0
            (Just auraoffear)
            (set (#perception) (18 :: Int) baseskills)

auraoffear :: SpellTemplate
auraoffear _ _ s = Spell
                     (SpellInfo  "Aura of Fear" 0 Evocation (getLevel (view (#primclass . #level) s) + abix (Just Charisma) s) 0 Nothing (EnemiesinRange Bursty 10))
                     emptyspellroll (True,False)
                     Nothing
                     Nothing
                     (Just $ Effect Fear (Present (Just 2) [] [0]) Nothing (Just $ Target Will 0))
                     Nothing
                     Nothing
                     False

guildthug :: Status
guildthug =  Status (ClassInfo (pure 8) (pure 8) (pure 0) (pure 0) (pure 0) (pure 2) (temporarize 1) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Enemy)
            (as 10 14 11 13 8 9)
            ((pure . pure . pure) 0 )
            (A 2 2 Nothing Nothing "Leather")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Weapon" )
            [Right . Right $ Weapon Slashing SmallArm defaultDamageDice 30 [] "Weapon"] 
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Neutral NeutraL))
            (pure Human)
            (pure 0)
            []
            [flashpowder]
            (pure Medium)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (pure $ combatmaneuver)
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#stealth) (4 :: Int) . set (#perception) (4 :: Int) $ baseskills)

tabaxianguard :: Status
tabaxianguard =  Status (ClassInfo (pure 11) (pure 11) (pure 2) (pure 3) (pure 0) (pure 0) (temporarize 1) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Ally)
            (as 13 12 10 10 11 9)
            ((pure . pure . pure) 0 )
            (A 1 1 Nothing Nothing "Padded")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Neutral NeutraL))
            (pure Human)
            (pure 0)
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (pure $ combatmaneuver)
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#stealth) (4 :: Int) . set (#perception) (4 :: Int) $ baseskills)

townfolk :: Status
townfolk =  Status (ClassInfo (pure 4) (pure 4) (pure 0) (pure 2) (pure 0) (pure 0) (temporarize 1) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Enemy)
            (as 13 12 10 10 11 9)
            ((pure . pure . pure) 0 )
            (A 0 0 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Neutral NeutraL))
            (pure Animal)
            (pure 0)
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (pure $ combatmaneuver)
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#perception) (4 :: Int) $ baseskills)

skulk :: Status
skulk =  Status (ClassInfo (pure 13) (pure 13) (pure 2) (pure 0) (pure 3) (pure 2) (temporarize 3) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision Low 60)
            (pure Enemy)
            (as 11 14 13 10 14 7)
            ((pure . pure . pure) 0 )
            (A 0 0 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Weapon" )
            [Right . Right $ Weapon Slashing SmallArm defaultDamageDice 30 [] "Weapon"] 
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Neutral NeutraL))
            (pure Animal)
            (pure 0)
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (pure $ combatmaneuver)
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#stealth) (14 :: Int) . set (#perception) (3 :: Int) $ baseskills)

woodenprotector :: Status
woodenprotector =  Status (ClassInfo (pure 27) (pure 27) (pure 4) (pure 5) (pure 2) (pure 22) (temporarize 6) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision Low 60)
            (pure Enemy2)
            (as 18 11 16 7 14 9)
            ((pure . pure . pure) 0 )
            (A 5 5 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Weapon" )
            [Right . Right $ Weapon Slashing SmallArm defaultDamageDice 30 [] "Weapon"] 
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Neutral NeutraL))
            (pure Plant)
            (pure 0)
            []
            [woodengaze]
            (pure Medium)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (pure $ combatmaneuver)
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#perception) (8 :: Int) $ baseskills)
woodengaze :: SpellTemplate
woodengaze _ _ _ = Spell
                     (SpellInfo  "Wooden Gaze" 2 Evocation 7 0 Nothing (SingleTarget 30))
                     emptyspellroll (True,False)
                     Nothing
                     Nothing
                     (Just $ Effect Entangled (Present (Just 3) [] [0]) Nothing (Just $ Target Reflex 0))
                     Nothing
                     Nothing
                     True


bombadierbeetle :: Status
bombadierbeetle =  Status (ClassInfo (pure 15) (pure 15) (pure 2) (pure (-2)) (pure 1) (pure 2) (temporarize 2) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision Dark 60)
            (pure Enemy)
            (as 15 10 14 1 10 9)
            ((pure . pure . pure) 0 )
            (A 6 6 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Neutral NeutraL))
            (pure Animal)
            (pure 0)
            []
            [breathweaponbeetle]
            (pure Medium)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (pure $ bite)
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#stealth) (8 :: Int) . set (#perception) (3 :: Int) $ baseskills)
            where
                bite :: Attack Int [Int]
                bite = Attack (AInfo "Bite" Melee 5 Piercing Reduc (Just 2) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False)) 
                       emptyAttack (Just $ D [8] 1 Nothing Nothing Negates) [] Nothing Nothing Nothing []
breathweaponbeetle :: SpellTemplate
breathweaponbeetle _ _ _ = Spell
                     (SpellInfo  "Beetle Explosion" 2 Evocation 3 0 Nothing (AllInRange (Sphere 5) 15))
                     emptyspellroll (True,False)
                     (Just $ D [6, 6] 0 (Just Acid) (Just $ Target Reflex 0) Half)
                     Nothing
                     Nothing
                     Nothing
                     Nothing
                     False

woundedjaguar :: Status
woundedjaguar =  Status (ClassInfo (pure 14) (pure 14) (pure 3) (pure 4) (pure 4) (pure 1) (temporarize 3) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision Low 60)
            (pure Enemy)
            (as 17 17 15 2 12 6)
            ((pure . pure . pure) 0 )
            (A 4 4 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Neutral NeutraL))
            (pure Animal)
            (pure 0)
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (bite:|[claw,claw])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#stealth) (8 :: Int) . set (#perception) (3 :: Int) $ baseskills)
            where
                bite :: Attack Int [Int]
                bite = Attack (AInfo "Bite" Melee 5 Piercing Reduc (Just 3) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False)) 
                       emptyAttack (Just $ D [4] 0 Nothing Nothing Negates) [] Nothing Nothing Nothing []
                claw :: Attack Int [Int]
                claw = Attack (AInfo "Claw" Melee 5 Piercing Reduc (Just 3) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False)) 
                       emptyAttack (Just $ D [4] 0 Nothing Nothing Negates) [] Nothing (Just $ Effect Grappled (Present (Just 2) [] [20]) Nothing (Just $ Target CMD 6)) Nothing []

summonnaturesally1 :: SpellTemplate
summonnaturesally1 mi _ s = Spell
    (SpellInfo  "Summon Natures Ally I" 2 Conjuration 0 0 Nothing Caster)
    emptyspellroll (False,False)
    Nothing
    Nothing
    Nothing
    (Just $ Summon "Dire Rat" (10 * level) 1)
    Nothing
    True
        where
            level = effectivespelllevel mi s

wolf :: Status
wolf =  Status (ClassInfo (pure 9) (pure 9) (pure 1) (pure 3) (pure 3) (pure 0) (temporarize 2) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision Low 60)
            (pure Enemy)
            (as 13 15 15 2 12 6)
            ((pure . pure . pure) 0 )
            (A 2 2 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Neutral NeutraL))
            (pure Animal)
            (pure 0)
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (pure $ bite)
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#stealth) (4 :: Int) . set (#perception) (7 :: Int) $ baseskills)
            where
                bite :: Attack Int [Int]
                bite = Attack (AInfo "Bite" Melee 5 Piercing Reduc (Just 0) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False)) 
                       emptyAttack (Just $ D [4] 0 Nothing Nothing Negates) [] Nothing (Just $ Effect Prone (Present (Just 1) [] [0]) Nothing (Just $ Target CMD 0)) Nothing []

summonnaturesally2 :: SpellTemplate
summonnaturesally2 mi _ s = Spell
    (SpellInfo  "Summon Natures Ally II" 2 Conjuration 0 0 Nothing Caster)
    emptyspellroll (False,False)
    Nothing
    Nothing
    Nothing
    (Just $ Summon "Wolf" (10 * level) 1)
    Nothing
    True
        where
            level = effectivespelllevel mi s

wolverine :: Status
wolverine =  t2ix Rage (Present Nothing [] Nothing) $ Status (ClassInfo (pure 19) (pure 19) (pure 2) (pure 5) (pure 3) (pure 3) (temporarize 3) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision Low 60)
            (pure Enemy)
            (as 15 15 15 2 12 10)
            ((pure . pure . pure) 0 )
            (A 2 2 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Neutral NeutraL))
            (pure Animal)
            (pure 0)
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (bite :| [claws, claws])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#perception) (9 :: Int) baseskills)
            where
                bite :: Attack Int [Int]
                bite = Attack (AInfo "Bite" Melee 5 Piercing Reduc (Just 0) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False)) 
                       emptyAttack (Just $ D [6] 0 Nothing Nothing Negates) [] Nothing Nothing Nothing []
                claws :: Attack Int [Int]
                claws = Attack (AInfo "Claws" Melee 5 Slashing Reduc (Just 0) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False)) 
                       emptyAttack (Just $ D [4] 0 Nothing Nothing Negates) [] Nothing Nothing Nothing []

summonnaturesally3 :: SpellTemplate
summonnaturesally3 mi _ s = Spell
    (SpellInfo  "Summon Natures Ally III" 2 Conjuration 0 0 Nothing Caster)
    emptyspellroll (False,False)
    Nothing
    Nothing
    Nothing
    (Just $ Summon "Wolverine" (10 * level) 1)
    Nothing
    True
        where
            level = effectivespelllevel mi s

griffon :: Status
griffon =  Status (ClassInfo (pure 27) (pure 27) (pure 5) (pure 4) (pure 4) (pure 3) (temporarize 5) (pure 0) (pure 0):: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision Low 60)
            (pure Enemy)
            (as 16 15 16 5 13 8)
            ((pure . pure . pure) 0 )
            (A 6 6 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Neutral NeutraL))
            (pure Animal)
            (pure 0)
            []
            []
            (pure Large)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (bite :| [claws, claws])
            rake
            combatmaneuver
            0
            30
            0
            Nothing      
            (set (#perception) (11 :: Int) baseskills)
            where
                bite :: Attack Int [Int]
                bite = Attack (AInfo "Bite" Melee 5 Piercing Reduc (Just 0) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False)) 
                       emptyAttack (Just $ D [6] 0 Nothing Nothing Negates) [] Nothing Nothing Nothing []
                claws :: Attack Int [Int]
                claws = Attack (AInfo "Claws" Melee 5 Slashing Reduc (Just (-1)) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False)) 
                       emptyAttack (Just $ D [6] 0 Nothing Nothing Negates) [] Nothing Nothing Nothing []
                rake :: Attack Int [Int]
                rake = set (#ddice) (Just $ D [4,4] 3 Nothing Nothing Negates) combatmaneuver

allosaurus :: Status
allosaurus =  Status (ClassInfo (pure 48) (pure 48) (pure 8) (pure 7) (pure 7) (pure 5) (temporarize 11) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision Low 60)
            (pure Enemy)
            (as 26 13 19 2 15 10)
            ((pure . pure . pure) 0 )
            (A 10 10 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Neutral NeutraL))
            (pure Animal)
            (pure 0)
            []
            []
            (pure Huge)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (bite :| [claws, claws])
            rake
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#perception) (30 :: Int) baseskills)
            where
                bite :: Attack Int [Int]
                bite = Attack (AInfo "Bite" Melee 5 Piercing Reduc (Just (-2)) Defense (C 19 2 0) Nothing (Al Neutral NeutraL) False (False,False)) 
                       emptyAttack (Just $ D [6, 6] 0 Nothing Nothing Negates) [] Nothing 
                       (Just $ Effect Grappled (Present (Just 2) [] [20]) Nothing (Just $ Target CMD 4)) Nothing []
                claws :: Attack Int [Int]
                claws = Attack (AInfo "Claws" Melee 5 Slashing Reduc (Just (-2)) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False)) 
                       emptyAttack (Just $ D [8] 0 Nothing Nothing Negates) [] Nothing Nothing Nothing []
                rake :: Attack Int [Int]
                rake = set (#ddice) (Just $ D [8,8] 0 Nothing Nothing Negates) combatmaneuver

summonnaturesally4 :: SpellTemplate
summonnaturesally4 mi _ s = Spell
    (SpellInfo  "Summon Natures Ally IV" 2 Conjuration 0 0 Nothing Caster)
    emptyspellroll (False,False)
    Nothing
    Nothing
    Nothing
    (Just $ Summon "Griffon" (10 * level) 1)
    Nothing
    True
        where
            level = effectivespelllevel mi s

summonnaturesally45 :: SpellTemplate
summonnaturesally45 _ _ _ = Spell
    (SpellInfo  "Summon Natures Ally V (1d3 IV)" 2 Conjuration 0 0 Nothing Caster)
    emptyspellroll (False,False)
    Nothing
    Nothing
    Nothing
    (Just $ Summon "Griffon" 100 3)
    Nothing
    True

largeearthelemental :: Status
largeearthelemental =  Status (ClassInfo (pure 44) (pure 44) (pure 8) (pure 6) (pure 2) (pure 6) (temporarize 8) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Enemy)
            (as 24 8 17 6 11 11)
            ((pure . pure . pure) 0 )
            (A 10 10 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            leeeffects
            leereductions
            (pure (Al Neutral NeutraL))
            (pure Outsider)
            (pure 0)
            []
            []
            (pure Large)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (slam :| [slam])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#perception) (11 :: Int) baseskills)
            where
                slam :: Attack Int [Int]
                slam = Attack (AInfo "Slam" Melee 10 Bludgeoning Magical (Just 0) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False)) 
                       emptyAttack (Just $ D [6,6] 1 Nothing Nothing Negates) [] Nothing Nothing Nothing []
                leeeffects :: Effects (Temporary (Temporal (Maybe Int)))
                leeeffects = set (#deadlyaim) (pure $ Present Nothing [] Nothing) $ elementaleffects
                leereductions :: DamageReduction (Temporary (Maybe Int))
                leereductions = (dredux (\x _ -> pure x) Norm 5 noReductions)

summonnaturesally5 :: SpellTemplate
summonnaturesally5 mi _ s = Spell
    (SpellInfo  "Summon Natures Ally V" 2 Conjuration 0 0 Nothing Caster)
    emptyspellroll (False,False)
    Nothing
    Nothing
    Nothing
    (Just $ Summon "Large Earth Elemental" (10 * level) 1)
    Nothing
    True
        where
            level = effectivespelllevel mi s
{- ClassInfo { hp :: a
            , bab :: a
            , fort :: a
            , ref :: a
            , wil :: a
            , level :: a
            , bsb :: a
            , bhb :: a 
            , stealth :: a
            , perception :: a -}
elephant :: Status
elephant =  Status (ClassInfo (pure 49) (pure 49) (pure 8) (pure 9) (pure 7) (pure 5) (temporarize 11) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Enemy)
            (as 30 10 19 2 13 7)
            ((pure . pure . pure) 0 )
            (A 9 9 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Neutral NeutraL))
            (pure Animal)
            (pure 0)
            []
            [trample]
            (pure Huge)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (gore :| [slam])
            gore
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#perception) (20 :: Int) baseskills)
            where
                gore :: Attack Int [Int]
                gore = Attack (AInfo "Gore" Melee 10 Piercing Reduc (Just (-2)) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False)) 
                       emptyAttack (Just $ D [8,8] 0 Nothing Nothing Negates) [] Nothing Nothing Nothing []
                slam :: Attack Int [Int]
                slam = Attack (AInfo "Slam" Melee 10 Bludgeoning Reduc (Just (-2)) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False)) 
                       emptyAttack (Just $ D [6,6] 1 Nothing Nothing Negates) [] Nothing Nothing Nothing []
trample :: SpellTemplate
trample _ _ s = Spell
                    (SpellInfo  "Trample" 2 Evocation go1 0 Nothing (AllInRange Line 60))
                    emptyspellroll (True,False)
                    (Just $ D [6, 6] go Nothing (Just $ Target Reflex 0) Half)
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    False
                    where
                        strength = view (#strength) (abilitybonuses s)
                        go = (3 * strength) `div` 2
                        go1 = strength + 5

summonnaturesally6 :: SpellTemplate
summonnaturesally6 mi _ s = Spell
    (SpellInfo  "Summon Natures Ally VI" 2 Conjuration 0 0 Nothing Caster)
    emptyspellroll (False,False)
    Nothing
    Nothing
    Nothing
    (Just $ Summon "Elephant" (10 * level) 1)
    Nothing
    True
        where
            level = effectivespelllevel mi s

vulture :: Status
vulture =  Status (ClassInfo (pure 44) (pure 44) (pure 9) (pure 6) (pure 6) (pure 6) (temporarize 10) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Enemy)
            (as 16 18 14 3 14 6)
            ((pure . pure . pure) 0 )
            (A 11 11 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Neutral NeutraL))
            (pure Animal)
            (pure 0)
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (bite :| [claws, claws])
            bite
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#perception) (15 :: Int) baseskills)
            where
                bite :: Attack Int [Int]
                bite = Attack (AInfo "Bite" Melee 5 Piercing Reduc (Just (-2)) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False)) 
                       emptyAttack (Just $ D [4] 0 Nothing Nothing Negates) [] Nothing Nothing Nothing []
                claws :: Attack Int [Int]
                claws = Attack (AInfo "Claws" Melee 5 Slashing Reduc (Just (-2)) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False)) 
                       emptyAttack (Just $ D [4] 0 Nothing Nothing Negates) [] Nothing Nothing Nothing []
         
summonvulture :: SpellTemplate
summonvulture _ _ _ = Spell
    (SpellInfo  "Summon Vulture" 2 Conjuration 0 0 Nothing Caster)
    emptyspellroll (False,False)
    Nothing
    Nothing
    Nothing
    (Just $ Summon "Vulture" (10000) 1)
    Nothing
    False

{- ClassInfo { hp :: a
            , bab :: a
            , fort :: a
            , ref :: a
            , wil :: a
            , level :: a
            , bsb :: a
            , bhb :: a 
            , stealth :: a
            , perception :: a -}

moltengolem :: Status
moltengolem = construct $ 
            Status (ClassInfo (pure 64) (pure 64) (pure 8) (pure 2) (pure 2) (pure 2) (temporarize 8) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Enemy)
            (as 18 9 0 0 11 1)
            ((pure . pure . pure) 0)
            (A 4 4 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Reduced BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            (set (#lightning) (pure Immune :: Temporary (Profile (Maybe Int))) . set (#fire) (pure Immune :: Temporary (Profile (Maybe Int))) $ normalelementalresistance)
            noEffects
            (dredux (\x _ -> pure x) Norm 5 noReductions)
            (pure (Al Neutral NeutraL))
            (pure Construct)
            (pure 0)
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (slam :| [slam])
            (set (#ddice) (Just $ D [6] 0 (Just Fire) Nothing Negates) combatmaneuver)
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills
            where
                slam :: Attack Int [Int]
                slam = Attack (AInfo "Slam" Melee 5 Piercing Reduc (Just 0) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) True (False,False)) 
                       emptyAttack (Just $ D [8, 6] 0 (Just Fire) Nothing Negates) [] 
                       Nothing Nothing Nothing []

younggreendragon :: Status
younggreendragon = t2ix Sleep (NA Nothing [] Nothing) $
            t2ix Paralyzed (NA Nothing [] Nothing) $
            Status (ClassInfo (pure 48) (pure 48) (pure 9) (pure 5) (pure 5) (pure 5) (temporarize 9) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision Dark 60)
            (pure Enemy)
            (as 21 12 17 12 13 12)
            ((pure . pure . pure) 0 )
            (A 10 10 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Reduced BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            (set (#acid) (pure $ Immune) normalelementalresistance)
            noEffects
            noReductions
            (pure (Al Neutral NeutraL))
            (pure Dragon)
            cfeatures
            []
            [breathweaponlarge]
            (pure Large)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (bite :| [claw, claw, wing, wing, tailswipe])
            bite
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#stealth) (9 :: Int) . set (#perception) (15 :: Int) $ baseskills)
            where
                bite :: Attack Int [Int]
                bite = Attack (AInfo "Bite" Melee 5 Piercing Reduc (Just (-1)) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) True (False,False)) 
                       emptyAttack (Just $ D [6, 6] 0 Nothing Nothing Negates) [] Nothing
                       Nothing Nothing []
                claw :: Attack Int [Int]
                claw = Attack (AInfo "Claw" Melee 5 Slashing Reduc (Just (-1)) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) True (False,False)) 
                       emptyAttack (Just $ D [8] 0 Nothing Nothing Negates) [] Nothing
                       Nothing Nothing []
                wing :: Attack Int [Int]
                wing = Attack (AInfo "Wing" Melee 5 Slashing Reduc (Just (-6)) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) True (False,False)) 
                       emptyAttack (Just $ D [6] 0 Nothing Nothing Negates) [] Nothing
                       Nothing Nothing []
                tailswipe :: Attack Int [Int]
                tailswipe = Attack (AInfo "Tail" Melee 10 Slashing Reduc (Just (-6)) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) True (False,False)) 
                       emptyAttack (Just $ D [8] 0 Nothing Nothing Negates) [] Nothing
                       Nothing Nothing []
                cfeatures :: ClassFeatures SpellTemplate Int
                cfeatures = set (#feats . #improvedinitiative) True emptyclassfeature
breathweaponlarge :: SpellTemplate
breathweaponlarge _ _ _ = Spell
                     (SpellInfo  "Large Breath Weapon" 2 Evocation 7 0 Nothing (AllInRange Cone 40))
                     emptyspellroll (True,False)
                     (Just $ D [6, 6, 6, 6, 6, 6] 0 Nothing (Just $ Target Reflex 0) Half)
                     Nothing
                     Nothing
                     Nothing
                     Nothing
                     True

troll :: Status
troll =     Status (ClassInfo (pure 27) (pure 27) (pure 4) (pure 5) (pure 2) (pure 4) (temporarize 6) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision Dark 60)
            (pure Enemy)
            (as 21 14 23 6 9 6)
            ((pure . pure . pure) 0 )
            (A 5 5 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            (set (#regeneration) (pure $ Present Nothing [] (Just 5)) noEffects)
            noReductions
            (pure (Al Neutral NeutraL))
            (pure Animal)
            (pure 0)
            []
            []
            (pure Large)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (bite :| [claws, claws])
            bite
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#perception) (8 :: Int) baseskills)
            where
                bite :: Attack Int [Int]
                bite = Attack (AInfo "Bite" Melee 5 Piercing Reduc (Just (-1)) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False)) 
                       emptyAttack (Just $ D [8] 0 Nothing Nothing Negates) [] Nothing 
                       Nothing Nothing []
                claws :: Attack Int [Int]
                claws = Attack (AInfo "Claws" Melee 5 Slashing Reduc (Just (-1)) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False)) 
                       emptyAttack (Just $ D [6] 0 Nothing Nothing Negates) [] Nothing Nothing Nothing []

bulette :: Status
bulette =  Status (ClassInfo (pure 44) (pure 44) (pure 6) (pure 7) (pure 6) (pure 4) (temporarize 8) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision Low 60)
            (pure Enemy)
            (as 23 15 20 2 13 6)
            ((pure . pure . pure) 0 )
            (A 12 12 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Neutral NeutraL))
            (pure Animal)
            (pure 0)
            []
            []
            (pure Huge)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (bite :| [claws, claws])
            leap
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#perception) (11 :: Int) baseskills)
            where
                bite :: Attack Int [Int]
                bite = Attack (AInfo "Bite" Melee 5 Piercing Reduc (Just (-1)) Defense (C 19 2 0) Nothing (Al Neutral NeutraL) False (False,False)) 
                       emptyAttack (Just $ D [8, 8] 0 Nothing Nothing Negates) [] Nothing 
                       Nothing Nothing []
                claws :: Attack Int [Int]
                claws = Attack (AInfo "Claws" Melee 5 Slashing Reduc (Just (-2)) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False)) 
                       emptyAttack (Just $ D [6, 6] 0 Nothing Nothing Negates) [] Nothing Nothing Nothing []
                leap :: Attack Int [Int]
                leap = set (#ddice) (Just $ D [6, 6, 6, 6, 6] 0 Nothing Nothing Negates) combatmaneuver

centipedeswarm :: Status
centipedeswarm =  Status (ClassInfo (pure 40) (pure 40) (pure 6) (pure 6) (pure 6) (pure 3) (temporarize 9) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision Low 60)
            (pure Enemy)
            (as 1 19 8 1 10 2)
            ((pure . pure . pure) 0 )
            (A 4 4 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Reduced BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Neutral NeutraL))
            (pure Animal)
            (pure 0)
            []
            []
            (pure Tiny)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (swarm :| [])
            swarm
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#perception) (4 :: Int) baseskills)
            where
                swarm :: Attack Int [Int]
                swarm = Attack (AInfo "Swarm" Missile 5 Piercing Reduc (Just 100) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False)) 
                       emptyAttack (Just $ D [6, 6] 0 Nothing Nothing Negates) [] 
                       (Just $ SD (Just Poisen) (Just $ Target Fortitude 3) (Just Dexterity) [4] [4] 0 [4] 0) Nothing Nothing []

wyrmlingforrestdragon :: Status
wyrmlingforrestdragon = t2ix Poisened (NA Nothing [] Nothing) $
            t2ix Paralyzed (NA Nothing [] Nothing) $
            t2ix Sleep (NA Nothing [] Nothing) $
            Status (ClassInfo (pure 55) (pure 55) (pure 7) (pure 5) (pure 5) (pure 5) (temporarize 7) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision Dark 60)
            (pure Enemy)
            (as 15 14 14 10 11 10)
            ((pure . pure . pure) 0 )
            (A 6 6 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Reduced BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            (set (#grenade . #weaponfeat) Specialization allproficient)
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Neutral NeutraL))
            (pure Dragon)
            cfeatures
            []
            [breathweaponwyrmling]
            (pure Small)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (bite :| [claw, claw])
            bite
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#stealth) (16 :: Int) . set (#perception) (10 :: Int) $ baseskills)
            where
                bite :: Attack Int [Int]
                bite = Attack (AInfo "Bite" Melee 5 Piercing Reduc (Just 2) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) True (False,False)) 
                       emptyAttack (Just $ D [6] 0 Nothing Nothing Negates) [] Nothing
                       Nothing Nothing []
                claw :: Attack Int [Int]
                claw = Attack (AInfo "Claw" Melee 5 Slashing Reduc (Just 1) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) True (False,False)) 
                       emptyAttack (Just $ D [4] 0 Nothing Nothing Negates) [] Nothing
                       Nothing Nothing []
                cfeatures :: ClassFeatures SpellTemplate Int
                cfeatures = set (#feats . #improvedinitiative) True $ 
                            set (#feats . #deadlyaim) True emptyclassfeature
breathweaponwyrmling :: SpellTemplate
breathweaponwyrmling _ _ _ = Spell
                     (SpellInfo  "Small Breath Weapon" 2 Evocation 5 0 Nothing (AllInRange Cone 20))
                     emptyspellroll (True,False)
                     (Just $ D [6, 6] 0 Nothing (Just $ Target Reflex 0) Half)
                     Nothing
                     Nothing
                     Nothing
                     Nothing
                     True
octopus :: Status
octopus = Status (ClassInfo (pure 12) (pure 12) (pure 1) (pure 2) (pure 3) (pure 0) (temporarize 2) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Enemy)
            (as 12 17 14 3 13 3)
            ((pure . pure . pure) 0 )
            (A 1 1 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Reduced BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Neutral NeutraL))
            (pure Animal)
            (pure 0)
            []
            []
            (pure Small)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (bite :| [])
            bite
            (over (#info . #attackbonus) ((Just 4) &) combatmaneuver)
            0
            30
            0
            Nothing
            (set (#stealth) (20 :: Int) . set (#perception) (11 :: Int) $ baseskills)
            where
                bite :: Attack Int [Int]
                bite = Attack (AInfo "Bite" Melee 5 Piercing Reduc (Just 2) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) True (False,False)) 
                       emptyAttack (Just $ D [3] 0 Nothing Nothing Negates) [] (Just $ SD (Just Poisen) (Just $ Target Fortitude 3) (Just Strength) [1] [1] 0 [4] 0)
                       Nothing Nothing []

constrictorsnake :: Status
constrictorsnake = Status (ClassInfo (pure 18) (pure 18) (pure 2) (pure 3) (pure 3) (pure 1) (temporarize 3) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Enemy)
            (as 17 17 12 1 12 2)
            ((pure . pure . pure) 0 )
            (A 2 2 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Reduced BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Neutral NeutraL))
            (pure Animal)
            (pure 0)
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (slam :| [])
            constrict
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#perception) (12 :: Int) baseskills)
            where
                slam :: Attack Int [Int]
                slam = Attack (AInfo "Slam" Melee 5 Bludgeoning Reduc (Just 0) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) True (False,False)) 
                       emptyAttack (Just $ D [4] 1 Nothing Nothing Negates) [] Nothing
                       (Just $ Effect Grappled (Present (Just 2) [] [20]) Nothing (Just $ Target CMD 4)) Nothing []
                constrict :: Attack Int [Int]
                constrict = Attack (AInfo "Constrict" Melee 5 Bludgeoning Reduc (Just 0) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) True (False,False)) 
                       emptyAttack (Just $ D [4] 1 Nothing Nothing Negates) [] Nothing
                       (Just $ Effect Grappled (Present (Just 2) [] [20]) Nothing (Just $ Target CMD 4)) Nothing []

giantfrog :: Status
giantfrog = Status (ClassInfo (pure 12) (pure 12) (pure 1) (pure 3) (pure 5) (pure 0) (temporarize 2) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Enemy)
            (as 15 13 16 1 9 6)
            ((pure . pure . pure) 0 )
            (A 1 1 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Reduced BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Neutral NeutraL))
            (pure Animal)
            (pure 0)
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (tongue :| [])
            bite
            swallowwhole
            0
            30
            0
            Nothing
            baseskills
            where
                tongue :: Attack Int [Int]
                tongue = Attack (AInfo "Tongue Grab" Melee 5 Piercing Reduc (Just 0) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) True (False,False)) 
                       emptyAttack Nothing [] Nothing
                       (Just $ Effect Grappled (Present (Just 2) [] [20]) Nothing (Just $ Target CMD 4)) Nothing []
                bite :: Attack Int [Int]
                bite = Attack (AInfo "Bite" Melee 5 Piercing Reduc (Just 0) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) True (False,False)) 
                       emptyAttack (Just $ D [6] 0 Nothing Nothing Negates) [] Nothing
                       (Just $ Effect Grappled (Present (Just 2) [] [20]) Nothing (Just $ Target CMD 4)) Nothing []
                swallowwhole :: Attack Int [Int]
                swallowwhole = Attack (AInfo "Swallow Whole" Melee 5 Bludgeoning Reduc (Just 0) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) True (False,False)) 
                       emptyAttack (Just $ D [4] 0 Nothing Nothing Negates) [] Nothing
                       (Just $ Effect Grappled (Present Nothing [] [20]) Nothing (Just $ Target CMD 4)) Nothing []

woodgolem :: Status
woodgolem = set (#effects . #immunetomagic) (pure $ Absent Nothing [] Nothing) . construct $ 
            Status (ClassInfo (pure 36) (pure 36) (pure 3) (pure 1) (pure 1) (pure 0) (temporarize 6) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Enemy)
            (as 18 10 0 0 8 10)
            ((pure . pure . pure) 0 )
            (A 4 4 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Reduced BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            (set (#fire) (pure Vulnerable :: Temporary (Profile (Maybe Int))) normalelementalresistance)
            noEffects
            (dredux (\x _ -> pure x) Slash 5 noReductions)
            (pure (Al Neutral NeutraL))
            (pure Construct)
            (pure 0)
            []
            []
            (pure Large)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (slam :| [slam])
            slam
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills
            where
                slam :: Attack Int [Int]
                slam = Attack (AInfo "Shield Slam" Melee 5 Piercing Reduc (Just (-1)) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) True (False,False)) 
                       emptyAttack (Just $ D [8] 0 Nothing Nothing Negates) [] 
                       Nothing Nothing Nothing []

lurcher :: Status
lurcher =  undead $ Status (ClassInfo (pure 24) (pure 24) (pure 2) (pure 1) (pure 3) (pure 4) (temporarize 5) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Enemy)
            (as 15 8 0 10 14 15)
            ((pure . pure . pure) 0 )
            (A 5 5 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Reduced BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Neutral NeutraL))
            (pure UnDead)
            (pure 0)
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (entrail :| [entrail, entrail, entrail])
            viscera
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills
            where
                entrail :: Attack Int [Int]
                entrail = Attack (AInfo "Entrail" Melee 5 Piercing Reduc (Just 1) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False)) 
                       emptyAttack (Just $ D [3] 0 Nothing Nothing Negates) [] 
                       Nothing (Just $ Effect Grappled (Present (Just 2) [] [20]) Nothing (Just $ Target CMD 4)) Nothing []
                viscera :: Attack Int [Int]
                viscera = Attack (AInfo "Choaking Viscera" Melee 5 Piercing Reduc (Just 0) Fortitude (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False)) 
                       emptyAttack Nothing [] 
                       (Just $ SD Nothing Nothing (Just Constitution) [3] [3] 0 [0] 0) Nothing Nothing []

croaker :: Status
croaker =  undead $ Status (ClassInfo (pure 60) (pure 60) (pure 3) (pure 3) (pure 3) (pure 6) (temporarize 9) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Enemy)
            (as 18 10 0 13 14 12)
            ((pure . pure . pure) 0 )
            (A 5 5 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Reduced BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            (set (#channelresistance) (pure $ Present Nothing [] Nothing :: Temporary (Temporal (Maybe Int))) isundead)
            (dredux (\x _ -> pure x) Magical 10 noReductions)
            (pure (Al Neutral NeutraL))
            (pure UnDead)
            (pure 0)
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (touch :| [])
            hangmansnoose
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills
            where
                touch :: Attack Int [Int]
                touch = Attack (AInfo "Madness Touch" Melee 5 Piercing Reduc (Just 0) Touch (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False)) 
                       emptyAttack Nothing [] 
                       (Just $ SD Nothing Nothing (Just Charisma) [6] [6] 0 [0] 0) Nothing Nothing []
                hangmansnoose :: Attack Int [Int]
                hangmansnoose = Attack (AInfo "Hangmans Noose" Melee 5 Piercing Reduc (Just 3) CMD (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False)) 
                       emptyAttack (Just $ D [6] 3 Nothing Nothing Negates) [] 
                       Nothing Nothing Nothing []

shadow :: Status
shadow =  undead $ Status (ClassInfo (pure 16) (pure 16) (pure 2) (pure 3) (pure 1) (pure 3) (temporarize 3) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision Dark 60)
            (pure Enemy)
            (as 0 14 0 6 12 15)
            ((pure . pure . pure) 0 )
            (A 2 2 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Reduced BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            (set (#incorporal) (pure $ Permanent Nothing [] Nothing) noEffects)
            (dredux (\x _ -> pure x) Magical 10 noReductions)
            (pure (Al Neutral NeutraL))
            (pure UnDead)
            (pure 0)
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (touch :| [])
            touch
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills
            where
                touch :: Attack Int [Int]
                touch = Attack (AInfo "Icy Touch" Missile 5 Piercing Reduc (Just 0) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False)) 
                       emptyAttack Nothing [] 
                       (Just $ SD Nothing Nothing (Just Strength) [6] [6] 0 [0] 0) Nothing Nothing []

ghoulstirge :: Status
ghoulstirge =  undead $ Status (ClassInfo (pure 24) (pure 24) (pure 2) (pure 1) (pure 1) (pure 4) (temporarize 4) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision Low 60)
            (pure Enemy)
            (as 10 17 0 6 14 12)
            ((pure . pure . pure) 0 )
            (A 2 2 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Reduced BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Neutral NeutraL))
            (pure UnDead)
            (pure 0)
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (bite :| [])
            blooddrain
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills
            where
                bite :: Attack Int [Int]
                bite = Attack (AInfo "Bite" Missile 5 Piercing Reduc (Just 0) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False)) 
                       emptyAttack (Just $ D [6] 0 Nothing Nothing Negates) [] 
                       Nothing (Just $ Effect Paralyzed (Present (Just 50) [] [0]) Nothing (Just $ Target Fortitude 3)) Nothing []
                blooddrain :: Attack Int [Int]
                blooddrain = Attack (AInfo "BloodDrain" Missile 5 Piercing Reduc (Just 0) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False)) 
                       emptyAttack Nothing [] (Just $ SD Nothing Nothing (Just Constitution) [6] [6] 0 [0] 0) Nothing Nothing []

choaker :: Status
choaker =  undead $ Status (ClassInfo (pure 6) (pure 6) (pure 1) (pure 0) (pure 0) (pure 3) (temporarize 2) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision Low 60)
            (pure Enemy)
            (as 18 11 0 6 11 10)
            ((pure . pure . pure) 0 )
            (A 2 2 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Reduced BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            (dredux (\x _ -> pure x) Bludgeon 5 noReductions)
            (pure (Al Neutral NeutraL))
            (pure UnDead)
            (pure 0)
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (claws :| [claws])
            claws
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills
            where
                claws :: Attack Int [Int]
                claws = Attack (AInfo "Choaker" Melee 5 Piercing Reduc (Just 0) CMD (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False)) 
                       emptyAttack (Just $ D [4] 0 Nothing Nothing Negates) [] 
                       Nothing (Just $ Effect Grappled (Present (Just 2) [] [20]) Nothing Nothing) Nothing []

giantleach :: Status
giantleach =  Status (ClassInfo (pure 12) (pure 12) (pure 1) (pure 3) (pure 0) (pure 0) (temporarize 2) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision Low 60)
            (pure Enemy)
            (as 11 12 12 0 10 2)
            ((pure . pure . pure) 0 )
            (A 0 0 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Reduced BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Neutral NeutraL))
            (pure Animal)
            (pure 0)
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (bite :| [])
            bite
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills
            where
                bite :: Attack Int [Int]
                bite = Attack (AInfo "Bite" Melee 5 Piercing Reduc (Just 0) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False)) 
                       emptyAttack (Just $ D [6] 0 Nothing Nothing Negates) [] 
                       (Just $ SD Nothing Nothing (Just Constitution) [2] [2] 0 [0] 0) Nothing Nothing []

carrionstorm :: Status
carrionstorm =  undead $ Status (ClassInfo (pure 18) (pure 18) (pure 1) (pure 0) (pure 2) (pure 3) (temporarize 1) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision Low 60)
            (pure Enemy)
            (as 1 11 0 2 14 6)
            ((pure . pure . pure) 0 )
            (A 0 0 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Reduced BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Neutral NeutraL))
            (pure Animal)
            (pure 0)
            []
            []
            (pure Tiny)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (swarm :| [])
            swarm
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills
            where
                swarm :: Attack Int [Int]
                swarm = Attack (AInfo "Swarm" Missile 5 Piercing Reduc (Just 100) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False)) 
                       emptyAttack (Just $ D [6] 0 Nothing Nothing Negates) [] 
                       Nothing (Just $ Effect Nauseated (Present (Just 1) [] [0]) Nothing (Just $ Target Fortitude 1)) Nothing []

batswarm :: Status
batswarm =  Status (ClassInfo (pure 18) (pure 18) (pure 1) (pure 0) (pure 2) (pure 3) (temporarize 1) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision Dark 60)
            (pure Enemy)
            (as 1 11 0 2 14 6)
            ((pure . pure . pure) 0 )
            (A 0 0 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Reduced BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Neutral NeutraL))
            (pure Animal)
            (pure 0)
            []
            []
            (pure Tiny)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (swarm :| [])
            swarm
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills
            where
                swarm :: Attack Int [Int]
                swarm = Attack (AInfo "Swarm" Missile 5 Piercing Reduc (Just 100) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False)) 
                       emptyAttack (Just $ D [6] 0 Nothing Nothing Negates) [] 
                       Nothing (Just $ Effect Nauseated (Present (Just 1) [] [0]) Nothing (Just $ Target Fortitude 1)) Nothing []

guard :: Status
guard =  Status (ClassInfo (pure 20) (pure 20) (pure 1) (pure 2) (pure 0) (pure 0) (temporarize 1) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Enemy)
            (as 15 11 12 10 9 6)
            ((pure . pure . pure) 0 )
            (A 5 5 (Just 4) Nothing "ChainMail")
            (pure Nothing)
            (Weapon Reduced BasicMelee defaultDamageDice 5 [] "Weapon" )
            [Right . Right $ Weapon Reduced BasicMelee defaultDamageDice 5 [] "Weapon"] 
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Neutral NeutraL))
            (pure Animal)
            (pure 0)
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills

goblin :: Status
goblin =  Status (ClassInfo (pure 10) (pure 10) (pure 1) (pure 2) (pure 0) (pure 0) (temporarize 1) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision Dark 60)
            (pure Enemy)
            (as 11 15 12 10 9 6)
            ((pure . pure . pure) 0 )
            (A 2 2 (Just 6) Nothing "Leather")
            (pure Nothing)
            (Weapon Reduced BasicMelee defaultDamageDice 5 [] "Weapon" )
            [Right . Right $ Weapon Reduced SmallArm defaultDamageDice 30 [] "Weapon"] 
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Neutral NeutraL))
            (pure Animal)
            (pure 0)
            []
            []
            (pure Small)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills

direwolf :: Status
direwolf =  Status (ClassInfo (pure 24) (pure 24) (pure 3) (pure 4) (pure 4) (pure 1) (temporarize 5) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision Low 60)
            (pure Enemy)
            (as 19 15 17 2 12 10)
            ((pure . pure . pure) 0 )
            (A 3 3 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Neutral NeutraL))
            (pure Animal)
            (pure 0)
            []
            []
            (pure Large)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (bite :| [])
            bite
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#perception) (10 :: Int) baseskills)
            where
                bite :: Attack Int [Int]
                bite = Attack (AInfo "Bite" Melee 5 Piercing Reduc (Just 0) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False)) 
                       emptyAttack (Just $ D [8] 0 Nothing Nothing Negates) [] 
                       Nothing (Just $ Effect Prone (Present (Just 1) [] [0]) Nothing (Just $ Target CMD 0)) Nothing []

direboar :: Status
direboar =  Status (ClassInfo (pure 29) (pure 29) (pure 3) (pure 4) (pure 4) (pure 1) (temporarize 5) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision Low 60)
            (pure Enemy)
            (as 23 10 17 2 13 8)
            ((pure . pure . pure) 0 )
            (A 6 6 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Neutral NeutraL))
            (pure Animal)
            (set (#feats . #improvedinitiative) True (pure 0))
            []
            []
            (pure Large)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (gore :| [])
            gore
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#perception) (12 :: Int) baseskills)
            where
                gore :: Attack Int [Int]
                gore = Attack (AInfo "Gore" Melee 5 Piercing Reduc (Just 0) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False)) 
                       emptyAttack (Just $ D [6, 6] 0 Nothing Nothing Negates) [] 
                       Nothing Nothing Nothing []
boar :: Status
boar =  Status (ClassInfo (pure 15) (pure 15) (pure 1) (pure 3) (pure 3) (pure 0) (temporarize 2) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision Low 60)
            (pure Enemy)
            (as 17 10 17 2 13 4)
            ((pure . pure . pure) 0 )
            (A 4 4 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Neutral NeutraL))
            (pure Animal)
            (pure 0)
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (gore :| [])
            gore
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#stealth) (4 :: Int) . set (#perception) (6 :: Int) $ baseskills)
            where
                gore :: Attack Int [Int]
                gore = Attack (AInfo "Gore" Melee 5 Piercing Reduc (Just 0) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False)) 
                       emptyAttack (Just $ D [8] 1 Nothing Nothing Negates) [] 
                       Nothing Nothing Nothing []

direbat :: Status
direbat =  Status (ClassInfo (pure 20) (pure 20) (pure 3) (pure 4) (pure 4) (pure 1) (temporarize 4) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision Dark 60)
            (pure Enemy)
            (as 17 15 13 2 14 6)
            ((pure . pure . pure) 0 )
            (A 3 3 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Neutral NeutraL))
            (pure Animal)
            (pure 0)
            []
            []
            (pure Large)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (bite :| [])
            bite
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#stealth) (4 :: Int) . set (#perception) (12 :: Int) $ baseskills)
            where
                bite :: Attack Int [Int]
                bite = Attack (AInfo "Bite" Melee 5 Piercing Reduc (Just (-1)) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False)) 
                       emptyAttack (Just $ D [8] 1 Nothing Nothing Negates) [] 
                       Nothing Nothing Nothing []

unicorn :: Status
unicorn =  Status (ClassInfo (pure 25) (pure 25) (pure 4) (pure 4) (pure 4) (pure 1) (temporarize 4) (pure 0) (pure 9) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision Low 60)
            (pure Enemy)
            (as 18 17 16 11 21 24)
            ((pure . pure . pure) 0 )
            (A 5 5 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Reduced BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Neutral NeutraL))
            (pure Animal)
            (pure 0)
            [curewounds 2, curewounds 2, curewounds 2, curewounds 3]
            []
            (pure Large)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (gore :| [hoof, hoof])
            charge
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#stealth) (8 :: Int) . set (#perception) (10 :: Int) $ baseskills)
            where
                gore :: Attack Int [Int]
                gore = Attack (AInfo "Gore" Melee 5 Piercing Magical (Just 0) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False))
                       emptyAttack (Just $ D [8] 0 Nothing Nothing Negates) [] 
                       Nothing Nothing Nothing []
                hoof :: Attack Int [Int]
                hoof = Attack (AInfo "Hoof" Melee 5 Bludgeoning Magical (Just (-3)) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False))
                       emptyAttack (Just $ D [6] 0 Nothing Nothing Negates) [] 
                       Nothing Nothing Nothing []
                charge :: Attack Int [Int]
                charge = Attack (AInfo "Charge" Melee 5 Piercing Magical (Just 2) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False))
                       emptyAttack (Just $ D [8, 8] 2 Nothing Nothing Negates) [] 
                       Nothing Nothing Nothing []

giantmantis :: Status
giantmantis =  Status (ClassInfo (pure 20) (pure 20) (pure 3) (pure 4) (pure 1) (pure 1) (temporarize 4) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Enemy)
            (as 16 13 16 0 14 11)
            ((pure . pure . pure) 0 )
            (A 5 5 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Neutral NeutraL))
            (pure Animal)
            (pure 0)
            []
            []
            (pure Large)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (claw :| [claw])
            bite
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#perception) (6 :: Int) baseskills)
            where
                claw :: Attack Int [Int]
                claw = Attack (AInfo "Claw" Melee 5 Slashing Reduc (Just (-1)) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False))
                       emptyAttack (Just $ D [6] 0 Nothing Nothing Negates) [] 
                       Nothing (Just $ Effect Grappled (Present (Just 2) [] [20]) Nothing (Just $ Target CMD 4)) Nothing []
                bite :: Attack Int [Int]
                bite = Attack (AInfo "Bite" Melee 5 Piercing Reduc (Just (-6)) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False))
                       emptyAttack (Just $ D [6] 0 Nothing Nothing Negates) [] 
                       Nothing Nothing Nothing []
                
assassinvine :: Status
assassinvine =  Status (ClassInfo (pure 20) (pure 20) (pure 3) (pure 4) (pure 1) (pure 1) (temporarize 4) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision Dark 60)
            (pure Enemy)
            (as 20 10 16 0 13 9)
            ((pure . pure . pure) 0 )
            (A 6 6 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            plantresistance
            (set (#entangled) (pure $ NA Nothing [] Nothing :: Temporary (Temporal (Maybe Int))) noEffects)
            noReductions
            (pure (Al Neutral NeutraL))
            (pure Plant)
            (pure 0)
            []
            [vineentangle]
            (pure Large)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (slam :| [])
            slam
            combatmaneuver
            0
            5
            0
            Nothing
            (set (#perception) (1 :: Int) baseskills)
            where
                plantresistance :: ElementalResistance (Temporary (Profile (Maybe Int)))
                plantresistance = set (#fire) (pure (NormalDamage (Just 10)) :: Temporary (Profile (Maybe Int))) $ 
                                     set (#ice) (pure (NormalDamage (Just 10)) :: Temporary (Profile (Maybe Int))) $
                                     set (#lightning) (pure Immune :: Temporary (Profile (Maybe Int))) normalelementalresistance
                slam :: Attack Int [Int]
                slam = Attack (AInfo "Slam" Melee 5 Bludgeoning Reduc (Just 0) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False))
                       emptyAttack (Just $ D [8] 2 Nothing Nothing Negates) [] 
                       Nothing (Just $ Effect Grappled (Present (Just 2) [] [20]) Nothing (Just $ Target CMD 4)) Nothing []
vineentangle :: SpellTemplate
vineentangle _ _ _ = (Spell (SpellInfo ("Vine Entangle") 2 Transmutation 3 0 Nothing (MapArea 30 0)) 
                                        emptyspellroll (True,False) Nothing Nothing (Just $ Effect Entangled (Present (Just 10) [] [0]) Nothing (Just $ Target Reflex 0)) Nothing Nothing False)
         
giantspider :: Status
giantspider =  Status (ClassInfo (pure 16) (pure 16) (pure 2) (pure 3) (pure 1) (pure 1) (temporarize 3) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision Dark 60)
            (pure Enemy)
            (as 11 17 12 0 10 2)
            ((pure . pure . pure) 0 )
            (A 2 2 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Neutral NeutraL))
            (pure Animal)
            (pure 0)
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (bite :| [])
            web
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#perception) (4 :: Int) baseskills)
            where
                bite :: Attack Int [Int]
                bite = Attack (AInfo "Bite" Melee 5 Piercing Reduc (Just 0) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False))
                       emptyAttack (Just $ D [6] 0 Nothing Nothing Negates) [] 
                       (Just $ SD (Just Poisen) (Just $ Target Fortitude 4) (Just Strength) [2] [2] 0 [0] 3) Nothing Nothing []
                web :: Attack Int [Int]
                web = Attack (AInfo "Web" Missile 5 Bludgeoning Reduc (Just 0) Reflex (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False))
                       emptyAttack Nothing [] 
                       Nothing (Just $ Effect Held (Present (Just 3) [] [0]) Nothing Nothing) Nothing []


muskcreep :: Status
muskcreep =  Status (ClassInfo (pure 16) (pure 16) (pure 2) (pure 3) (pure 1) (pure 1) (temporarize 3) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision Dark 60)
            (pure Enemy)
            (as 17 15 16 0 11 8)
            ((pure . pure . pure) 0 )
            (A 2 2 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Neutral NeutraL))
            (pure Plant)
            (pure 0)
            []
            [zombiefy]
            (pure Medium)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (tendril :| [])
            pollenspray
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills
            where
                tendril :: Attack Int [Int]
                tendril = Attack (AInfo "Tendril" Melee 5 Piercing Reduc (Just 0) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False))
                       emptyAttack (Just $ D [4] 1 Nothing Nothing Negates) [] Nothing Nothing Nothing []
                pollenspray :: Attack Int [Int]
                pollenspray = Attack (AInfo "Pollen Spray" Missile 5 Piercing Reduc (Just 0) Touch (C 21 1 0) Nothing (Al Neutral NeutraL) False (False,False))
                       emptyAttack Nothing [] Nothing (Just $ Effect Confused (Present (Just 3) [] [0]) Nothing (Just $ Target Will 4)) Nothing []
zombiefy :: SpellTemplate
zombiefy _ _ _ = (Spell (SpellInfo ("Yellow Musk Zombie") 2 Transmutation 0 0 Nothing Caster) 
                                        emptyspellroll (True,False) Nothing (Just $ SD Nothing Nothing (Just Intelligence) [4] [4] 1 [4] 0) Nothing Nothing Nothing False)

zombie :: Status
zombie =  undead . t2ix Staggered (Permanent Nothing [] Nothing) $ Status (ClassInfo (pure 12) (pure 12) (pure 1) (pure 0) (pure 0) (pure 3) (temporarize 2) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision Low 60)
            (pure Enemy)
            (as 17 10 0 0 10 10)
            ((pure . pure . pure) 0 )
            (A 2 2 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            (noEffects)
            (dredux (\x _ -> pure x) Slash 5 noReductions)
            (pure (Al Neutral NeutraL))
            (pure Human)
            (pure 0)
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (slam :| [])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills
            where
                slam :: Attack Int [Int]
                slam = Attack (AInfo "Bite" Melee 5 Bludgeoning Reduc (Just 0) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False))
                       emptyAttack (Just $ D [6] 1 Nothing Nothing Negates) [] Nothing Nothing Nothing []

ghoul :: Status
ghoul =  undead $ Status (ClassInfo (pure 12) (pure 12) (pure 1) (pure 2) (pure 0) (pure 3) (temporarize 2) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision Dark 60)
            (pure Enemy)
            (as 13 15 0 13 14 14)
            ((pure . pure . pure) 0 )
            (A 2 2 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Neutral NeutraL))
            (pure Human)
            (pure 0)
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (bite :| [claws, claws])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#perception) (7 :: Int) baseskills)
            where
                bite :: Attack Int [Int]
                bite = Attack (AInfo "Bite" Melee 5 Piercing Reduc (Just 1) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False))
                       emptyAttack (Just $ D [6] 0 Nothing Nothing Negates) [] (Just $ SD (Just Disease) (Just $ Target Fortitude 3) (Just Constitution) [6] [6] 0 [0] 0) 
                       (Just $ Effect Paralyzed (Present (Just 3) [] [0]) Nothing (Just $ Target Fortitude 5)) Nothing []
                claws :: Attack Int [Int]
                claws = Attack (AInfo "Claws" Melee 5 Slashing Reduc (Just 1) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False))
                       emptyAttack (Just $ D [6] 0 Nothing Nothing Negates) [] Nothing (Just $ Effect Paralyzed (Present (Just 3) [] [0]) Nothing (Just $ Target Fortitude 5)) Nothing []                       

ghoulpriest :: Status
ghoulpriest =  undead . classupdate $ Status (ClassInfo (pure 12) (pure 12) (pure 1) (pure 2) (pure 0) (pure 3) (temporarize 2) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision Dark 60)
            (pure Enemy)
            (as 13 15 0 13 14 14)
            ((pure . pure . pure) 0 )
            (A 2 2 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Weapon" )
            []
            allproficient
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Neutral NeutraL))
            (pure Human)
            (pure 0)
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 0 0 0 0 0 0 0)
            (bite :| [claws, claws])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#perception) (7 :: Int) baseskills)
            where
                bite :: Attack Int [Int]
                bite = Attack (AInfo "Bite" Melee 5 Piercing Reduc (Just 1) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False))
                       emptyAttack (Just $ D [6] 0 Nothing Nothing Negates) [] (Just $ SD (Just Disease) (Just $ Target Fortitude 3) (Just Constitution) [6] [6] 0 [0] 0) 
                       (Just $ Effect Paralyzed (Present (Just 3) [] [0]) Nothing (Just $ Target Fortitude 5)) Nothing []
                claws :: Attack Int [Int]
                claws = Attack (AInfo "Claws" Melee 5 Slashing Reduc (Just 1) Defense (C 20 2 0) Nothing (Al Neutral NeutraL) False (False,False))
                       emptyAttack (Just $ D [6] 0 Nothing Nothing Negates) [] Nothing (Just $ Effect Paralyzed (Present (Just 3) [] [0]) Nothing (Just $ Target Fortitude 5)) Nothing []                       