{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module DND.CHARACTER.Character
where

import Control.Monad.Trans.State.Strict (State, state, runState)
import Control.Lens (Lens', set, over, view)
import Data.Char (isDigit)
import Control.Lens.Indexed (imap)
import Data.Generics.Labels ()
import GHC.Generics (Generic)
import Control.Applicative
import Data.Either (isLeft, fromRight)
import Data.Maybe (fromJust, catMaybes)
import System.Random
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Graphics.Gloss hiding (Scale)

import DND.ATTACKS.Attack (Attack (Attack), AttackInfo (AInfo), Result (CriticalMiss, CriticalHit, Miss, Hit))
import DND.ATTACKS.AttackFunctions (isHit)
import DND.ATTACKS.AttackFunctions2 (s2a, maybeint2int)
import DND.ATTACKS.DamageReduction (noReductions, dredux, DamageType (Slashing, Piercing, Bludgeoning), Material (Pierce, Norm))
import DND.ATTACKS.Smite (Alignment (Al), Lawfulness (Lawful, Chaotic, Neutral), Benificence (Evil, Good, NeutraL), Race (Human, Orc, Halfling, Gnome, UnDead, Elf, Kitsune, Tengu))
import DND.ATTACKS.Weapon (Weapon(Weapon), 
      WeaponType (BasicMelee, Grenade, HeavyWeapon, AdvancedMelee, SmallArm, Sniper, LongArm)
      , Enchantment (Fiery, LongSite), Range (Melee, Missile), allproficient, noproficient, rangefinder, WeaponFeat (Specialization), Weapons, FeatProfile)

import DND.CHARACTER.AbilityScores (AbilityScores, strength, dexterity, as)
import DND.CHARACTER.Bonuses (Bonuses (miscclass), BonusTypes)
import DND.CHARACTER.Class (Skills, ClassInfo (ClassInfo, bab), Classes (Classes), collapseclasses, cleric)
import DND.CHARACTER.ClassFeatures (ClassFeatures, SolarionFeatures (SolFeat), EnvoyFeatures (EnvFeat))
import DND.CHARACTER.ClassUpdate2 (classupdate)
import DND.CHARACTER.Status (isRing, Equipment, Health (..), Status(Status, primclass), Size (Medium, Small), Health (Healthy, Dead), playerclasses, gatherbonuses, MagicItem (MagicItem), EquipmentArea (..), eqix, magicitemtest2, magicitemtest,
      damager, damager', emptystatus, currenthitpoints, health, healthupdate, abilitybonuses, effects, Team (..), SpellTemplate, emptyclassfeature, combatmaneuver, channelenergy', baseskills, Vision (Vision), VisionCategory (NormalVision, Dark, Low), 
      effectivespelllevel, resolvepoints)

import DND.DAMAGEDICE.DamageDice (DamageDice (D), SaveProfile (Negates, Half), defaultDamageDice)
import DND.DAMAGEDICE.DamageDiceFunctions (dDEP2MI, mddstatus)
import DND.DAMAGEDICE.Elemental (normalelementalresistance, undeadelementalresistance, Elemental (Fire, Ice, Acid), Profile (NormalDamage))

import DND.DEFENSE.Armour (Armour (A), defaultArmour)
import DND.DEFENSE.Defense (Target (Target), Defense (Reflex, Defense))

import DND.SPELLS.Spell (SkillFeatProfile (None, Lesser), SpellFeat)
import DND.SPELLS.SpellFunctions (stabilize, noncombatspell, truestrike, shield, shockinggrasp, burninghands, blur, scorchingray, invisibility, blurpotion, shieldoffaithpotion,
      seeinvisibility, mirrorimage, lightningbolt, heroism, fireshield, phantasmalkiller, dominateperson, hideouslaughter, curewounds, hold, glowofhealth, solarfurnace, dispellmagic,
      confusion, catsgrace, bearsstrength, entangle, flamestrike, balefullpolymorth, barkskin, bullsendurancemass, divinefavour, bless, causewounds, healpoisen, causefear, doom,
      darkheal, sleep1, firebolt, shieldoffaith, protectionfrom, magearmour, magicmissile, fireballwand, grenade, divinegrenade, charmanimal, burninghandswand, tnaglefootbag, blackhole, 
      supernova, haste, mindthrust, slowpoison, keensenses, commandundead, removeaffliction, dragongland, holographicclone, improvedhurry, wallosteel, repulsionspell, destruction,
      fireballaura)
import DND.SPELLS.Summons (summonskeleton, summonnaturesally1, summonnaturesally2, summonnaturesally3, summonnaturesally4, summonnaturesally5, summonnaturesally6, summonvulture)

import DND.STATDAMAGE.StatDamage (StatDamage ,showsd)

import DND.STATUSEFFECTS.Effect (Effect)
import DND.STATUSEFFECTS.Effects (noEffects, isundead, (&), (-$-), addtemporal, Temporal (Absent, Off, Present, NA, On), isActive, 
      StatusEffect (..),
      Effects, effix, isNA, isOn, temporalduration, viewtempmodifyier)
import DND.STATUSEFFECTS.StatusEffectFunctions2 (t2ix)

import DND.Action (Action (Action))
import DND.Roll (Dice, Roll (runRoll), (~>), sumdice, roll)
import DND.TemporaryValue (Temporary, temporary, modifytemporary, temporarize, addtemporary)

type Location = (Int, Int)

defaultPicture :: Picture
defaultPicture = Color white $ Circle 300

data Character = Character {name :: String
                              , runLocation :: Location
                              , runStartLocation :: Location
                              , runPicture :: Picture
                              , mouseSelect:: Bool
                              , topofOrder:: Bool
                              , inCombat:: Bool
                              , pC :: Bool
                              , mousePoint:: Point
                              , status :: Status
                              , immediateVision :: [Location]
                              , illuminatedSquares :: [Location]} deriving (Generic, Show)

defaultcharacter :: Character
defaultcharacter = Character "Default Character" (0,0) (0,0) defaultPicture False False False False (0.0,0.0) emptystatus [] []

instance Eq Character where 
      (==) (Character name1 _ _ _ _ _ _ _ _ _ _ _) (Character name2 _ _ _ _ _ _ _ _ _ _ _) = name1 == name2

instance Ord Character where 
      (>=) (Character name1 _ _ _ _ _ _ _ _ _ _ _) (Character name2 _ _ _ _ _ _ _ _ _ _ _) = name1 >= name2
      compare (Character name1 _ _ _ _ _ _ _ _ _ _ _) (Character name2 _ _ _ _ _ _ _ _ _ _ _) = compare name1 name2

type CharacterUpdate = State Character

{-instance Show Character where
      show (Character name _) = show name-}

ishelpless :: Character -> Bool
ishelpless char
      | view (#status . #health) char == Dead = True
      | (isActive . temporary) (view (#status . #effects . #paralyzed) char) = True
      | (isActive . temporary) (view (#status . #effects . #confused) char) = True
      | (isActive . temporary) (view (#status . #effects . #fear) char) = True
      | (isActive . temporary) (view (#status . #effects . #nauseated) char) = True
      | (isActive . temporary) (view (#status . #effects . #stunned) char) = True
      | (view (#status . #health) char) == Dieing = True
      | (view (#status . #health) char) == Stabelized = True
      | otherwise = False

helplessornotincombat :: Character -> Bool
helplessornotincombat char = ishelpless char || (not . view (#inCombat) $ char)

showc :: Character -> String
showc (Character name _ _ _ _ _ _ _ _ status _ _) = show name ++ " " ++ (show (health status))

isMonster :: Character -> Bool
isMonster character
  | (combatmaneuver :| []) == special = False
  | otherwise = True
  where
    special = view (#status . #monsterAttack) character

detailedview :: Character ->  String
detailedview c = go7 ++ " : " ++ (show $ (currenthitpoints . status) c) ++ ", " ++ 
      (go1 $ ((temporary <$>) . effects . status) c) ++ " " ++ 
      (go5 $ (view (#status . #statdamage) c)) ++ " " ++ 
      go3 ++ " " ++ go6
      where
        go02 :: Maybe Int -> String
        go02 mi
            | mi == Just 1 = " Evil"
            | mi == Just 2 = " Good"
            | mi == Just 3 = " Lawful"
            | mi == Just 4 = " Chaotic"
            | otherwise = " Evil"
        go0 :: StatusEffect -> Temporal (Maybe Int) -> String
        go0 s t
            | (isActive t) && s == ProtectionFrom = abridged s ++ (go02 $ viewtempmodifyier t) ++ (go2 $ temporalduration t) ++ ", "
            | (isOn t) = abridged s ++ (go2 $ viewtempmodifyier t) ++ (go2 $ temporalduration t) ++ ", "
            | (isActive t) && (t /= (view (#permanent) $ effix s (view (#status . #effects) c))) = abridged s ++ (go2 $ viewtempmodifyier t) ++ (go2 $ temporalduration t) ++ ", "
            | (isNA t) && (t /= (view (#permanent) $ effix s (view (#status . #effects) c))) = "N/A" ++ abridged s ++ ", "
            | otherwise = []
            where
                  abridged s' = filter (\x -> x `elem` ['A'..'Z']) $ show s'
        go1 :: Effects (Temporal (Maybe Int)) -> String
        go1 eff = foldr (++) [] $ imap go0 eff
        go2 :: Maybe Int -> String
        go2 mi = case mi of
                  Nothing -> ""
                  Just 0 -> ""
                  Just y -> " " ++ show y
        go3 :: String
        go3
            | (isActive . temporary . view (#status . #effects . #poisened) $ c) = show . fmap go4 . view (#status . #abilityscores) $ c
            | otherwise = ""
        go4 :: Num a => Temporary a -> a
        go4 ta = view (#permanent) ta - temporary ta
        go5 :: Show a => [a] -> String
        go5 bs = case bs of
              [] -> ""
              cs -> show cs
        go6 :: String
        go6
            | (isActive . temporary . view (#status . #effects . #poisened) $ c) = ", Level " ++ (show . go4 $ level)
            | otherwise = ""
            where
            level = view (#status . #primclass . #level) $ c
        go7 :: String
        go7 = take 5 (name c) ++ filter (\x -> isDigit x) (name c)

detailedview' :: Character ->  (String, String)
detailedview' c = ("Sp: " ++ (show . view (#status . #movement) $ c) ++ ", " ++ (show $ (currenthitpoints . status) c) ++ go7 ++ go8, 
      (go1 $ ((temporary <$>) . effects . status) c) ++ " " ++ 
      (go9 $ (view (#status . #statdamage) c)) ++ " " ++ 
      go3 ++ " " ++ go6)
      where
        go02 :: Maybe Int -> String
        go02 mi
            | mi == Just 1 = " E"
            | mi == Just 2 = " G"
            | mi == Just 3 = " L"
            | mi == Just 4 = " C"
            | otherwise = " E"
        go0 :: StatusEffect -> Temporal (Maybe Int) -> String
        go0 s t
            | (isActive t) && s == ProtectionFrom = abridged s ++ (go02 $ viewtempmodifyier t) ++ (go2 $ temporalduration t) ++ (view (#character) t) ++ ", "
            | (isOn t) = abridged s ++ (go2 $ viewtempmodifyier t) ++ (go2 $ temporalduration t) ++ (view (#character) t) ++ ", "
            | (isActive t) && (t /= (view (#permanent) $ effix s (view (#status . #effects) c))) = abridged s ++ (go2 $ viewtempmodifyier t) ++ (go2 $ temporalduration t) ++ (view (#character) t) ++ ", "
            | (isNA t) && (t /= (view (#permanent) $ effix s (view (#status . #effects) c))) = "N/A" ++ abridged  s ++ ", "
            | otherwise = []
            where
                  abridged s' = filter (\x -> x `elem` ['A'..'Z']) $ show s'
        go1 :: Effects (Temporal (Maybe Int)) -> String
        go1 eff = foldr (++) [] $ imap go0 eff
        go2 :: Maybe Int -> String
        go2 mi = case mi of
                  Nothing -> ""
                  Just 0 -> ""
                  Just y -> " " ++ show y
        go3 :: String
        go3
            | isMonster c = show . fmap go4 . view (#status . #abilityscores) $ c
            | (isActive . temporary . view (#status . #effects . #poisened) $ c) = show . fmap go4 . view (#status . #abilityscores) $ c
            | otherwise = ""
        go4 :: Num a => Temporary a -> a
        go4 ta = view (#permanent) ta - temporary ta
        go5 :: Show a => [a] -> String
        go5 bs = case bs of
              [] -> ""
              cs -> show cs
        go9 :: Show a => [StatDamage a] -> String
        go9 bs = case bs of
              [] -> ""
              cs -> show $ showsd <$> cs
        go6 :: String
        go6
            | (isActive . temporary . view (#status . #effects . #poisened) $ c) = ", Level " ++ (show . go4 $ level)
            | otherwise = ""
            where
            level = view (#status . #primclass . #level) $ c
        go7 :: String
        go7 = " " ++ show current ++ "/" ++ show total
            where
            (current, total) = resolvepoints $ view (#status) c
        go8 :: String
        go8 = " " ++ show current
            where
            current = view (#status . #health) c

detailedview2 :: Character ->  String
detailedview2 c = name c ++ " : " ++ (show $ (currenthitpoints . status) c) ++ ", " ++ 
      (go1 $ ((temporary <$>) . effects . status) c) ++ " " ++ (go9 $ (view (#status . #statdamage) c)) ++ 
      ", " ++ go3 ++ ", " ++ go6 ++ ", "  ++ (go5 (go7 <$> (view (#status . #otherweapons) c))) ++ ", "  ++ 
      (wname . view (#status . #primaryhand) $ c) ++
      ", " ++ (aname . view (#status . #equipedarmour) $ c)
      where
        go0 :: StatusEffect -> Temporal (Maybe Int) -> String
        go0 s t
            | (isOn t) = show s ++ (go2 $ viewtempmodifyier t) ++ (go2 $ temporalduration t) ++ ", "
            | (isActive t) && (t /= (view (#permanent) $ effix s (view (#status . #effects) c))) = show s ++ (go2 $ temporalduration t) ++ (go2 $ viewtempmodifyier t) ++ ", "
            | (isNA t) && (t /= (view (#permanent) $ effix s (view (#status . #effects) c))) = "N/A" ++ show s ++ ", "
            | otherwise = []
        go1 :: Effects (Temporal (Maybe Int)) -> String
        go1 eff = foldr (++) [] $ imap go0 eff
        go2 :: Maybe Int -> String
        go2 mi = case mi of
                  Nothing -> ""
                  Just y -> " " ++ show y
        go3 :: String
        go3 = show . fmap go4 . view (#status . #abilityscores) $ c
        go4 :: Num a => Temporary a -> a
        go4 ta = view (#permanent) ta - temporary ta
        go5 :: Show a => [a] -> String
        go5 bs = case bs of
              [] -> ""
              cs -> show cs
        go6 :: String
        go6 = ", Level " ++ (show . fmap temporary $ level)
            where
            level = view (#status . #primclass) $ c
        go7 :: Either (MagicItem Int) (Either (Armour Int) Weapon) -> String
        go7 b = either miname (either aname wname) b
        go9 :: Show a => [StatDamage a] -> String
        go9 bs = case bs of
              [] -> ""
              cs -> show $ showsd <$> cs
        wname weapon = view (#name) weapon
        aname armour = view (#name) armour
        miname mitem = view (#name) mitem

skilldice :: (Lens' (Bonuses (BonusTypes (Temporary Int))) (BonusTypes (Temporary Int))) -> (Lens' (AbilityScores Int) Int) -> (Lens' Status Int) -> Character -> Dice (Int,Character)
skilldice lens1 lens2 lens3 char = state $ (\x -> skillcheck x lens1 lens2 lens3 char)

skillcheck :: StdGen -> (Lens' (Bonuses (BonusTypes (Temporary Int))) (BonusTypes (Temporary Int))) -> (Lens' (AbilityScores Int) Int) -> (Lens' Status Int) -> Character -> ((Int,Character), StdGen)
skillcheck g lens1 lens2 lens3 char = ((runRoll roll1 + abilitybon + skillbon + skill, char), g2)
      where
            status = view (#status) char
            skillbon = temporary . sum . view (lens1) $ gatherbonuses status
            abilitybon = view (lens2) $ abilitybonuses status
            skill = view (lens3) status
            (roll1, g2) = roll 20 g

skillcheckint :: Int -> Character -> StdGen -> (((Int, String),Character), StdGen)
skillcheckint i char g
      | i == 1 = go "Acrobatics" (\x -> x - (maybeint2int acpenalty) + abbonus) $ skillcheck g (#skills . #acrobatics) (#dexterity) (#skills . #acrobatics) char
      | i == 2 = go "Athletics" (\x -> x - (maybeint2int acpenalty) + abbonus) $ skillcheck g (#skills . #athletics) (#strength) (#skills . #athletics) char
      | i == 3 = go "Bluff" (\x -> x + abbonus) $ skillcheck g (#skills . #bluff) (#charisma) (#skills . #bluff) char
      | i == 4 = go "Computers" (\x -> x + abbonus) $ skillcheck g (#skills . #computers) (#intelligence) (#skills . #computers) char
      | i == 5 = go "Culture" (\x -> x + abbonus) $ skillcheck g (#skills . #culture) (#intelligence) (#skills . #culture) char
      | i == 6 = go "Diplomacy" (\x -> x + abbonus) $ skillcheck g (#skills . #diplomacy) (#charisma) (#skills . #diplomacy) char
      | i == 7 = go "Disguise" (\x -> x + abbonus) $ skillcheck g (#skills . #disguise) (#charisma) (#skills . #disguise) char
      | i == 8 = go "Engineering" (\x -> x + abbonus) $ skillcheck g (#skills . #engineering) (#intelligence) (#skills . #engineering) char
      | i == 9 = go "Intimidate" (\x -> x + abbonus) $ skillcheck g (#skills . #intimidate) (#charisma) (#skills . #intimidate) char
      | i == 10 = go "Life Science "  (\x -> x + abbonus) $ skillcheck g (#skills . #lifeScience) (#intelligence) (#skills . #lifeScience) char
      | i == 11 = go "Medicine" (\x -> x + abbonus) $ skillcheck g (#skills . #medicine) (#intelligence) (#skills . #medicine) char
      | i == 12 = go "Mysticism" (\x -> x + abbonus) $ skillcheck g (#skills . #mysticism) (#wisdom) (#skills . #mysticism) char
      | i == 13 = go "Perception" (\x -> x + abbonus) $ skillcheck g (#skills . #perception) (#wisdom) (#skills . #perception) char
      | i == 14 = go "Physical Science" (\x -> x + abbonus) $ skillcheck g (#skills . #physicalScience) (#intelligence) (#skills . #physicalScience) char
      | i == 15 = go "Piloting" (\x -> x + abbonus) $ skillcheck g (#skills . #piloting) (#dexterity) (#skills . #piloting) char
      | i == 16 = go "Profession" (\x -> x + abbonus) $ skillcheck g (#skills . #profession) (#wisdom) (#skills . #profession) char
      | i == 17 = go "Sense Motive" (\x -> x + abbonus) $ skillcheck g (#skills . #senseMotive) (#wisdom) (#skills . #senseMotive) char
      | i == 18 = go "Sleight of Hand" (\x -> x - (maybeint2int acpenalty) + abbonus) $ skillcheck g (#skills . #sleightOfHand) (#dexterity) (#skills . #sleightOfHand) char
      | i == 19 = go "Stealth" (\x -> x - (maybeint2int acpenalty) + abbonus) $ skillcheck g (#skills . #stealth) (#dexterity) (#skills . #stealth) char
      | i == 20 = go "Survival" (\x -> x + abbonus) $ skillcheck g (#skills . #survival) (#wisdom) (#skills . #survival) char
      where
            go :: String -> (a -> b) -> ((a, c), d) -> (((b, String), c), d)
            go name f ((a, b), c) = (((f a, name), b), c)
            acpenalty = view (#status . #equipedarmour . #achPenalty) char
            solarfilter
                  |(isActive . temporary $ view (#status . #effects . #gravitonmode) char) = 
                        (filter (\x -> x `elem` solarGravitonSkills) (view (#status . #classfeatures . #solarionfeatures . #sidereal) char))
                  |(isActive . temporary $ view (#status . #effects . #photonmode) char) = 
                        (filter (\x -> x `elem` solarPhotonSkills) (view (#status . #classfeatures . #solarionfeatures . #sidereal) char))
                  | otherwise = []
            solarbonus = if i `elem` solarfilter then (runRoll . fst $ roll 6 g) else 0
            envoybonus = if i `elem` (view (#status . #classfeatures . #envoyfeatures . #expertise) char) then envoyamount else 0
            envoyamount
                  | view (#status . #playerclasses . #env) char >= 20 = (runRoll . fst $ roll 8 g) + 4
                  | view (#status . #playerclasses . #env) char >= 17 = (runRoll . fst $ roll 8 g) + 3
                  | view (#status . #playerclasses . #env) char >= 13 = (runRoll . fst $ roll 8 g) + 2
                  | view (#status . #playerclasses . #env) char >= 9 = (runRoll . fst $ roll 6 g) + 2
                  | view (#status . #playerclasses . #env) char >= 5 = (runRoll . fst $ roll 6 g) + 1
                  | otherwise = (runRoll . fst $ roll 6 g)
            abbonus = solarbonus + envoybonus

solarPhotonSkills :: [Int]
solarPhotonSkills = [5, 6, 9, 11, 20]

solarGravitonSkills :: [Int]
solarGravitonSkills = [3, 7, 12, 17, 19]

classify :: Character -> ClassInfo (Temporary Int)
classify a = collapseclasses $ ((pure <$>) . playerclasses . status) a

sa21a :: Character -> Attack Int [Int] -> (Status,  Action (Attack [Int]) [Int])
sa21a c a = Action <$> (s2a True (view (#status) c) a)

sa2a :: (Lens' Status (NonEmpty (Attack Int [Int]))) -> Character -> (Status, [Action (Attack [Int]) [Int]])
sa2a lens c = (newstatus, newaction)
      where
            newstatus = fst $ sa21a c ((NE.head . (view (#status . lens))) c)
            sa = view (#status . lens) c
            newaction = NE.toList $ fmap (snd . (sa21a c)) sa


cm2a :: (Lens' Status (Attack Int [Int])) -> Maybe (Effect (Temporal [Int])) -> Character -> (Status, Action (Attack [Int]) [Int])
cm2a lens meff (Character _ _ _ _ _ _ _ _ _ s _ _) = (newstatus2,
  Action (Attack (AInfo n r inc t m (snd tohit1) d c mabi newalign prep spellproxy) att dam bonusdam mstd newmeff msdd mseff))
    where
      (Attack (AInfo n r inc t m oldtohit d c mabi _ prep spellproxy) att dam bonusdam mstd oldmeff msdd mseff) = cm
      newalign = temporary $ view (#alignment) s
      cm = view lens s
      rangebonus = (pure $ (strength . abilitybonuses) s)
      babbonus = (pure $ (temporary . bab . primclass) s)
      miscbonus = (pure $ (temporary . sum . bab . miscclass . gatherbonuses) s)
      improvedcbmbonus
            | mse == (Just Grappled) && improvedgrapple = Just 4
            | mse == (Just Prone) && improvedtrip = Just 4
            | mse == (Just Disarmed) && improveddisarm = Just 4
            | otherwise = Nothing
            where
            mse = view (#statuseffect) <$> newmeff
            improvedgrapple = isActive . temporary $ view (#effects . #improvedgrapple) s
            improvedtrip = isActive . temporary $ view (#effects . #improvedtrip) s
            improveddisarm = isActive . temporary $ view (#effects . #improveddisarm) s
      newtohit
            | (isActive . temporary) (view (#effects . #nauseated) s) = Nothing
            | otherwise = (rangebonus & babbonus & miscbonus & improvedcbmbonus) >>= (\x -> fmap (+ x) oldtohit)
      tohit1 :: (Status, Maybe [Int])
      tohit1
            | (isActive . temporary) $ view (#effects . #trueStrike) s = (news, (pure . (+ 20)) <$> newtohit)
            | otherwise = (s, pure <$> newtohit)
              where
                  news = over (#effects . #trueStrike) (addtemporal (Absent Nothing [] Nothing)) s
      newstatus
            | (isActive . temporary) $ view (#effects . #invisible) s = t2ix Invisible (Absent Nothing [] Nothing) $ fst tohit1
            | otherwise = fst tohit1
      newstatus2
            | (isActive . temporary) $ view (#effects . #stealth) s = t2ix Stealth (Absent Nothing [] Nothing) $ newstatus
            | otherwise = newstatus
      newmeff
            | meff == Nothing = oldmeff
            | (view (#statuseffect) <$> meff) == Just Grappled = set (#temporalmodifierbonus) newtohit <$> meff
            | otherwise = meff

fireshieldcharacter :: StdGen -> Maybe Character -> Character -> Character
fireshieldcharacter _ Nothing = id
fireshieldcharacter g (Just character)
      | (isActive . temporary) (view (#status . #effects . #fireshield) character) = over (#status) (mddstatus 0 20 (Just $ D d6 bonus (Just Fire) Nothing Negates))
      | otherwise = id
            where
                  status = view (#status) character
                  bonus = min 15 (max (effectivespelllevel (Just 5) status) (effectivespelllevel (Just 6) status))
                  d6 = (runRoll . fst) $ runState (sumdice [6]) g

fixresults :: [Result (Maybe Int)] -> [Result (Maybe Int)]
fixresults [] = []
fixresults (r : rs)
      | r == CriticalMiss = (fmap ((negate <$>)<$>) rs)
      | otherwise = (fixresults rs)

undoresults :: [Result (Maybe Int)] -> Character -> Character
undoresults rs c = snd $ runState (characterupdate ~> fixresults rs) c

testresult :: [Result (Maybe Int)]
testresult = [Hit (Just 15), CriticalMiss, CriticalHit (Just 15) (Just 3)]

elementalharmCharacter :: DamageDice Roll -> Character -> Character
elementalharmCharacter ddr c = over (#status) (healthupdate . (damager i)) c
  where
      i = dDEP2MI (runRoll <$> ddr) $ view (#status . #elementalresistance) c

harmCharacter :: Maybe Int -> Character -> Character
harmCharacter i c = over (#status) (healthupdate . (damager i)) c

result2damage :: Result (Maybe Int) -> Character -> ((), Character)
result2damage CriticalMiss c = ((), c)
result2damage (Miss _ _) c = ((), c)
result2damage (Hit a) c = ((), harmCharacter a go2)
      where
            go2 :: Character
            go2
              | (isActive . temporary) $ view (#status . #effects . #stoneSkin) c = over (#status . #effects . #stoneSkin) (modifytemporary (((+15) <$>) -$-)) c
              | otherwise = c
result2damage (CriticalHit a b) c = ((), over (#status) (healthupdate . (damager' (#injury) b)) $ harmCharacter (go a b) go2)
      where
            go a1 b1
                  | a1 < (Just 0) && b1 < (Just 0) = liftA2 (*) (fmap negate a1) (b1)
                  | otherwise = (liftA2 (*) a1 b1)
            go2 :: Character
            go2
              | (isActive . temporary) $ view (#status . #effects . #stoneSkin) c = over (#status . #effects . #stoneSkin) (modifytemporary (((+15) <$>) -$-)) c
              | otherwise = c

characterupdate :: Result (Maybe Int) -> CharacterUpdate ()
characterupdate r = state $ result2damage r

changeitem :: MagicItem Int -> Character -> Character
changeitem mitem character
      | isRing mitem && (view (#status . #equipment . #leftring) character == Nothing) = set (#status . #equipment . #leftring) (pure mitem) character
      | isRing mitem && (view (#status . #equipment . #rightring) character == Nothing) = set (#status . #equipment . #rightring) (pure mitem) character
      | otherwise = set (#status . #equipment . (eqix . view (#area) $ mitem)) (pure mitem) $
            over (#status . #otherweapons) (++ (catMaybes . (\x -> [x]) . fmap Left . view (#status . #equipment . (eqix . view (#area) $ mitem)) $ character)) character

removeequip :: Character -> Character
removeequip character = emptycharacter
      where
            emptycharacter = (removemagicitems . movearmourtoequipment . putinfirsthand . movefirsthandtoequipment) character
            firsthand = view (#status . #primaryhand) character
            movearmourtoequipment s'
                  | defaultArmour == (view (#status . #equipedarmour) s') = s'
                  | otherwise = set (#status . #equipedarmour) (defaultArmour) $ over (#status . #otherweapons) (++ [Right . Left $ view (#status . #equipedarmour) s']) s'
            removemagicitems s' = set (#status . #equipment) (pure Nothing) $ over (#status . #otherweapons) (++ (catMaybes .  foldl (\x y -> fmap Left y : x) [] $ (view (#status . #equipment) s'))) s'
            movefirsthandtoequipment s'
                  | firsthand == emptyweapon = s'
                  | otherwise = over (#status . #otherweapons) (\x -> x ++ [Right . Right $ firsthand]) s'
            putinfirsthand = set (#status . #primaryhand) (emptyweapon)
            emptyweapon = Weapon Slashing BasicMelee defaultDamageDice 5 [] "UnArmed"

changeweapons :: Maybe (Either (Armour Int) Weapon) -> Character -> Character
changeweapons Nothing character
      | otherwise = emptycharacter
      where
            emptycharacter = (putinfirsthand . movefirsthandtoequipment) character
            firsthand = view (#status . #primaryhand) character
            movefirsthandtoequipment s'
                  | firsthand == emptyweapon = s'
                  | otherwise = over (#status . #otherweapons) (\x -> x ++ [Right . Right $ firsthand]) s'
            putinfirsthand = set (#status . #primaryhand) (emptyweapon)
            emptyweapon = Weapon Slashing BasicMelee defaultDamageDice 5 [] "UnArmed"
changeweapons (Just armourorweapon) character
      | isdisarmed = character
      | inCombat character && isArmour = character
      | isArmour = over (#status) equiparmour $ over (#status) movearmourtoequipment character
      | otherwise = over (#status) (putinfirsthand . movefirsthandtoequipment) character
            where
                  status = view (#status) character
                  isdisarmed = (isActive . temporary) $ view (#effects . #disarmed) status
                  emptyweapon = Weapon Slashing BasicMelee defaultDamageDice 5 [] "UnArmed"
                  weapon = fromRight emptyweapon armourorweapon
                  firsthand = view (#primaryhand) status
                  isArmour = either (\_ -> True) (\_ -> False) armourorweapon
                  putinfirsthand = set (#primaryhand) (weapon)
                  movefirsthandtoequipment :: Status -> Status
                  movefirsthandtoequipment s'
                        | firsthand == emptyweapon = s'
                        | otherwise = over (#otherweapons) (\x -> x ++ [Right . Right $ firsthand]) s'
                  armour = view (#equipedarmour) status
                  movearmourtoequipment :: Status -> Status
                  movearmourtoequipment s'
                        | armour == defaultArmour = s'
                        | otherwise = over (#otherweapons) (\x -> x ++ [Right . Left $ armour]) s'
                  equiparmour :: Status -> Status
                  equiparmour s' = either (\x -> set (#equipedarmour) x s') (\_ -> s') armourorweapon


kas :: Character
kas = Character "Kasam" (0,0) (0,0) defaultPicture False False False True (0,0)
      (classupdate $ Status (pure . pure $ 0 :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Ally)
            (as 27 16 18 16 16 7)
            (set (#miscclass . #wil . #size) (pure 3 :: Temporary Int) (((pure . pure . pure) 0) :: Bonuses (BonusTypes (Temporary Int))))
            (A 5 5 Nothing Nothing "FullPlate")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "StaffLance")
            [(Right . Right $Weapon Slashing BasicMelee defaultDamageDice 5 [] "Pike")]
            (set (#basicMelee . #weaponfocus) True allproficient)
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Chaotic Good))
            (pure Orc)
            (pure 0)
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 2 0 0 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#perception) (18 :: Int) baseskills))
            []
            []

sep :: Character
sep = Character "Sephinne" (0,0) (0,0) defaultPicture False False False True (0,0)
      (classupdate $ Status (pure . pure $ 0 :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Ally)
            (as 21 18 14 18 12 28)
            (((pure . pure . pure) 0) :: Bonuses (BonusTypes (Temporary Int)))
            (A 5 5 Nothing Nothing "FullPlate")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Lance")
            [(Right . Right $Weapon Slashing BasicMelee defaultDamageDice 5 [] "LongSword"), (Right . Right $Weapon Slashing BasicMelee defaultDamageDice 5 [] "LongBow")]
            (set (#basicMelee . #weaponfeat) Specialization allproficient)
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Chaotic Good))
            (pure Halfling)
            cfeatures
            []
            []
            (pure Small)
            0
            Healthy
            []
            (Classes 6 0 0 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#stealth) (16 :: Int) baseskills))
            []
            []
             where
                  cfeatures :: ClassFeatures SpellTemplate Int
                  cfeatures = over (#arcanespells . #known . #first) (++ [summonskeleton, truestrike, shield, shockinggrasp, burninghands]) $
                              over (#arcanespells . #known . #second) (++ [blur, scorchingray, invisibility, seeinvisibility, mirrorimage]) $
                              over (#arcanespells . #known . #third) (++ [lightningbolt, heroism]) $
                              over (#arcanespells . #known . #fourth) (++ [fireshield, phantasmalkiller]) $
                              over (#arcanespells . #known . #fifth) (++ [dominateperson]) $
                              set (#feats . #arcanestrike) True emptyclassfeature

hud :: Character
hud = Character "Huda" (0,0) (0,0) defaultPicture False False False True (0,0)
      (classupdate $ Status (pure . pure $ 0 :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Ally)
            (as 18 18 18 22 20 30)
            (((pure . pure . pure) 0) :: Bonuses (BonusTypes (Temporary Int)))
            (A 5 5 Nothing Nothing "Breastplate")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Rapier")
            [(Right . Right $Weapon Slashing BasicMelee defaultDamageDice 5 [] "TwoHandedSword"), (Right . Right $Weapon Slashing BasicMelee defaultDamageDice 5 [] "Gun")]
            (allproficient)
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            (noReductions)
            (pure (Al Chaotic Good))
            (pure Gnome)
            cfeatures
            []
            []
            (pure Small)
            0
            Healthy
            []
            (Classes 5 0 0 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#stealth) (18 :: Int) baseskills))
            []
            []
             where
                  cfeatures :: ClassFeatures SpellTemplate Int
                  cfeatures = over (#arcanespells . #known . #first) (++ [shield, invisibility, hideouslaughter, (curewounds 1)]) $
                              over (#arcanespells . #known . #second) (++ [mirrorimage, hold, catsgrace]) $
                              over (#arcanespells . #known . #third) (++ [confusion]) $
                              over (#arcanespells . #known . #fourth) (++ [dominateperson, hold]) $
                              set (#feats . #arcanestrike) True $ set (#feats . #improvedinitiative) True emptyclassfeature

bor :: Character
bor = Character "Borris" (0,0) (0,0) defaultPicture False False False True (0,0)
      (classupdate $ Status (over (#ref) (+ 3) $ over (#wil) (+ 3) $ over (#fort) (+ 3) $ (pure . pure $ 0 :: ClassInfo (Temporary Int)))
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Ally)
            (as 18 19 14 14 24 11)
            (set (#defense . #attacks . #deflection) (pure 3 :: Temporary Int) (((pure . pure . pure) 0) :: Bonuses (BonusTypes (Temporary Int))))
            (A 5 5 Nothing Nothing "Breastplate")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "SlingStaff")
            [(Right . Right $Weapon Slashing BasicMelee defaultDamageDice 5 [] "Pike"), (Right . Right $Weapon Slashing BasicMelee defaultDamageDice 5 [] "SlingStaff")]
            (allproficient)
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            (noReductions)
            (pure (Al Chaotic Evil))
            (pure Halfling)
            cfeatures
            [flamestrike]
            [summonvulture]
            (pure Small)
            0
            Healthy
            []
            (Classes 2 0 0 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#perception) (12 :: Int) $ baseskills))
            []
            []
             where
                  cfeatures :: ClassFeatures SpellTemplate Int
                  cfeatures = over (#mysticspells . #known . #first) (++ [(curewounds 1), summonnaturesally1, entangle]) $
                              over (#mysticspells . #known . #second) (++ [summonnaturesally2, bearsstrength, hold, barkskin]) $
                              over (#mysticspells . #known . #third) (++ [summonnaturesally3, (curewounds 2), dominateperson]) $
                              over (#mysticspells . #known . #fourth) (++ [summonnaturesally4, flamestrike]) $
                              over (#mysticspells . #known . #fifth) (++ [summonnaturesally5, balefullpolymorth]) $
                              over (#mysticspells . #known . #sixth) (++ [summonnaturesally6, bullsendurancemass]) emptyclassfeature

pri :: Character
pri = Character "Princess" (0,0) (0,0) defaultPicture False False False True (0,0)
      (classupdate $ Status (over (#ref) (+ 3) $ over (#wil) (+ 3) $ over (#fort) (+ 3) $ (pure . pure $ 0 :: ClassInfo (Temporary Int)))
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Ally)
            (as 18 25 15 26 15 10)
            (((pure . pure . pure) 0) :: Bonuses (BonusTypes (Temporary Int)))
            (A 5 5 Nothing Nothing "Studded")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "LongBow")
            [(Right . Right $Weapon Slashing BasicMelee defaultDamageDice 5 [] "LongBow"), (Right . Right $Weapon Slashing BasicMelee defaultDamageDice 5 [] "Rapier")]
            (set (#basicMelee . #weaponfocus) True $ allproficient)
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            (noReductions)
            (pure (Al Chaotic Evil))
            (pure Elf)
            cfeatures
            []
            []
            (pure Medium)
            20
            Healthy
            []
            (Classes 4 0 0 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#stealth) (27 :: Int) $ set (#perception) (16 :: Int) $ baseskills))
            []
            []
             where
                  cfeatures :: ClassFeatures SpellTemplate Int
                  cfeatures = over (#arcanespells . #known . #first) (++ [truestrike, shield]) $
                              over (#arcanespells . #known . #second) (++ [invisibility, catsgrace]) $
                              over (#arcanespells . #known . #third) (++ []) emptyclassfeature
                              
skeleton :: Character
skeleton = Character "Skeleton" (0,0) (0,0) defaultPicture False False False False (0,0)
      (Status (cleric 1)
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Enemy)
            (as 18 16 12 20 12 14)
            ((pure . pure . pure) 0 :: Bonuses (BonusTypes (Temporary Int)))
            (A 5 5 Nothing Nothing "ChainMail")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Mace")
            [(Right . Right $ Weapon Slashing BasicMelee defaultDamageDice 5 [] "LongBow"), Right . Right $ Weapon Slashing BasicMelee defaultDamageDice 5 [] "Club"]
            allproficient
            (pure None)
            0
            0
            0
            undeadelementalresistance
            isundead
            noReductions
            (pure (Al Lawful Evil))
            (pure UnDead)
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
            baseskills)
            []
            []

orc :: Character
orc = Character "Kuroki Hin" (0,0) (0,0) defaultPicture False False False True (0,0)
      (classupdate $ Status (pure . pure $ 0 :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision Dark 60)
            (pure Ally)
            (as 18 12 18 16 14 9)
            (((pure . pure . pure) 0) :: Bonuses (BonusTypes (Temporary Int)))
            (A 5 5 Nothing Nothing "Breastplate")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "LongBow")
            [Left magicitemtest, Left magicitemtest2, Right . Right $ Weapon Slashing BasicMelee defaultDamageDice 5 [] "Javelin"]
            (allproficient)
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            (noReductions)
            (pure (Al Neutral Good))
            (pure Orc)
            cfeatures
            []
            [healpoisen, grenade, divinegrenade]
            (pure Medium)
            0
            Healthy
            []
            (Classes 4 0 0 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#perception) (6 :: Int) baseskills))
            []
            []
             where
                  cfeatures :: ClassFeatures SpellTemplate Int
                  cfeatures = emptyclassfeature                                                    

orcdruid :: Character
orcdruid = Character "Djorama" (0,0) (0,0) defaultPicture False False False True (0,0)
      (t2ix ImprovedGetem (Off Nothing [] Nothing) . classupdate $ 
            Status (pure . pure $ 0 :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision Dark 60)
            (pure Ally)
            (as 18 16 12 9 18 14)
            (over (#skills . #intimidate . #size) (+ 2) $ over (#skills . #lifeScience . #misc) (+ 3) $ over (#skills . #lifeScience . #holy) (+ 2) $ over (#skills . #survival . #holy) (+ 2) $ ((pure . pure . pure) 0) :: Bonuses (BonusTypes (Temporary Int)))
            (A 5 5 Nothing Nothing "Hide")
            (set (#forcefield) (Just (10,3)) (pure Nothing :: Equipment (Maybe (MagicItem Int))))
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "GreatClub")
            [Right . Right $ (Weapon Slashing BasicMelee defaultDamageDice 5 [] "SlingStaff"), Left $ ringofprotectionleft, Left $ ringofswimmingleft]
            (allproficient)
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            (noReductions)
            (pure (Al Chaotic NeutraL))
            (pure Orc)
            cfeatures
            []
            [blackhole]
            (pure Medium)
            0
            Healthy
            []
            (Classes 0 0 1 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#lifeScience) (4 :: Int) . set (#survival) (4 :: Int) . set (#intimidate) (4 :: Int) $ baseskills))
            []
            []
            where
                  cfeatures :: ClassFeatures SpellTemplate Int
                  cfeatures = over (#mysticspells . #known . #zero) (++ [(noncombatspell "Create Water"), (noncombatspell "Detect Magic"), (noncombatspell "Guidance"), stabilize]) $
                              over (#mysticspells . #known . #first) (++ [(curewounds 1), dispellmagic, entangle, charmanimal, confusion]) emptyclassfeature


synapticaccel2 :: MagicItem Int
synapticaccel2 = MagicItem (set (#abilityscores . #dexterity . #item) (4 :: Int) 
      ((pure . pure $ 0) :: Bonuses (BonusTypes Int)) :: Bonuses (BonusTypes Int)) 
      [] "Myofibril Mutualite" LeftRing

cranialaccel :: MagicItem Int
cranialaccel = MagicItem (set (#abilityscores . #intelligence . #item) (2 :: Int) 
      ((pure . pure $ 0) :: Bonuses (BonusTypes Int)) :: Bonuses (BonusTypes Int)) 
      [] "Cranial Stimulant" RightRing

cranialaccel2 :: MagicItem Int
cranialaccel2 = MagicItem (set (#abilityscores . #intelligence . #item) (4 :: Int) 
      ((pure . pure $ 0) :: Bonuses (BonusTypes Int)) :: Bonuses (BonusTypes Int)) 
      [] "Cranial Stimulant" RightRing

myofibilmutualite :: MagicItem Int
myofibilmutualite = MagicItem (set (#abilityscores . #strength . #item) (4 :: Int) 
      ((pure . pure $ 0) :: Bonuses (BonusTypes Int)) :: Bonuses (BonusTypes Int)) 
      [] "Myofibril Mutualite" LeftRing
synapticaccel :: MagicItem Int
synapticaccel = MagicItem (set (#abilityscores . #dexterity . #item) (2:: Int)
      ((pure . pure $ 0) :: Bonuses (BonusTypes Int)) :: Bonuses (BonusTypes Int)) 
      [] "Synaptic Accelerator" RightRing
ringofresistance3 :: MagicItem Int
ringofresistance3 = MagicItem (set (#defense . #saves . #item) (3 :: Int)
      ((pure . pure $ 0) :: Bonuses (BonusTypes Int)) :: Bonuses (BonusTypes Int)) 
      [] "Ring of Resistance 3" RightRing
kasathpowerArmour :: MagicItem Int
kasathpowerArmour = MagicItem (set (#abilityscores . #strength . #item) (4 :: Int) 
      ((pure . pure $ 0) :: Bonuses (BonusTypes Int)) :: Bonuses (BonusTypes Int)) 
      [] "Power Armour Core" LeftRing

stephen :: Character
stephen = Character "Stephen" (0,0) (0,0) defaultPicture False False False True (0,0)
      (t2ix Cleave (On Nothing [] (Just 1)) . 
      t2ix HIPS (Present Nothing [] Nothing) . 
      t2ix ImprovedDoubleAttack (Present Nothing [] (Just 1)) . 
      t2ix DarkMatter (Off Nothing [] Nothing) . 
      t2ix PhotonMode (Off Nothing [] Nothing) . 
      t2ix GravitonMode (Off Nothing [] Nothing) . 
      classupdate $ Status (pure . pure $ 0 :: ClassInfo (Temporary Int))
            (Just $ Vision NormalVision 10)
            (pure $ Vision NormalVision 60)
            (pure Ally)
            (as 19 19 13 18 8 19)
            (over (#skills . #profession . #size) (+ 2) . 
            over (#skills . #stealth . #size) (+ 6) . 
            over (#skills . #diplomacy . #size) (+ 2) . 
            set (#miscclass . #vp . #size) (pure 4 :: Temporary Int) . 
            set (#defense . #attacks . #deflection) (pure 2 :: Temporary Int) $ ((pure . pure . pure) 0 :: Bonuses (BonusTypes (Temporary Int))))
            (A 13 15 (Just 5) (Just 1) "Kasatha Microcord IV")
            (set (#forcefield) (Just (5,2)) . set (#leftring) (Just myofibilmutualite) . set (#rightring) (Just synapticaccel) $ (pure Nothing :: Equipment (Maybe (MagicItem Int))))
            (Weapon Bludgeoning AdvancedMelee (D [6,6,6,6,6] 0 Nothing (Just $ Target Defense 0) Negates) 5 [Fiery] "Comet Power-Fist")
            [Right . Right $ (Weapon Piercing HeavyWeapon (D [10,10] 0 Nothing (Just $ Target Defense 0) Negates) 60 [] "Light Machine Gun")]
            (set (#basicMelee . #weaponfeat) Specialization . 
            set (#advancedMelee . #weaponfeat) Specialization . 
            set (#heavyWeapon . #weaponfeat) Specialization . 
            fmap (set (#weaponfocus) True) $ noproficient)
            (pure None)
            0
            0
            0
            (set (#fire) (pure (NormalDamage (Just 10)) :: Temporary (Profile (Maybe Int))) normalelementalresistance)
            noEffects
            (dredux (\x _ -> pure x) Norm 5 noReductions)
            (pure (Al Chaotic Good))
            (pure Orc)
            cfeatures
            []
            [blackhole, supernova, glowofhealth, solarfurnace]
            (pure Medium)
            0
            Healthy
            []
            (Classes 0 0 0 0 12 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            (Just fireballaura)
            (set (#acrobatics) (15 :: Int) . set (#athletics) (15 :: Int) . set (#culture) (5 :: Int) . 
            set (#diplomacy) (15 :: Int) . set (#intimidate) (15 :: Int) . set (#perception) (15 :: Int) . 
            set (#physicalScience) (12 :: Int) . set (#profession) (4 :: Int) . set (#senseMotive) (15 :: Int) . 
            set (#stealth) (15 :: Int) $ baseskills))
            []
            []
            where
                  cfeatures :: ClassFeatures SpellTemplate Int
                  cfeatures = set (#solarionfeatures) (SolFeat True True [2,6,18,19]) emptyclassfeature


bob :: Character
bob = Character "Bob" (0,0) (0,0) defaultPicture False False False True (0,0)
      (t2ix TrickAttack (On Nothing [] (Just 2)) . 
      t2ix Evasion (Present Nothing [] Nothing) . 
      t2ix GreaterEvasion (Present Nothing [] Nothing) . 
      t2ix DeadlyAim (On Nothing [] Nothing) .  
      t2ix HIPS (Present Nothing [] Nothing) . 
      t2ix UncannyDodge (Present Nothing [] (Just 1)) . 
      t2ix Mobility (Present Nothing [] Nothing) . 
      t2ix TripleAttack (Present Nothing [] Nothing) .
      classupdate $ Status (pure . pure $ 0 :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision Dark 60)
            (pure Ally)
            (as 10 21 18 19 16 14)
            (over (#skills) (fmap (over (#luck) (+ 4))) .
            over (#skills . #sleightOfHand . #size) (+ 1) . 
            over (#initiative . #size) (+ 4) .  
            over (#skills . #acrobatics . #size) (+ 3) .  
            over (#skills . #stealth . #size) (+ 3) .  
            over (#skills . #bluff . #size) (+ 3) .  
            over (#skills . #piloting . #size) (+ 3) . 
            over (#skills . #stealth . #size) (+ 2) . 
            over (#skills . #survival . #size) (+ 2) . 
            over (#skills . #engineering . #size) (+ 2) . 
            over (#skills . #perception . #size) (+ 2) . 
            set (#miscclass . #vp . #size) (pure 3 :: Temporary Int) . 
            set (#miscclass . #ref . #size) (pure 3 :: Temporary Int) $ ((pure . pure . pure) 0 :: Bonuses (BonusTypes (Temporary Int))))
            (A 9 10 (Just 8) Nothing "Upgraded Imperial Azlanti Armour")
            (set (#forcefield) (Just (10,3)) . set (#leftring) (Just cranialaccel) . set (#rightring) (Just synapticaccel2) $ (pure Nothing :: Equipment (Maybe (MagicItem Int))))
            (Weapon Slashing BasicMelee (D [6,6] 0 Nothing (Just $ Target Defense 0) Negates) 5 [] "Dueling Buzz-Blade")
            [Right . Right $ (Weapon Piercing SmallArm (D [6,6,6] 0 Nothing (Just $ Target Defense 0) Negates) 60 [] "Elite Semi-Auto Pistol"),
            Right . Right $ (Weapon Piercing Sniper (D [8,8,8,8] 0 (Just Fire) (Just $ Target Defense 0) Negates) 500 [] "Elite Diasporan Rifle")]
            (set (#basicMelee . #weaponfeat) Specialization . 
            set (#smallArm . #weaponfeat) Specialization . 
            set (#sniper . #weaponfeat) Specialization . 
            fmap (set (#weaponfocus) True) $ noproficient)
            (pure None)
            0
            0
            0
            (set (#acid) (pure (NormalDamage (Just 5)) :: Temporary (Profile (Maybe Int))) normalelementalresistance)
            noEffects
            (dredux (\x _ -> pure x) Norm 5 noReductions)
            (pure (Al Chaotic Evil))
            (pure Gnome)
            cfeatures
            []
            [dragongland, holographicclone]
            (pure Small)
            0
            Healthy
            []
            (Classes 0 0 0 12 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            60
            0
            Nothing
            (set (#acrobatics) (15 :: Int) . set (#athletics) (15 :: Int) . set (#bluff) (15 :: Int) . set (#computers) (15 :: Int) . 
            set (#culture) (15 :: Int) . set (#diplomacy) (15 :: Int) . set (#disguise) (15 :: Int) . set (#engineering) (15 :: Int) . 
            set (#intimidate) (15 :: Int) . set (#medicine) (9 :: Int) . set (#perception) (15 :: Int) . set (#piloting) (15 :: Int) . 
            set (#senseMotive) (15 :: Int) . set (#sleightOfHand) (15 :: Int) . set (#stealth) (15 :: Int)  . set (#survival) (15 :: Int) $ baseskills))
            []
            []
            where
                  cfeatures :: ClassFeatures SpellTemplate Int
                  cfeatures = emptyclassfeature

ancillary :: Character
ancillary = Character "Ancillary" (0,0) (0,0) defaultPicture False False False True (0,0)
      (t2ix DeadlyAim (On Nothing [] Nothing) .
      t2ix ExpertAttack (Off Nothing [] Nothing) .
      t2ix Mobility (Present Nothing [] Nothing) .
      classupdate $ Status (pure . pure $ 0 :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision Dark 60)
            (pure Ally)
            (as 16 22 12 18 12 21)
            (over (#skills) (fmap (over (#luck) (+ 4))) .
            over (#skills . #medicine . #size) (+ 2) .
            over (#skills . #bluff . #size) (+ 5) .  
            over (#skills . #lifeScience . #size) (+ 1) .
            over (#skills . #engineering . #size) (+ 3) . 
            set (#miscclass . #vp . #size) (pure 4 :: Temporary Int) . 
            set (#miscclass . #ref . #size) (pure 2 :: Temporary Int) $ ((pure . pure . pure) 0 :: Bonuses (BonusTypes (Temporary Int))))
            (A 12 13 (Just 6) Nothing "Freebooter Armour III")
            (set (#forcefield) Nothing . set (#leftring) (Just cranialaccel2) $ (pure Nothing :: Equipment (Maybe (MagicItem Int))))
            (Weapon Slashing BasicMelee (D [6,6] 0 Nothing (Just $ Target Defense 0) Negates) 5 [] "Dueling Buzz-Blade")
            [Right . Right $ (Weapon Piercing LongArm (D [4,4,4,4,4,4] 0 (Just Fire) (Just $ Target Defense 0) Negates) 60 [] "Autobeam Rifle"),
            Right . Right $ (Weapon Piercing Grenade (D [6,6,6,6,6,6] 0 Nothing (Just $ Target Reflex 0) Half) 15 [] "Flack Genade IV")]
            (set (#basicMelee . #weaponfeat) Specialization . 
            set (#smallArm . #weaponfeat) Specialization . 
            set (#longArm . #weaponfeat) Specialization .  
            set (#grenade . #weaponfeat) Specialization . 
            set (#longArm . #weaponfocus) True $ noproficient)
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            (dredux (\x _ -> pure x) Pierce 5 noReductions)
            (pure (Al Chaotic Evil))
            (pure Human)
            cfeatures
            []
            [improvedhurry]
            (pure Medium)
            0
            Healthy
            []
            (Classes 12 0 0 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#acrobatics) (15 :: Int) . set (#athletics) (9 :: Int) . set (#bluff) (15 :: Int) . set (#computers) (9 :: Int) . 
            set (#culture) (15 :: Int) . set (#diplomacy) (9 :: Int) . set (#disguise) (15 :: Int) . set (#engineering) (15 :: Int) . 
            set (#lifeScience) (15 :: Int) . set (#medicine) (9 :: Int) . set (#perception) (15 :: Int) . set (#piloting) (9 :: Int) . 
            set (#senseMotive) (15 :: Int) . set (#sleightOfHand) (9 :: Int) . set (#stealth) (15 :: Int) $ baseskills))
            []
            []
            where
                  cfeatures :: ClassFeatures SpellTemplate Int
                  cfeatures = set (#envoyfeatures) (EnvFeat [1,8,17,19] True) emptyclassfeature

michelle :: Character
michelle = Character "Michelle" (0,0) (0,0) defaultPicture False False False True (0,0)
      (t2ix Cleave (On Nothing [] Nothing) .  
      t2ix ArcanStrike (On Nothing [] Nothing) . 
      t2ix SpellFocus (Present Nothing [] Nothing) . 
      classupdate $ Status (pure . pure $ 0 :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision Dark 60)
            (pure Ally)
            (as 16 12 18 20 20 10)
            (over (#skills) (fmap (over (#holy) (+ 2))) .
            over (#skills . #medicine . #size) (+ 3) . 
            over (#skills . #senseMotive . #size) (+ 3) . 
            over (#skills . #computers . #size) (+ 7) . 
            over (#skills . #mysticism . #size) (+ 5) .
            over (#skills . #perception . #size) (+ 1) . 
            over (#skills . #culture . #size) (+ 2) . 
            over (#skills . #diplomacy . #size) (+ 2) . 
            set (#miscclass . #vp . #size) (pure 6 :: Temporary Int) $ ((pure . pure . pure) 0 :: Bonuses (BonusTypes (Temporary Int))))
            (A 10 13 (Just 2) (Just 3) "Kasatha Power Armour")
            (set (#leftring) (Just kasathpowerArmour) . set (#rightring) (Just ringofresistance3) $ (pure Nothing :: Equipment (Maybe (MagicItem Int))))
            (Weapon Slashing BasicMelee (D [6,6,6] 0 Nothing (Just $ Target Defense 0) Negates) 5 [] "Ultra Thin Dueling Sword")
            [Right . Right $ (Weapon Piercing SmallArm (D [6,6] 0 (Just Acid) (Just $ Target Defense 0) Negates) 30 [] "Aphelion Blasting Pistol")]
            (set (#basicMelee . #weaponfeat) Specialization . 
            set (#smallArm . #weaponfeat) Specialization . 
            set (#basicMelee . #weaponfocus) True $ noproficient)
            (pure None)
            0
            0
            0
            (set (#lightning) (pure (NormalDamage (Just 5)) :: Temporary (Profile (Maybe Int))) normalelementalresistance)
            noEffects
            noReductions
            (pure (Al Chaotic Good))
            (pure Elf)
            cfeatures
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 0 0 12 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#athletics) (15 :: Int) . set (#computers) (15 :: Int) . set (#culture) (15 :: Int) . 
            set (#diplomacy) (15 :: Int) . set (#lifeScience) (15 :: Int) . set (#medicine) (15 :: Int) . 
            set (#mysticism) (12 :: Int) . set (#perception) (4 :: Int) . set (#physicalScience) (15 :: Int) . 
            set (#senseMotive) (15 :: Int) . set (#survival) (15 :: Int) $ baseskills))
            []
            []
            where
                  cfeatures :: ClassFeatures SpellTemplate Int
                  cfeatures = over (#mysticspells . #known . #zero) (++ [(noncombatspell "Telepathic Message"), (noncombatspell "Detect Magic"), (noncombatspell "Psychokinetic Hand"), stabilize, wallosteel, repulsionspell, destruction]) $
                              over (#mysticspells . #known . #first) (++ [(noncombatspell "Charm"), (noncombatspell "Identify"), (noncombatspell "Detect Thoughts"), (curewounds 1), slowpoison, keensenses, (mindthrust 1)]) $
                              over (#mysticspells . #known . #second) (++ [(noncombatspell "Augury"), (noncombatspell "Zone of Truth"), (noncombatspell "Guidance"), (curewounds 2), commandundead, seeinvisibility, invisibility, healpoisen, (mindthrust 2)]) $
                              over (#mysticspells . #known . #third) (++ [(noncombatspell "Suggestion"), (noncombatspell "Clairvoyance"), (curewounds 3), dispellmagic, haste, removeaffliction, (mindthrust 3)]) $
                              over (#mysticspells . #known . #fourth) (++ [(noncombatspell "Divination"), (curewounds 4), confusion, summonskeleton, (mindthrust 4)]) emptyclassfeature

barbear :: Character
barbear = Character "Barbearian" (0,0) (0,0) defaultPicture False False False True (0,0)
      (t2ix Cleave (Present Nothing [] Nothing) . classupdate $ Status (pure . pure $ 0 :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision Low 60)
            (pure Ally)
            (as 18 16 18 9 14 12)
            (over (#skills . #intimidate . #size) (+ 2) $ ((pure . pure . pure) 0) :: Bonuses (BonusTypes (Temporary Int)))
            (A 5 5 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "TwoHandedAxe")
            []
            (allproficient)
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            (noReductions)
            (pure (Al Chaotic NeutraL))
            (pure Orc)
            cfeatures
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 1 0 0 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            50
            0
            Nothing
            (set (#athletics) (4 :: Int) . set (#survival) (4 :: Int) . set (#intimidate) (4 :: Int) $ baseskills))
            []
            []
            where
                  cfeatures :: ClassFeatures SpellTemplate Int
                  cfeatures = emptyclassfeature
temple :: Character
temple = Character "Temple" (0,0) (0,0) defaultPicture False False False True (0,0)
      (t2ix TrickAttack (On Nothing [] (Just 2)) . classupdate $ Status (pure . pure $ 0 :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Ally)
            (as 12 18 9 18 12 16)
            (over (#defense . #attacks . #luck) (addtemporary 1) $ ((pure . pure . pure) 0) :: Bonuses (BonusTypes (Temporary Int)))
            (A 5 5 Nothing Nothing "ChainShirt")
            (pure Nothing)
            (Weapon Slashing SmallArm (D [8] 0 Nothing (Just $ Target Reflex 0) Half) 50 [] "Grenade")
            []
            (allproficient)
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            (noReductions)
            (pure (Al Chaotic NeutraL))
            (pure Orc)
            cfeatures
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 1 0 0 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#bluff) (4 :: Int) . set (#diplomacy) (4 :: Int) . set (#engineering) (4 :: Int) . set (#mysticism) (4 :: Int) 
            . set (#stealth) (4 :: Int) . set (#perception) (4 :: Int) . set (#computers) (4 :: Int) 
            . set (#medicine) (4 :: Int) . set (#culture) (4 :: Int) . set (#profession) (4 :: Int) 
            . set (#piloting) (4 :: Int) . set (#sleightOfHand) (4 :: Int) $ set (#acrobatics) (4 :: Int) baseskills))
            []
            []
            where
                  cfeatures :: ClassFeatures SpellTemplate Int
                  cfeatures = set (#feats . #weaponfinesse) True $ 
                              set (#feats . #improvedinitiative) True $ emptyclassfeature

guildrogue :: Character
guildrogue = Character "Rogue" (0,0) (0,0) defaultPicture False False False False (0,0)
      (classupdate $ Status (pure . pure $ 0 :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Enemy)
            (as 14 15 13 12 10 10)
            (((pure . pure . pure) 0) :: Bonuses (BonusTypes (Temporary Int)))
            (A 5 5 Nothing Nothing "Leather")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "ShortSword")
            [Right . Right $ Weapon Slashing BasicMelee defaultDamageDice 5 [] "ShortBow"]
            (set (#basicMelee . #weaponfocus) True allproficient)
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            (noReductions)
            (pure (Al Chaotic NeutraL))
            (pure Orc)
            cfeatures
            []
            [tnaglefootbag]
            (pure Medium)
            0
            Healthy
            []
            (Classes 1 0 0 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#acrobatics) (4 :: Int) . set (#piloting) (4 :: Int) . set (#bluff) (4 :: Int) . set (#athletics) (4 :: Int) 
            . set (#engineering) (4 :: Int) . set (#disguise) (4 :: Int) . set (#senseMotive) (4 :: Int) 
            . set (#perception) (4 :: Int) . set (#sleightOfHand) (4 :: Int) . set (#stealth) (4 :: Int) $ baseskills))
            []
            []
            where
                  cfeatures :: ClassFeatures SpellTemplate Int
                  cfeatures = set (#feats . #improvedinitiative) True emptyclassfeature

kit :: Character
kit = Character "Satomi Kane" (0,0) (0,0) defaultPicture False False False True (0,0)
      (classupdate $ Status (pure . pure $ 0)
            Nothing
            (pure $ Vision Low 60)
            (pure Ally)
            (as 10 18 12 16 10 18)
            (((pure . pure . pure) 0) :: Bonuses (BonusTypes (Temporary Int)))
            (A 5 5 Nothing Nothing "ChainShirt")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "ShortBow")
            [Right . Right $Weapon Slashing BasicMelee defaultDamageDice 5 [] "Rapier"]
            (allproficient)
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            (noReductions)
            (pure (Al Chaotic Evil))
            (pure Kitsune)
            cfeatures
            [summonnaturesally4]
            [healpoisen, firebolt, grenade, divinegrenade]
            (pure Medium)
            0
            Healthy
            []
            (Classes 4 0 0 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills)
            []
            []
             where
                  cfeatures :: ClassFeatures SpellTemplate Int
                  cfeatures = over (#arcanespells . #known . #first) (++ [summonnaturesally1, hideouslaughter, curewounds 1]) $
                              over (#arcanespells . #known . #second) (++ [invisibility, curewounds 2]) $
                              set (#feats . #arcanestrike) True $ set (#feats . #weaponfinesse) True emptyclassfeature   
                                                                   
sorceror :: Character
sorceror = Character "Bob" (0,0) (0,0) defaultPicture False False False True (0,0)
      (classupdate $ Status (pure . pure $ 0)
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Ally)
            (as 10 16 18 14 12 18)
            (((pure . pure . pure) 0) :: Bonuses (BonusTypes (Temporary Int)))
            (A 5 5 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "ShortBow")
            []
            (allproficient)
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            (noReductions)
            (pure (Al Neutral NeutraL))
            (pure Human)
            cfeatures
            [fireballwand]
            [healpoisen, firebolt, grenade, divinegrenade, magearmour]
            (pure Medium)
            0
            Healthy
            []
            (Classes 4 0 0 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills)
            []
            []
             where
                  cfeatures :: ClassFeatures SpellTemplate Int
                  cfeatures = over (#arcanespells . #known . #first) (++ [shield, burninghands, summonskeleton, magearmour]) $
                                over (#arcanespells . #known . #second) (++ [scorchingray]) $
                                set (#feats . #arcanestrike) True emptyclassfeature
                                                                   
sorcerorrogue :: Character
sorcerorrogue = Character "Chimalpilli" (0,0) (0,0) defaultPicture False False False False (0,0)
      (classupdate $ Status (pure . pure $ 0)
            Nothing
            (pure $ Vision Low 60)
            (pure Ally)
            (as 12 18 10 12 8 18)
            (((pure . pure . pure) 0) :: Bonuses (BonusTypes (Temporary Int)))
            (A 5 5 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "ShortBow")
            []
            (allproficient)
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            (noReductions)
            (pure (Al Neutral NeutraL))
            (pure Human)
            cfeatures
            []
            [summonskeleton]
            (pure Medium)
            0
            Healthy
            []
            (Classes 1 0 0 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills)
            []
            []
             where
                  cfeatures :: ClassFeatures SpellTemplate Int
                  cfeatures = over (#arcanespells . #known . #first) (++ [burninghands, magearmour]) $
                                set (#feats . #arcanestrike) True emptyclassfeature

ten :: Character
ten = Character "Yuki Rio" (0,0) (0,0) defaultPicture False False False True (0,0)
      (classupdate $ Status (pure . pure $ 0)
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Ally)
            (as 16 18 10 10 18 15)
            (((pure . pure . pure) 0) :: Bonuses (BonusTypes (Temporary Int)))
            (A 5 5 Nothing Nothing "ChainShirt")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Kukri")
            [Right . Right $ Weapon Slashing BasicMelee defaultDamageDice 5 [] "ShortSword", Right . Right $ Weapon Slashing BasicMelee defaultDamageDice 5 [] "CompositeShortBow"]
            (allproficient)
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Neutral Good))
            (pure Tengu)
            cfeatures
            [darkheal, darkheal, darkheal, darkheal, channelenergy']
            [healpoisen, grenade, divinegrenade]
            (pure Medium)
            0
            Healthy
            []
            (Classes 4 0 0 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills)
            []
            []
             where
                  cfeatures :: ClassFeatures SpellTemplate Int
                  cfeatures = over (#mysticspells . #known . #first) (++ [curewounds 1, bless, divinefavour, causewounds 1, shieldoffaith, protectionfrom "Evil", summonnaturesally1]) $
                              over (#mysticspells . #known . #second) (++ [hold, bearsstrength, curewounds 2, barkskin]) $
                              set (#feats . #improveddisarm) True emptyclassfeature

abess :: Character
abess = Character "Abess Loyola" (0,0) (0,0) defaultPicture False False False False (0,0)
      (classupdate $ Status (pure . pure $ 0)
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Enemy)
            (as 16 12 16 10 18 15)
            (((pure . pure . pure) 0) :: Bonuses (BonusTypes (Temporary Int)))
            (A 5 5 Nothing Nothing "Breastplate")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "LongBow")
            [Right . Right $ Weapon Slashing BasicMelee defaultDamageDice 5 [] "CompositeShortBow"]
            (allproficient)
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Neutral Good))
            (pure Human)
            cfeatures
            [darkheal, darkheal, darkheal, darkheal, channelenergy']
            [healpoisen, grenade, divinegrenade]
            (pure Medium)
            0
            Healthy
            []
            (Classes 6 0 0 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills)
            []
            []
             where
                  cfeatures :: ClassFeatures SpellTemplate Int
                  cfeatures = over (#mysticspells . #known . #first) (++ [curewounds 1, bless, divinefavour, causewounds 1, shieldoffaith, protectionfrom "Evil", summonnaturesally1]) $
                              over (#mysticspells . #known . #second) (++ [hold, bearsstrength, curewounds 2, barkskin]) $
                              over (#mysticspells . #known . #third) (++ [(curewounds 3), dominateperson]) $
                              set (#feats . #improveddisarm) True $
                              set (#feats . #arcanestrike) True emptyclassfeature

conquistador :: Character
conquistador = Character "Ignathio De La Cruz" (0,0) (0,0) defaultPicture False False False False (0,0)
      (classupdate $ Status (pure . pure $ 0 :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Enemy)
            (as 16 18 16 12 15 10)
            miscbonuses
            (A 5 5 Nothing Nothing "Breastplate")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Arquebus")
            [(Right . Right $Weapon Slashing BasicMelee defaultDamageDice 5 [] "LongSword")]
            conquistadorprofile
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Lawful Evil))
            (pure Human)
            cfeatures
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 5 0 0 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills)
            []
            []
             where                   
                  conquistadorprofile :: Weapons FeatProfile
                  conquistadorprofile = set (#longArm . #weaponfocus) True $ set (#longArm . #weaponfeat) (Specialization) $ allproficient
                  miscbonuses :: Bonuses (BonusTypes (Temporary Int))
                  miscbonuses = set (#miscclass . #ref . #size) (pure 3 :: Temporary Int) (((pure . pure . pure) 0) :: Bonuses (BonusTypes (Temporary Int)))
                  cfeatures :: ClassFeatures SpellTemplate Int
                  cfeatures = set (#feats . #deadlyaim) True emptyclassfeature

sherrif :: Character
sherrif = Character "Pedro De Valdivia" (0,0) (0,0) defaultPicture False False False False (0,0)
      (classupdate $ Status (pure . pure $ 0 :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Enemy2)
            (as 16 13 14 10 12 9)
            miscbonuses
            (A 5 5 Nothing Nothing "Breastplate")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "TwoHandedSword")
            [(Right . Right $Weapon Slashing BasicMelee defaultDamageDice 5 [] "Arquebus")]
            conquistadorprofile
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            noReductions
            (pure (Al Lawful Evil))
            (pure Human)
            cfeatures
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 4 0 0 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills)
            []
            []
             where                   
                  conquistadorprofile :: Weapons FeatProfile
                  conquistadorprofile = set (#basicMelee . #weaponfocus) True $ set (#basicMelee . #weaponfeat) (Specialization) $ allproficient
                  miscbonuses :: Bonuses (BonusTypes (Temporary Int))
                  miscbonuses = set (#defense . #attacks . #luck) (pure 1) $ 
                        set (#miscclass . #ref . #size) (pure 3 :: Temporary Int) 
                        (((pure . pure . pure) 0) :: Bonuses (BonusTypes (Temporary Int)))
                  cfeatures :: ClassFeatures SpellTemplate Int
                  cfeatures = emptyclassfeature

ringofprotectionleft :: MagicItem Int
ringofprotectionleft = 
      MagicItem (set (#defense . #attacks . #item) (1 :: Int) $ 
      set (#defense . #saves . #item) (1 :: Int)
      ((pure . pure $ 0) :: Bonuses (BonusTypes Int)) :: Bonuses (BonusTypes Int)) 
      [] "Ring of Protection +1" LeftRing
ringofswimmingleft :: MagicItem Int
ringofswimmingleft = 
      MagicItem (set (#skills . #athletics . #item) (53:: Int)
      ((pure . pure $ 0) :: Bonuses (BonusTypes Int)) :: Bonuses (BonusTypes Int)) 
      [] "Ring of Swimming" LeftRing

pug :: Character
pug = Character "Pudge Waverly" (0,0) (0,0) defaultPicture False False False False (0,0)
      (classupdate $ Status (pure . pure $ 0)
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Enemy)
            (as 6 13 10 15 14 9)
            (set (#miscclass . #hp . #penalty) (pure 3 :: Temporary Int) ((pure . pure . pure) 0 :: Bonuses (BonusTypes (Temporary Int))) :: Bonuses (BonusTypes (Temporary Int)))
            (A 5 5 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Gun")
            []
            (allproficient)
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            (noReductions)
            (pure (Al Neutral NeutraL))
            (pure Orc)
            cfeatures
            []
            [blurpotion, blurpotion]
            (pure Medium)
            0
            Healthy
            []
            (Classes 1 0 0 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills)
            []
            []
             where
                  cfeatures :: ClassFeatures SpellTemplate Int
                  cfeatures = over (#arcanespells . #known . #first) (++ [shield, magicmissile]) emptyclassfeature

knu :: Character
knu = Character "Knu" (0,0) (0,0) defaultPicture False False False False (0,0)
      (classupdate $ Status (pure . pure $ 0)
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Enemy)
            (as 8 15 10 10 9 12)
            (((pure . pure . pure) 0) :: Bonuses (BonusTypes (Temporary Int)))
            (A 5 5 Nothing Nothing "Natural")
            (set (#leftring) (Just ringofprotectionleft) $ (pure Nothing :: Equipment (Maybe (MagicItem Int))) :: Equipment (Maybe (MagicItem Int)))
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Kukri")
            []
            (allproficient)
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            (noReductions)
            (pure (Al Neutral NeutraL))
            (pure Human)
            cfeatures
            []
            [burninghandswand]
            (pure Medium)
            0
            Healthy
            []
            (Classes 1 0 0 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills)
            []
            []
             where
                  cfeatures :: ClassFeatures SpellTemplate Int
                  cfeatures = over (#arcanespells . #known . #first) (++ [hideouslaughter, sleep1]) $
                              set (#feats . #weaponfinesse) True emptyclassfeature

grelm :: Character
grelm = Character "Grelm Hammercloack" (0,0) (0,0) defaultPicture False False False False (0,0)
      (classupdate $ Status (pure . pure $ 0)
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Enemy)
            (as 13 16 14 9 12 10)
            (((pure . pure . pure) 0) :: Bonuses (BonusTypes (Temporary Int)))
            (A 5 5 Nothing Nothing "Scale")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "TwoHandedSword")
            [Right . Right $ Weapon Slashing BasicMelee defaultDamageDice 5 [] "CompositeLongBow"]
            (grelmprofile)
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            (noReductions)
            (pure (Al Neutral NeutraL))
            (pure Human)
            cfeatures
            []
            [curewounds 1, curewounds 1]
            (pure Medium)
            0
            Healthy
            []
            (Classes 1 0 0 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            20
            0
            Nothing
            baseskills)
            []
            []
             where
                  cfeatures :: ClassFeatures SpellTemplate Int
                  cfeatures = emptyclassfeature
                  grelmprofile :: Weapons FeatProfile
                  grelmprofile = set (#basicMelee . #weaponfocus) True allproficient

nirashi :: Character
nirashi = Character "Nirashi Sylvanmede" (0,0) (0,0) defaultPicture False False False False (0,0)
      (classupdate $ Status (pure . pure $ 0)
            Nothing
            (pure $ Vision Low 60)
            (pure Enemy)
            (as 14 8 10 12 16 13)
            (((pure . pure . pure) 0) :: Bonuses (BonusTypes (Temporary Int)))
            (A 5 5 Nothing Nothing "HalfPlate")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "StaffLance")
            [Right . Right $ Weapon Slashing BasicMelee defaultDamageDice 5 [] "Javelin"]
            (allproficient)
            (pure None)
            0
            0
            0
            normalelementalresistance
            (set (#sleep) (pure $ NA Nothing [] Nothing) noEffects)
            (noReductions)
            (pure (Al Neutral NeutraL))
            (pure Elf)
            cfeatures
            []
            [shieldoffaithpotion 2, shieldoffaithpotion 2]
            (pure Medium)
            0
            Healthy
            []
            (Classes 1 0 0 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            20
            0
            Nothing
            baseskills)
            []
            []
             where
                  cfeatures :: ClassFeatures SpellTemplate Int
                  cfeatures = over (#mysticspells . #known . #first) (++ [curewounds 1, causefear, doom, causewounds 1]) emptyclassfeature

tenrogue :: Character
tenrogue = Character "Ashikaga Yoshi" (0,0) (0,0) defaultPicture False False False True (0,0)
      (classupdate $ Status (pure . pure $ 0)
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Ally)
            (as 16 18 15 18 10 10)
            (((pure . pure . pure) 0) :: Bonuses (BonusTypes (Temporary Int)))
            (A 5 5 Nothing Nothing "ChainShirt")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Kukri")
            [Right . Right $ Weapon Slashing BasicMelee defaultDamageDice 5 [] "ShortSword", Right . Right $ Weapon Slashing BasicMelee defaultDamageDice 5 [] "ShortBow"]
            (set (#basicMelee . #weaponfocus) True allproficient)
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            (noReductions)
            (pure (Al Chaotic NeutraL))
            (pure Tengu)
            cfeatures
            []
            [healpoisen, grenade, divinegrenade]
            (pure Medium)
            0
            Healthy
            []
            (Classes 4 0 0 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            (set (#info . #attackbonus) (Just 4) combatmaneuver)
            0
            30
            0
            Nothing
            baseskills)
            []
            []
             where
                  cfeatures :: ClassFeatures SpellTemplate Int
                  cfeatures = emptyclassfeature

thug :: Character
thug = Character "Street Thug" (0,0) (0,0) defaultPicture False False False False (0,0)
      (classupdate $ Status (pure . pure $ 0)
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Enemy)
            (as 16 15 13 8 10 12)
            (((pure . pure . pure) 0) :: Bonuses (BonusTypes (Temporary Int)))
            (A 5 5 Nothing Nothing "Studded")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Club")
            []
            (allproficient)
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            (noReductions)
            (pure (Al Chaotic Evil))
            (pure Human)
            cfeatures
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 1 0 0 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills)
            []
            []
             where
                  cfeatures :: ClassFeatures SpellTemplate Int
                  cfeatures = emptyclassfeature

wakochief :: Character
wakochief = Character "Wako chief" (0,0) (0,0) defaultPicture False False False False (0,0)
      (classupdate $ Status (pure . pure $ 0)
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Enemy)
            (as 16 16 13 12 12 14)
            (((pure . pure . pure) 0) :: Bonuses (BonusTypes (Temporary Int)))
            (A 5 5 Nothing Nothing "ChainMail")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "CompositeLongBow")
            [(Right . Right $ Weapon Slashing BasicMelee defaultDamageDice 5 [] "Club"), (Right . Right $ Weapon Slashing BasicMelee defaultDamageDice 5 [] "Club")]
            (set (#basicMelee . #weaponfeat) Specialization allproficient)
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            (noReductions)
            (pure (Al Chaotic Evil))
            (pure Human)
            cfeatures
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 2 0 0 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills)
            []
            []
             where
                  cfeatures :: ClassFeatures SpellTemplate Int
                  cfeatures = emptyclassfeature

samuraiagent :: Character
samuraiagent = Character "Samurai Agent" (0,0) (0,0) defaultPicture False False False False (0,0)
      (classupdate $ Status (pure . pure $ 0)
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Enemy)
            (as 16 16 13 8 12 10)
            (((pure . pure . pure) 0) :: Bonuses (BonusTypes (Temporary Int)))
            (A 5 5 Nothing Nothing "ChainMail")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "TwoHandedSword")
            [Right . Right $Weapon Slashing BasicMelee defaultDamageDice 5 [] "CompositeLongBow"]
            (set (#basicMelee . #weaponfeat) Specialization allproficient)
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            (noReductions)
            (pure (Al Chaotic Evil))
            (pure Human)
            cfeatures
            []
            [curewounds 2, invisibility]
            (pure Medium)
            0
            Healthy
            []
            (Classes 3 0 0 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills)
            []
            []
             where
                  cfeatures :: ClassFeatures SpellTemplate Int
                  cfeatures = emptyclassfeature


samuraiagentna :: Character
samuraiagentna = Character "Samurai Agent No armour" (0,0) (0,0) defaultPicture False False False False (0,0)
      (classupdate $ Status (pure . pure $ 0)
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Enemy)
            (as 16 16 13 8 12 10)
            (((pure . pure . pure) 0) :: Bonuses (BonusTypes (Temporary Int)))
            (A 5 5 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "TwoHandedSword")
            [Right . Right $Weapon Slashing BasicMelee defaultDamageDice 5 [] "CompositeLongBow"]
            (set (#basicMelee . #weaponfeat) Specialization allproficient)
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            (noReductions)
            (pure (Al Chaotic Evil))
            (pure Human)
            cfeatures
            []
            [curewounds 2, invisibility]
            (pure Medium)
            0
            Healthy
            []
            (Classes 3 0 0 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills)
            []
            []
             where
                  cfeatures :: ClassFeatures SpellTemplate Int
                  cfeatures = emptyclassfeature

preacher :: Character
preacher = Character "Street Preacher" (0,0) (0,0) defaultPicture False False False False (0,0)
      (classupdate $ Status (pure . pure $ 0)
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Enemy)
            (as 10 13 12 8 15 16)
            (((pure . pure . pure) 0) :: Bonuses (BonusTypes (Temporary Int)))
            (A 5 5 Nothing Nothing "ChainMail")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Club")
            []
            (allproficient)
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            (noReductions)
            (pure (Al Neutral NeutraL))
            (pure Human)
            cfeatures
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 3 0 0 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills)
            []
            []
             where
                  cfeatures :: ClassFeatures SpellTemplate Int
                  cfeatures = over (#mysticspells . #known . #first) (++ [curewounds 1, causefear, doom, causewounds 1]) $
                              over (#mysticspells . #known . #second) (++ [curewounds 2, hold]) emptyclassfeature

ronin :: Character
ronin = Character "Ronin" (0,0) (0,0) defaultPicture False False False False (0,0)
      (classupdate $ Status (pure . pure $ 0)
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Enemy)
            (as 16 15 13 8 12 10)
            (((pure . pure . pure) 0) :: Bonuses (BonusTypes (Temporary Int)))
            (A 5 5 Nothing Nothing "ChainMail")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "TwoHandedSword")
            [Right . Right $Weapon Slashing BasicMelee defaultDamageDice 5 [] "LongBow", Right . Right $Weapon Slashing BasicMelee defaultDamageDice 5 [] "ShortSword"]
            (set (#basicMelee . #weaponfeat) Specialization allproficient)
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            (noReductions)
            (pure (Al Neutral NeutraL))
            (pure Human)
            cfeatures
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 2 0 0 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills)
            []
            []
             where
                  cfeatures :: ClassFeatures SpellTemplate Int
                  cfeatures = emptyclassfeature


roninna :: Character
roninna = Character "Ronin No Armour" (0,0) (0,0) defaultPicture False False False False (0,0)
      (classupdate $ Status (pure . pure $ 0)
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Enemy)
            (as 16 15 13 8 12 10)
            (((pure . pure . pure) 0) :: Bonuses (BonusTypes (Temporary Int)))
            (A 5 5 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "TwoHandedSword")
            [Right . Right $Weapon Slashing BasicMelee defaultDamageDice 5 [] "LongBow", Right . Right $Weapon Slashing BasicMelee defaultDamageDice 5 [] "ShortSword"]
            (set (#basicMelee . #weaponfeat) Specialization allproficient)
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            (noReductions)
            (pure (Al Neutral NeutraL))
            (pure Human)
            cfeatures
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 2 0 0 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills)
            []
            []
             where
                  cfeatures :: ClassFeatures SpellTemplate Int
                  cfeatures = emptyclassfeature

oracle :: Character
oracle = Character "Oracle" (0,0) (0,0) defaultPicture False False False False (0,0)
      (classupdate $ Status (pure . pure $ 0)
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Enemy)
            (as 8 10 12 13 15 16)
            (((pure . pure . pure) 0) :: Bonuses (BonusTypes (Temporary Int)))
            (A 5 5 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "Club")
            []
            (allproficient)
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            (noReductions)
            (pure (Al Neutral NeutraL))
            (pure Human)
            cfeatures
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 3 0 0 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills)
            []
            []
             where
                  cfeatures :: ClassFeatures SpellTemplate Int
                  cfeatures = over (#arcanespells . #known . #first) (++ [hideouslaughter, shield, shockinggrasp, burninghands]) $
                              over (#arcanespells . #known . #second) (++ [scorchingray, hold]) emptyclassfeature

sorcerormonkp :: Character
sorcerormonkp = Character "Yamaoka Akii" (0,0) (0,0) defaultPicture False False False False (0,0)
      (over (#classfeatures . #arcanespells . #perday . #first) (\x -> x - 1) $
      over (#classfeatures . #arcanespells . #perday . #second) (\x -> x - 2) $
      t2ix BearStrength (Present (Just 50) [] Nothing) $
      t2ix MageArmour (Present (Just 50) [] Nothing) $
      t2ix Shield (Present (Just 50) [] Nothing) $
      t2ix StoneSkin (Present (Just 50) [] (Just 30)) $
      classupdate $ Status (over (#hp) (+ 6) (pure . pure $ 0))
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Enemy)
            (as 14 16 12 14 14 17)
            (((pure . pure . pure) 0) :: Bonuses (BonusTypes (Temporary Int)))
            (A 5 5 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "UnArmed")
            []
            (allproficient)
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            (noReductions)
            (pure (Al Neutral NeutraL))
            (pure Human)
            cfeatures
            []
            [shield]
            (pure Medium)
            0
            Healthy
            []
            (Classes 5 0 0 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills)
            []
            []
             where
                  cfeatures :: ClassFeatures SpellTemplate Int
                  cfeatures = over (#arcanespells . #known . #first) (++ [magearmour, hideouslaughter, magicmissile, burninghands, shockinggrasp]) $
                              over (#arcanespells . #known . #second) (++ [bearsstrength, scorchingray]) emptyclassfeature
{-Yamaoka Akii-}
sorcerormonk :: Character
sorcerormonk = Character "Sorceror Monk" (0,0) (0,0) defaultPicture False False False False (0,0)
      (t2ix StoneSkin (Present (Just 50) [] (Just 30)) $
      classupdate $ Status (pure . pure $ 0)
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Enemy)
            (as 14 16 12 14 14 17)
            (((pure . pure . pure) 0) :: Bonuses (BonusTypes (Temporary Int)))
            (A 5 5 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "UnArmed")
            []
            (allproficient)
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            (noReductions)
            (pure (Al Neutral NeutraL))
            (pure Human)
            cfeatures
            []
            [shield]
            (pure Medium)
            0
            Healthy
            []
            (Classes 5 0 0 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills)
            []
            []
             where
                  cfeatures :: ClassFeatures SpellTemplate Int
                  cfeatures = over (#arcanespells . #known . #first) (++ [magearmour, hideouslaughter, magicmissile, burninghands, shockinggrasp]) $
                              over (#arcanespells . #known . #second) (++ [bearsstrength, scorchingray]) emptyclassfeature

orcmonk :: Character
orcmonk = Character "Kyougoku Takashi" (0,0) (0,0) defaultPicture False False False True (0,0)
      (classupdate $ Status (pure . pure $ 0 :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Ally)
            (as 20 16 16 9 18 10)
            (((pure . pure . pure) 0) :: Bonuses (BonusTypes (Temporary Int)))
            (A 5 5 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "UnArmed")
            []
            (set (#basicMelee . #weaponfeat) Specialization allproficient)
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            (noReductions)
            (pure (Al Chaotic Evil))
            (pure Orc)
            cfeatures
            []
            [healpoisen, grenade, divinegrenade]
            (pure Medium)
            0
            Healthy
            []
            (Classes 4 0 0 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#perception) (6 :: Int) $ baseskills))
            []
            []
             where
                  cfeatures :: ClassFeatures SpellTemplate Int
                  cfeatures = emptyclassfeature

warriormonk :: Character
warriormonk = Character "Warrior Monk" (0,0) (0,0) defaultPicture False False False False (0,0)
      (classupdate $ Status (pure . pure $ 0 :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Enemy)
            (as 16 14 12 10 16 8)
            (((pure . pure . pure) 0) :: Bonuses (BonusTypes (Temporary Int)))
            (A 5 5 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "UnArmed")
            []
            (set (#basicMelee . #weaponfocus) True allproficient)
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            (noReductions)
            (pure (Al Neutral NeutraL))
            (pure Orc)
            cfeatures
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 3 0 0 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            (set (#perception) (6 :: Int) $ baseskills))
            []
            []
             where
                  cfeatures :: ClassFeatures SpellTemplate Int
                  cfeatures = emptyclassfeature
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
madge :: Character
madge = Character "Gosanke Ona (Madge)" (0,0) (0,0) defaultPicture False False False False (0,0)
      (classupdate $ Status (pure . pure $ 0 :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Ally)
            (as 8 16 10 14 12 15)
            (((pure . pure . pure) 0) :: Bonuses (BonusTypes (Temporary Int)))
            (A 5 5 Nothing Nothing "Leather")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "ShortBow")
            [Right . Right $Weapon Slashing BasicMelee defaultDamageDice 5 [] "ShortSword"]
            (allproficient)
            (pure None :: SpellFeat SkillFeatProfile)
            0
            0
            0
            normalelementalresistance
            noEffects
            (noReductions)
            (pure (Al Neutral NeutraL))
            (pure Human)
            cfeatures
            []
            []
            (pure Small)
            0
            Healthy
            []
            (Classes 2 0 0 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills)
            []
            []
             where
                  cfeatures :: ClassFeatures SpellTemplate Int
                  cfeatures = set (#feats . #weaponfinesse) True emptyclassfeature

rekkart :: Character 
rekkart = Character "Shijou Harutare (Rekkart)" (0,0) (0,0) defaultPicture False False False False (0,0)
      (classupdate $ Status (set (#hp) (pure 3 :: Temporary Int) (pure . pure $ 0 :: ClassInfo (Temporary Int)))
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Ally)
            (as 13 8 12 12 9 15)
            (((pure . pure . pure) 0) :: Bonuses (BonusTypes (Temporary Int)))
            (A 5 5 Nothing Nothing "ChainMail")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "LongSword")
            []
            (set (#basicMelee . #weaponfocus) True allproficient)
            (pure None :: SpellFeat SkillFeatProfile)
            0
            0
            0
            normalelementalresistance
            noEffects
            (noReductions)
            (pure (Al Neutral NeutraL))
            (pure Human)
            emptyclassfeature
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 3 0 0 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills)
            []
            []

malgrim :: Character 
malgrim = Character "Nishitovin Mansanori (Malgrim)" (0,0) (0,0) defaultPicture False False False False (0,0)
      (classupdate $ Status (pure . pure $ 0 :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Enemy)
            (as 16 16 14 10 11 12)
            (((pure . pure . pure) 0) :: Bonuses (BonusTypes (Temporary Int)))
            (A 5 5 Nothing Nothing "Studded")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "UnArmed")
            [Right . Right $Weapon Slashing BasicMelee defaultDamageDice 5 [] "LongSword"]
            (set (#basicMelee . #weaponfocus) True allproficient)
            (pure None :: SpellFeat SkillFeatProfile)
            0
            0
            0
            normalelementalresistance
            noEffects
            (noReductions)
            (pure (Al Neutral NeutraL))
            (pure Human)
            cfeatures
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 3 0 0 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills)
            []
            []
             where
                  cfeatures :: ClassFeatures SpellTemplate Int
                  cfeatures = emptyclassfeature

killian :: Character 
killian = Character "Nikko Imo (Killian/Sveth)" (0,0) (0,0) defaultPicture False False False False (0,0)
      (classupdate $ Status (pure . pure $ 0 :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Ally)
            (as 10 20 11 13 12 15)
            (((pure . pure . pure) 0) :: Bonuses (BonusTypes (Temporary Int)))
            (A 5 5 Nothing Nothing "Leather")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "ShortSword")
            []
            (allproficient)
            (pure None :: SpellFeat SkillFeatProfile)
            0
            0
            0
            normalelementalresistance
            noEffects
            (noReductions)
            (pure (Al Neutral NeutraL))
            (pure Human)
            cfeatures
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 3 0 0 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills)
            []
            []
             where
                  cfeatures :: ClassFeatures SpellTemplate Int
                  cfeatures = set (#feats . #weaponfinesse) True emptyclassfeature

patrissa :: Character 
patrissa = Character "Kushizu Kuwane (Patrissa)" (0,0) (0,0) defaultPicture False False False False (0,0)
      (classupdate $ Status (pure . pure $ 0 :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Ally)
            (as 8 12 14 10 15 16)
            (((pure . pure . pure) 0) :: Bonuses (BonusTypes (Temporary Int)))
            (A 5 5 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "ShortSword")
            []
            (allproficient)
            (set (#enchantment) Lesser $ pure None :: SpellFeat SkillFeatProfile)
            0
            0
            0
            normalelementalresistance
            noEffects
            (noReductions)
            (pure (Al Neutral NeutraL))
            (pure Human)
            cfeatures
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 2 0 0 0 0 0 0)
            (combatmaneuver :| [])
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills)
            []
            []
             where
                  cfeatures :: ClassFeatures SpellTemplate Int
                  cfeatures = over (#arcanespells . #known . #first) (++ [sleep1]) emptyclassfeature

halgrak :: Character
halgrak = Character "Saitou Tsubo (Halgrak)" (0,0) (0,0) defaultPicture False False False False (0,0)
      (Status (ClassInfo (pure 0) (pure 10) (pure 1) (pure 0) (pure 0) (pure 0) (temporarize 3) (pure 0) (pure 0) :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Ally)
            (as 16 10 13 8 10 11)
            (((pure . pure . pure) 0) :: Bonuses (BonusTypes (Temporary Int)))
            (A 5 5 Nothing Nothing "Natural")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "ShortSword")
            []
            (allproficient)
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            (noReductions)
            (pure (Al Neutral NeutraL))
            (pure Human)
            emptyclassfeature
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
            baseskills)
            []
            []

daimyo :: Character
daimyo = Character "Daimyo Otagi-Sama" (0,0) (0,0) defaultPicture False False False False (0,0)
      (Status (pure . pure $ 0 :: ClassInfo (Temporary Int))
            Nothing
            (pure $ Vision NormalVision 60)
            (pure Ally)
            (as 16 12 14 15 15 14)
            (set (#defense . #attacks . #deflection) (pure 2 :: Temporary Int) (((pure . pure . pure) 0) :: Bonuses (BonusTypes (Temporary Int))))
            (A 5 5 Nothing Nothing "Breastplate")
            (pure Nothing)
            (Weapon Slashing BasicMelee defaultDamageDice 5 [] "TwoHandedSword")
            [Right . Right $Weapon Slashing BasicMelee defaultDamageDice 5 [] "CompositeLongBow"]
            (set (#basicMelee . #weaponfeat) Specialization allproficient)
            (pure None)
            0
            0
            0
            normalelementalresistance
            noEffects
            (dredux (\x _ -> pure x) Pierce 5 noReductions)
            (pure (Al Lawful NeutraL))
            (pure Human)
            cfeatures
            []
            []
            (pure Medium)
            0
            Healthy
            []
            (Classes 5 0 0 0 0 0 0)
            (pure $ combatmaneuver)
            combatmaneuver
            combatmaneuver
            0
            30
            0
            Nothing
            baseskills)
            []
            []
            where 
                cfeatures :: ClassFeatures SpellTemplate Int
                cfeatures = set (#feats . #improvedinitiative) True $ 
                     set (#feats . #improveddisarm) True emptyclassfeature