{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DND.CHARACTER.Status
where

import Control.Monad.Trans.State.Strict (State)
import Data.Generics.Labels ()
import Control.Lens (Lens', set, over, view)
import GHC.Generics (Generic)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Control.Lens.Indexed
import Data.Traversable

import DND.ATTACKS.Attack (Attack (Attack), AttackRoll (AttackRoll), AttackInfo (AInfo))
import DND.ATTACKS.DamageReduction (DamageReduction, Material (Reduc), DamageType (Bludgeoning))
import DND.ATTACKS.Smite (Alignment (Al), Lawfulness (Chaotic, Neutral), Benificence (Good, NeutraL), Race (Human, Construct))
import DND.ATTACKS.Weapon (Weapon (Weapon), Weapons, FeatProfile, Critical (C), Range (Melee), defaultWeapon, allproficient)

import DND.CHARACTER.AbilityScores (AbilityScores, constitution, currentbonuses, Ability, charisma)
import DND.CHARACTER.Bonuses (Bonuses (miscclass), BonusTypes)
import DND.CHARACTER.Class (ClassInfo, Classes, Skills, hp, level, vp)
import DND.CHARACTER.ClassFeatures (ClassFeatures)

import DND.DAMAGEDICE.DamageDice (DamageDice (D), SaveProfile (Half))
import DND.DAMAGEDICE.Elemental (ElementalResistance, Profile, Elemental (Negative, Positive))

import DND.DEFENSE.Armour (Armour, defaultArmour)
import DND.DEFENSE.Defense (Defense (Will, CMD), Target (Target))

import DND.SPELLS.Spell (Spell (Spell), testspell, SkillFeatProfile (None), SpellFeat, emptyspellroll, SpellSchool (Evocation), SpellInfo (SpellInfo), 
  SpellTarget (AllInRange, SingleTarget), SpellArea (Bursty))

import DND.STATDAMAGE.StatDamage (StatDamage (SD))

import DND.STATUSEFFECTS.Effect (Effect (Effect))
import DND.STATUSEFFECTS.Effects (Effects, StatusEffect (Turned, Commanded), Temporal (Absent, Present), addtemporal, viewtempmodifyier, isActive, (&))

import DND.TemporaryValue (Temporary (T), temporary, permanent, revert, addtemporary, modifytemporary)

data Status = Status { primclass :: ClassInfo (Temporary Int)
                     , illumination :: Maybe Vision
                     , vision :: Temporary Vision
                     , team :: Temporary Team
                     , abilityscores :: AbilityScores (Temporary Int)
                     , bonuses :: Bonuses (BonusTypes (Temporary Int))
                     , equipedarmour :: Armour Int
                     , equipment :: Equipment (Maybe (MagicItem Int))
                     , primaryhand :: Weapon
                     , otherweapons :: [Either (MagicItem Int) (Either (Armour Int) Weapon)]
                     , weaponbonus :: Weapons FeatProfile
                     , spellfeat :: SpellFeat SkillFeatProfile
                     , damage :: Temporary Int
                     , injury :: Temporary Int
                     , resolve :: Temporary Int
                     , elementalresistance :: ElementalResistance (Temporary (Profile (Maybe Int)))
                     , effects :: Effects (Temporary (Temporal (Maybe Int)))
                     , damagereduction :: DamageReduction (Temporary (Maybe Int))
                     , alignment :: Temporary Alignment
                     , race :: Temporary Race
                     , classfeatures :: ClassFeatures SpellTemplate Int
                     , specialabilities :: [SpellTemplate]
                     , itemspells :: [SpellTemplate]
                     , tempsize :: Temporary Size
                     , concealment :: Temporary Int
                     , health :: Health
                     , statdamage :: [StatDamage [Int]]
                     , playerclasses :: Classes Int
                     , monsterAttack :: NonEmpty (Attack Int [Int])
                     , specialAttack :: Attack Int [Int]
                     , cmb :: Attack Int [Int]
                     , ap :: Int
                     , movement :: Int
                     , movementtotal :: Int
                     , aura :: Maybe SpellTemplate
                     , skills :: Skills Int}
                     deriving (Generic, Show, Eq, Ord)

data VisionCategory = NormalVision | Low | Dark deriving (Eq, Show, Generic, Ord)

data Vision = Vision {runCategory :: VisionCategory, runDistance :: Int} deriving (Eq, Show, Generic, Ord)

magicitemtest :: MagicItem Int
magicitemtest = MagicItem (set (#miscclass . #hp . #luck) 20 (pure . pure $ 0 :: Bonuses (BonusTypes Int))) [] "Test Item" LeftRing

magicitemtest2 :: MagicItem Int
magicitemtest2 = MagicItem (set (#miscclass . #hp . #luck) 20 (pure . pure $ 0 :: Bonuses (BonusTypes Int))) [] "Test Item 2" RightRing

viewdamage :: Bonuses (BonusTypes a) -> BonusTypes a
viewdamage a = view (#damage) a

viewluck :: (BonusTypes a) -> a
viewluck a = view (#luck) a

viewmagic :: (BonusTypes a) -> a
viewmagic a = view (#magic) a

testbonuses :: Bonuses (BonusTypes (Temporary Int))
testbonuses = over (#damage . #luck) (addtemporary 3) (pure . pure . pure $ 0)

data MagicItem a = MagicItem {bonai :: Bonuses (BonusTypes a)
                    , spells :: [SpellTemplate]
                    , name :: String
                    , area :: EquipmentArea} deriving (Generic, Show, Eq, Ord)

instance Functor MagicItem where
  fmap f (MagicItem b s n a) = MagicItem (fmap (fmap f) b) s n a

instance Applicative MagicItem where
  pure a = MagicItem (pure . pure $ a) [] "Default Name" LeftRing
  (<*>) (MagicItem f fs fn fa) (MagicItem x _ _ _) = MagicItem ((<*>) <$> f <*> x) fs fn fa
 
data EquipmentArea =  Feet | Forearms | LeftRing | RightRing | Amulet | Cloak | Helmet deriving (Generic, Show, Eq, Ord)

data Equipment a = Equipment {forcefield :: Maybe (Int,Int),
                  feet :: a,
                  forearms :: a,
                  leftring :: a,
                  rightring :: a,
                  amulet :: a,
                  cloak :: a,
                  helmet :: a} deriving (Generic, Show, Eq, Ord)

eqix :: EquipmentArea -> Lens' (Equipment a) a
eqix Feet = #feet
eqix Forearms = #forearms
eqix LeftRing = #leftring
eqix RightRing = #rightring
eqix Amulet = #amulet
eqix Cloak = #cloak
eqix Helmet = #helmet

instance FunctorWithIndex EquipmentArea Equipment

instance Foldable Equipment where
    foldMap = foldMapDefault
    length _ = 7

instance FoldableWithIndex EquipmentArea Equipment

instance Traversable Equipment where
    traverse = itraverse . const

instance TraversableWithIndex EquipmentArea Equipment where
    itraverse ƒ (Equipment i a b c d e f g) =
        Equipment i <$> ƒ (Feet) a <*> ƒ (Forearms) b <*> ƒ (LeftRing) c <*> ƒ (RightRing) d <*> ƒ (Amulet) e 
        <*> ƒ (Cloak) f <*> ƒ (Helmet) g

instance Functor Equipment where
  fmap f (Equipment i a b c d e g h) = Equipment i (f a) (f b) (f c) (f d) (f e) (f g) (f h)

instance Applicative Equipment where
  pure a = Equipment Nothing a a a a a a a
  (<*>) (Equipment i fa fb fc fd fe ff fg) (Equipment _ a b c d e f g) = Equipment i (fa a) (fb b) (fc c) (fd d) (fe e) (ff f) (fg g)

isRing :: MagicItem a -> Bool
isRing (MagicItem _ _ _ LeftRing) = True
isRing (MagicItem _ _ _ RightRing) = True
isRing _ = False

baseskills :: Skills Int
baseskills = pure 0

emptystatus :: Status
emptystatus = Status (pure $ pure 0) Nothing (pure $ Vision NormalVision 60) (pure Ally) (pure $ pure 0) ((pure . pure . pure) 0) defaultArmour (pure Nothing) defaultWeapon [] allproficient (pure None)
  (pure 0) (pure 0) (pure 0) ((pure . pure . pure) Nothing) ((pure . pure . pure) Nothing) ((pure . pure) Nothing) (pure $ (Al Chaotic Good)) (pure Human) 
  (pure 0) [] [] (pure Medium) (pure 0) Healthy [] (pure 0) (pure combatmaneuver) combatmaneuver combatmaneuver 0 0 0 Nothing (pure 0)


combatmaneuver :: Attack Int [Int]
combatmaneuver = Attack (AInfo "Combat Maneuver" Melee 5 Bludgeoning Reduc (Just 0) CMD (C 21 0 0) Nothing (Al Neutral NeutraL) False (False,False)) 
                    emptyAttack (Just $ pure [0]) [] Nothing Nothing Nothing []

emptyAttack :: AttackRoll [Int]
emptyAttack = AttackRoll [20] [20] [100] [100] [100] [20] [20] [20]

emptyclassfeature :: ClassFeatures SpellTemplate Int
emptyclassfeature = pure 0

detailedstatus :: Status -> String
detailedstatus s = (show $ currenthitpoints s)

data Size = Diminutive | Tiny | Small | Medium | Large | Huge | Gargantuan | Collosal deriving (Generic, Show, Eq, Ord, Enum)

data Team = Ally | Enemy | Enemy2 | Bystander deriving (Generic, Show, Eq, Ord)

data Health
  = Dead | Stabelized | Dieing | Danger | Wounded | Hurt | Blooded
  | Grazed | Healthy deriving (Generic, Show, Eq, Ord)

testsd :: StatDamage [Int]                  
testsd = SD Nothing Nothing Nothing [4] [4] 0 [4] 1

type StatusUpdate = State Status

type SpellTemplate = (Maybe Int -> Maybe Ability -> Status -> Spell [Int])

testspelltemplate :: SpellTemplate
testspelltemplate _ _ _ = testspell

instance Show SpellTemplate where
  show f = view (#info . #name) (f Nothing Nothing emptystatus)

spellname :: SpellTemplate -> String
spellname st = view (#info . #name) $ st Nothing Nothing emptystatus

instance Eq SpellTemplate where
  (==) a b = (a Nothing Nothing emptystatus) == (b Nothing Nothing emptystatus)

instance Ord SpellTemplate where
  (<=) a b = (a Nothing Nothing emptystatus) <= (b Nothing Nothing emptystatus)
  compare a b = compare (a Nothing Nothing emptystatus) (b Nothing Nothing emptystatus)

instance Eq (Status -> Status) where
  (==) a b = (a emptystatus) == (b emptystatus)

size :: Status -> Size
size s = NE.head $ view (#temporaries) (tempsize s)

washealed :: NonEmpty Int -> Bool
washealed (0 :| []) = True
washealed (_ :| []) = False
washealed (a :| as)
  | a < (head as) = True
  | otherwise = False

healamount :: NonEmpty Int -> (Int, NonEmpty Int)
healamount ne@(0 :| []) = (0, ne)
healamount ne@(x :| []) = (0, ne)
healamount ne@(a :| as) = ((head as) - a, head as :| tail as)

healthupdate1 :: Status -> Status
healthupdate1 p
    | curvp <= 0 && (fst $ resolvepoints p) <= 0 = set (#health) Dead p
    | (any (\x -> 0 > x) . currentbonuses . abilityscores $ p) = set (#health) Dead p
    | view (#health) p == Dieing && (washealed $ view (#damage . #temporaries) p) && curvp <= 0 =  over (#injury) (modifytemporary (\x -> x + curvp)) . set (#health) Stabelized $ p
    | curvp < 0 && view (#health) p == Dieing = over (#resolve) (modifytemporary (\x -> x + 1)) . over (#injury) (modifytemporary (\x -> x + curvp)) $ p
    | curvp < 0 = over (#injury) (modifytemporary (\x -> x + curvp)) . set (#health) Dieing $ p
    | (curvp == 0) && view (#health) p == Stabelized = p
    | (curvp == 0) = set (#health) Dieing p
    | absd = set (#health) Danger p
    | curvp <= (maxvp `div` 8) = set (#health) Danger p
    | curvp <= (maxvp `div` 4) = set (#health) Wounded p
    | curvp <= (maxvp `div` 2) = set (#health) Hurt p
    | curvp <= (3 * maxvp `div` 4) = set (#health) Blooded p
    | curvp <= (9 * maxvp `div` 10) = set (#health) Grazed p
    | otherwise = set (#health) Healthy p
        where
          HP (T (_ :| []) maxhp) (T (curvp :| []) maxvp) = currenthitpoints p
          absd = scoredanger . abilityscores $ p

healthupdate2 :: Status -> Status
healthupdate2 s = set (#damage . #temporaries) (hpchange + (temporary . view (#damage)) s :| []) $ damager' (#injury) (Just vpchange) s
  where
    currenthp = currenthitpoints s
    (healtotal, currentdamage) = healamount $ view (#damage . #temporaries) s 
    permanenthp = view (#hitpoints . #permanent) currenthp
    permanentvp = view (#vitalitypoints . #permanent) currenthp
    currentinjury = temporary $ view (#injury) s
    (hpchange, vpchange) = if (washealed $ view (#damage . #temporaries) s) && healtotal > 0 then go1 else go2
    go1
      | (healtotal - currentinjury) > (NE.head currentdamage) = (negate $ (temporary . view (#damage)) s, negate currentinjury)
      | healtotal > currentinjury = (currentinjury, negate currentinjury)
      | otherwise = (healtotal, negate healtotal)
    go2
      | permanenthp < (temporary . view (#damage) $ s) = (permanenthp - (temporary . view (#damage) $ s), (temporary . view (#damage) $ s) - permanenthp)
      | otherwise = (0,0)

healthupdate :: Status -> Status
healthupdate = healthupdate2 . healthupdate1

data Hitpoints = HP { hitpoints :: Temporary Int
                    , vitalitypoints :: Temporary Int}
                    deriving (Generic, Eq, Ord)

instance Show Hitpoints where
  show (HP (T (a :| _) b) (T (c :| _) d)) = show a ++ "/" ++ show b ++ " " ++ show c ++ "/" ++ show d

gettotalcurrent' :: Hitpoints -> Int
gettotalcurrent' (HP (T (a :| _) _) (T (b :| _) _)) = a + b

gettotalcurrent :: Status -> Int
gettotalcurrent s = gettotalcurrent' $ currenthitpoints s

currenthitpoints :: Status -> Hitpoints
currenthitpoints s = HP (T ((fst go1) :| []) stamina) (T (go2 :| []) vitality)
    where
        vitality = ((temporary . sum . vp . miscclass . gatherbonuses) s) + ((temporary . vp . primclass) s)
        stamina 
          | (isActive . temporary . view (#effects . #undead) $ s) = ((temporary . sum . hp . miscclass . gatherbonuses) s) + ((temporary . hp . primclass) s) + (((charisma . abilitybonuses) s) * 
          (((permanent . level . primclass) s) - ((temporary . level . primclass) s)))
          | ((Construct ==) . temporary . view (#race) $ s) = ((temporary . sum . hp . miscclass . gatherbonuses) s) + ((temporary . hp . primclass) s) 
          | otherwise = ((temporary . sum . hp . miscclass . gatherbonuses) s) + ((temporary . hp . primclass) s) + (((constitution . abilitybonuses) s) * 
          (((permanent . level . primclass) s) - ((temporary . level . primclass) s)))
        go1
          | (temporary . damage) s >= stamina = (0, ((temporary . damage) s) - stamina)
          | otherwise = (stamina - ((temporary . damage) s), 0)
        go2 = vitality - (snd go1) - ((temporary . injury) s)

gatherbonuses :: Status -> Bonuses (BonusTypes (Temporary Int))
gatherbonuses s = foldl function69 (bonuses s) (equipment s)

function69 :: Bonuses (BonusTypes (Temporary Int)) -> Maybe (MagicItem Int) -> Bonuses (BonusTypes (Temporary Int))
function69 bs mmi = case mmi of
  Nothing -> bs
  Just y -> ((<*>) . fmap addtemporary) <$> (bonai y) <*> bs

abilitybonuses' :: Status -> AbilityScores Int
abilitybonuses' s = fmap ((\x -> x `div ` 2) . (\x -> x - 10)) $ (+) <$> scores <*> bonusess
  where
    scores = (currentbonuses . abilityscores) s
    bonusess = (temporary . sum) <$> (view (#abilityscores) . gatherbonuses $ s)

maxDexterity :: Armour Int -> Maybe Int
maxDexterity = view (#maxDex)

limitdexterity :: Status -> Int
limitdexterity s = case (maxDexterity . equipedarmour) s of
    Nothing -> ((view (#dexterity) . abilitybonuses') s)
    Just y -> min ((view (#dexterity) . abilitybonuses') s) ((temporary . sum . view (#maxdex) . gatherbonuses) s + y)

abilitybonuses :: Status -> AbilityScores Int
abilitybonuses s = set (#dexterity) (limitdexterity s) $ abilitybonuses' s

scoredanger :: AbilityScores (Temporary Int) -> Bool
scoredanger asc = any id islowanddamaged
  where
  temporaries = fmap temporary asc
  isdamaged = fmap (\x -> x > 0) temporaries
  currentscore = currentbonuses asc
  islow = fmap (\x -> x < 7) currentscore
  islowanddamaged = (&&) <$> islow <*> isdamaged

damager' :: (Lens' Status (Temporary Int)) -> Maybe Int -> Status -> Status
damager' _ Nothing s = s
damager' lens (Just a) s
    | NE.head (view (lens . #temporaries) s) + a <= 0 = set (lens . #temporaries) ( 0 :| []) s
    | otherwise = over (lens . #temporaries) (\ (x :| xs) -> a + x :| (x : xs)) s

damager :: Maybe Int -> Status -> Status
damager Nothing s = s
damager (Just a) s = over (#damage . #temporaries) (\ (x :| xs) -> a + x :| (x : xs)) s

resolvepoints :: Status -> (Int, Int)
resolvepoints s = (rptotal - rpdamage, rptotal)
  where
    rpbase = (sum $ view (#playerclasses) s) `div` 2
    abilitymod = maximum $ abilitybonuses s
    rptotal = max 1 $ abilitymod + rpbase
    rpdamage = temporary $ view (#resolve) s

penalty :: (Lens' Status (Temporary Int)) -> Int -> Status -> Status
penalty lens a s = over (lens . #temporaries) (\ (x :| xs) -> (x - a) :| (x : xs)) s

spellresistance :: Status -> Int
spellresistance s = (temporary . sum) . view (#spellresistance) . gatherbonuses $ s

initiative :: Status -> Int
initiative s = ((temporary . sum) . view (#initiative) . gatherbonuses $ s) + (view (#dexterity) (abilitybonuses s)) + go
    where
      go
        | view (#classfeatures . #feats . #improvedinitiative) s = 4
        | otherwise = 0

channelenergy :: SpellTemplate
channelenergy _ _ s = Spell (SpellInfo ("Channel " ++ (show energytype) ++ " Energy") 2 Evocation difficulty 0 Nothing (AllInRange Bursty 30)) emptyspellroll (False,False)
                            (Just (D damagedice 0 (Just energytype) (Just (Target Will 0)) Half)) Nothing Nothing Nothing Nothing True
    where
        spelllevel = max (effectivespelllevel (Just 2) s) (effectivespelllevel (Just 3) s)
        damagedice = take ((spelllevel + 1) `div` 2) $ cycle [6]
        difficulty = ((spelllevel + 1) `div` 2) + (view (#charisma) $ abilitybonuses s)
        energytype = Positive

channelenergy' :: SpellTemplate
channelenergy' _ _ s = Spell (SpellInfo ("Channel 'Positive' Energy") 2 Evocation difficulty 0 Nothing (AllInRange Bursty 30)) emptyspellroll (False,False)
                            (Just (D damagedice 0 (Just Positive) (Just (Target Will 0)) Half)) Nothing Nothing Nothing Nothing True
    where
        spelllevel = max (effectivespelllevel (Just 2) s) (effectivespelllevel (Just 3) s)
        damagedice = take ((spelllevel + 1) `div` 2) $ cycle [6]
        difficulty = ((spelllevel + 1) `div` 2) + (view (#wisdom) $ abilitybonuses s)

turnundead :: SpellTemplate
turnundead _ _ s = Spell (SpellInfo name 2 Evocation difficulty 0 Nothing (AllInRange Bursty 30)) emptyspellroll (False,False)
                            Nothing Nothing (Just $ Effect statuseffect (Present duration [] [0]) go2 (Just $ Target Will 0)) Nothing Nothing True
    where
        spelllevel = max (effectivespelllevel (Just 2) s) (effectivespelllevel (Just 3) s)
        difficulty = ((spelllevel + 1) `div` 2) + (view (#charisma) $ abilitybonuses s)
        energytype = Positive
        name
          | energytype == Negative = "Command Undead"
          | otherwise = "Turn Undead"
        statuseffect
          | energytype == Negative = Commanded
          | otherwise = Turned
        duration
          | energytype == Negative = Nothing
          | otherwise = Just (10 * spelllevel)
        go2
          | energytype == Positive = Nothing
          | temporary (view (#team) s) == Ally = Just 1
          | temporary (view (#team) s) == Enemy = Just 2
          | temporary (view (#team) s) == Enemy2 = Just 3
          | temporary (view (#team) s) == Bystander = Just 4
          | otherwise = Just 4

layonhands :: SpellTemplate
layonhands _ _ s = Spell (SpellInfo ("Lay on Hands") 2 Evocation 0 0 Nothing (SingleTarget 9)) emptyspellroll (False,False)
                            (Just (D damagedice bonus (Just Positive) Nothing Half)) Nothing Nothing Nothing Nothing True
    where
        spelllevel = sum $ view (#playerclasses) s
        damagedice
            | spelllevel >= 20 = [0]
            | otherwise = take (spelllevel `div` 2) $ cycle [6]
        bonus
            | spelllevel >= 20 = spelllevel * 3
            | otherwise = 0

effectivespelllevel :: Maybe Int -> Status -> Int
effectivespelllevel Nothing _ = 0
effectivespelllevel mi s 
    | mi == (Just 1) = view (#playerclasses . #myst) s
    | mi == (Just 2) = view (#playerclasses . #techno) s
    |otherwise = 0