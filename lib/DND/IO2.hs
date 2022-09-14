{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module DND.IO
where

import Control.Lens (view, set, over)
import Control.Monad.Trans.State.Strict
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Generics.Labels ()
import GHC.Generics ()
import Data.List.Index (indexed, deleteAt)
import System.Random
import Control.Monad (join)
import Data.Char (isDigit)
import Data.Maybe (isJust, fromJust, catMaybes)
import Control.Lens.Indexed (ifoldMap, imap)
import Data.List (delete)


import DND.ATTACKS.Attack (Attack, Result (CriticalMiss))
import DND.ATTACKS.AttackFunctions (attackRolls, attacksRolls, isHit, attackings)
import DND.ATTACKS.AttackFunctions2 (updateattack, longshots, weapon2listattacks, weapon21attacks, w2a', s2a, offhand)
import DND.ATTACKS.DamageReduction (Material (Bronze, Iron, ColdIron, Adamantine, Mithral, Silver))
import DND.ATTACKS.Weapon (WeaponType, Weapons, Weapon (Weapon), Range (Missile), rangefinder)

import DND.CHARACTER.Character (Character (status), detailedview, undoresults, cm2a, sa21a, sa2a, changeweapons, sneakattack, fireshieldcharacter, isMonster)
import DND.CHARACTER.Status (Status, Health (Dead), healthupdate, Team (Enemy), damager, combatmaneuver)

import DND.DAMAGEDICE.DamageDice (DamageDice (D), SaveProfile (Negates))
import DND.DAMAGEDICE.DamageDiceFunctions (mddstatus)
import DND.DAMAGEDICE.Elemental (i2me)

import DND.DEFENSE.Armour (EquipedArmour (EA), ArmourType (Padded, Leather, Studded, ChainShirt, Hide, Scale , ChainMail, Breastplate, Splint, Banded, HalfPlate, FullPlate, LightShield, HeavyShield, TowerShield))
import DND.DEFENSE.DefenseFunctions (i2md)

import DND.SPELLS.SpellFunctions (getspells, spellsperday)
import DND.SPELLS.Spell (SpellTarget (Allies, Enemies, SingleTarget, Caster))

import DND.STATUSEFFECTS.Effect (Effect (Effect))
import DND.STATUSEFFECTS.Effects (StatusEffect (Prone, Stealth, Disarmed, ElementalBodyIII, ElementalBodyIV, Stigmata, Ki, Grappled), on, off, Temporal2 (Present2, Absent2), isOff2, isOn2, selecttemporal2)
import DND.STATUSEFFECTS.StatusEffectFunctions2 (t2ix)
import DND.STATUSEFFECTS.StatusEffectFunctions3 (effectslist, twtgwp)


import DND.Action (Action, runF)
import DND.Roll (Roll, (~>), sumdice, đ, roll, runRoll)
import DND.TemporaryValue (temporary)
import DND.Turn (Game, endturn, modifygamestate, action2gameresult, (!!?), safemodifyat, testgame, initiativefinal, fullcastspell, gamemaker, enemies, passtime3, 
  resteveryone, loot, addtoequipment, removefromequipment, forrestencounters35, underground35, randomizehealthandscores2)

getInt :: IO (Maybe Int)
getInt = do
  line <- getLine
  go line
    where
      go line
        | line == "" = return Nothing
        | foldr (&&) True (isDigit <$> line) = return (Just $ read line)
        | otherwise = return Nothing

getInts :: [Maybe Int] -> IO [Maybe Int]
getInts is = do
  putStrLn "Choose Target"
  first <- getInt
  go (first : is)
    where
      go [] =  return []
      go (int : ints)
        | int == Nothing = do
          return (int : ints)
        | otherwise = do
          getInts (int : ints)

index :: Show a => NonEmpty a -> String
index (c :| cs) = "0: " ++ (show c) ++ "      " ++ (go cs)
  where
    go [] = []
    go (ec : ecs) = show (length (c :| cs) - (length (ec : ecs))) ++ ": " ++ (show ec) ++ "      " ++ (go ecs)

index' :: Show a => NonEmpty a -> IO (String)
index' (c :| cs) = do 
  putStrLn $ "0: " ++ (show c)
  go 1 cs
  where
    go :: Show a => Int -> [a] -> IO String
    go _ [] = return ""
    go int (ec : ecs) = do 
      putStrLn $ show int ++ ": " ++ (show ec)
      go (int + 1) ecs

indexlist :: Show a => [a] -> String
indexlist [] = ""
indexlist (c : cs) = "0: " ++ (show c) ++ "      " ++ (go cs)
  where
    go [] = []
    go (ec : ecs) = show (length (c :| cs) - (length (ec : ecs))) ++ ": " ++ (show ec) ++ "      " ++ (go ecs) 

chooseAttack :: Maybe Int ->  Maybe Int -> Maybe Int -> Maybe (Effect (Temporal2 [Int])) -> Maybe Int -> StdGen -> Game (Status, [Action (Attack Int) Roll])
chooseAttack _ _ _ _ Nothing _ = gets go1
  where
    go1 cs = (((status . NE.head) cs) , [])
chooseAttack distance mi1 mi2 m (Just i) g = gets go 
  where
    go cs
      | i == 1 = (newstatus1, fst $ runState (attacksRolls action1) g)
      | i == 2 = (newstatus2, fst $ runState (attacksRolls action2) g)
      | i == 3 = (newstatus3, fst $ runState (attacksRolls action3) g)
      | i == 4 = (newstatus4, fst $ runState (attackRolls action4) g)
      | i == 5 = (newstatus5, fst $ runState (attackRolls action5) g)
      | i == 6 = (newstatus1, fst $ runState (attacksRolls action1) g)
      | i == 7 = (newstatus2, fst $ runState (attacksRolls action2) g)
      | i == 8 = (newstatus1, fst $ runState (attacksRolls (longshots distance action1)) g)
      | i == 9 = (newstatus2, fst $ runState (attacksRolls (longshots distance action2)) g)
      | i == 10 = (newstatus6, fst $ runState (attackRolls action6) g)
      | i == 11 = (newstatus1, fst $ runState (attacksRolls ((updateattack mi1 mi2) <$> action1)) g)
      | i == 22 = (newstatus2, fst $ runState (attacksRolls ((updateattack mi1 mi2) <$> action2)) g)
      | i == 33 = (newstatus3, fst $ runState (attacksRolls ((updateattack mi1 mi2) <$> action3)) g)
      | i == 44 = (newstatus4, fst $ runState (attackRolls (updateattack mi1 mi2 action4)) g)
      | i == 55 = (newstatus5, fst $ runState (attackRolls (updateattack mi1 Nothing action5)) g)
      | i == 66 = (newstatus1, fst $ runState (attacksRolls ((updateattack mi1 mi2) <$> action1)) g)
      | i == 77 = (newstatus2, fst $ runState (attacksRolls ((updateattack mi1 mi2) <$> action2)) g)
      | i == 88 = (newstatus1, fst $ runState (attacksRolls (longshots distance $ (updateattack mi1 mi2) <$> action1)) g)
      | i == 99 = (newstatus2, fst $ runState (attacksRolls (longshots distance $ (updateattack mi1 mi2) <$> action2)) g)
      | i == 100 = (newstatus6, fst $ runState (attackRolls (updateattack mi1 mi2 action6)) g)
      | otherwise = (newstatus1, fst $ runState (attacksRolls action1) g)
        where
          (newstatus1, action1) = (weapon2listattacks . status . NE.head) cs
          (newstatus2, action2) = (weapon21attacks . status . NE.head) cs
          (newstatus3, action3) =  ((sa2a (#monsterAttack)) . NE.head) cs
          (newstatus4, action4) =  sa21a (NE.head cs) ((NE.head . (view (#status . #monsterAttack)) . NE.head) cs)
          (newstatus5, action5) =  ((cm2a (#cmb) m) . NE.head) cs
          (newstatus6, action6) =  sa21a (NE.head cs) (((view (#status . #specialAttack)) . NE.head) cs)

chooseAttack2 :: Maybe Int ->  Maybe Int -> Maybe Int -> Maybe (Effect (Temporal2 [Int])) -> Maybe Int -> NonEmpty Character -> (Status, [Action (Attack [Int]) [Int]])
chooseAttack2 _ _ _ _ Nothing cs = go1
  where
    go1 = (((status . NE.head) cs) , [])
chooseAttack2 distance mi1 mi2 m (Just i) cs = go 
  where
    go
      | i == 1 = (newstatus1,  action1)
      | i == 2 = (newstatus2,  action2)
      | i == 3 = (newstatus3,  action3)
      | i == 4 = (newstatus4,  [action4])
      | i == 5 = (newstatus5,  [action5])
      | i == 6 = (newstatus1,  action1)
      | i == 7 = (newstatus2,  action2)
      | i == 8 = (newstatus1,  (longshots distance action1))
      | i == 9 = (newstatus2,  (longshots distance action2))
      | i == 10 = (newstatus6,  [action6])
      | i == 11 = (newstatus1,  ((updateattack mi1 mi2) <$> action1))
      | i == 22 = (newstatus2,  ((updateattack mi1 mi2) <$> action2))
      | i == 33 = (newstatus3,  ((updateattack mi1 mi2) <$> action3))
      | i == 44 = (newstatus4,  [(updateattack mi1 mi2 action4)])
      | i == 55 = (newstatus5,  [(updateattack mi1 Nothing action5)])
      | i == 66 = (newstatus1,  ((updateattack mi1 mi2) <$> action1))
      | i == 77 = (newstatus2,  ((updateattack mi1 mi2) <$> action2))
      | i == 88 = (newstatus1,  (longshots distance $ (updateattack mi1 mi2) <$> action1))
      | i == 99 = (newstatus2,  (longshots distance $ (updateattack mi1 mi2) <$> action2))
      | i == 100 = (newstatus6, [(updateattack mi1 mi2 action6)])
      | otherwise = (newstatus1,  action1)
        where
          (newstatus1, action1) = (weapon2listattacks . status . NE.head) cs
          (newstatus2, action2) = (weapon21attacks . status . NE.head) cs
          (newstatus3, action3) =  ((sa2a (#monsterAttack)) . NE.head) cs
          (newstatus4, action4) =  sa21a (NE.head cs) ((NE.head . (view (#status . #monsterAttack)) . NE.head) cs)
          (newstatus5, action5) =  ((cm2a (#cmb) m) . NE.head) cs
          (newstatus6, action6) =  sa21a (NE.head cs) (((view (#status . #specialAttack)) . NE.head) cs)

chooseCombatManeuver :: Maybe Int -> IO (Maybe (Effect (Temporal2 [Int])))
chooseCombatManeuver i
    | ((i == (Just 5)) || (i == (Just 55))) = go
    | otherwise = return Nothing
        where
            go = do
                putStrLn "Choose Combat Maeuver: 1) Trip, 2) Disarm, 3) Grapple, 4) Other"
                int1 <- getInt
                let result = go1 int1
                return result
                    where
                        go1 :: Maybe Int -> (Maybe (Effect (Temporal2 [Int])))
                        go1 (Just 1) = Just $ Effect Prone (Present2 (Just 1) [0]) Nothing Nothing
                        go1 (Just 2) = Just $ Effect Disarmed (Present2 (Just 2) [0]) Nothing Nothing
                        go1 (Just 3) = Just $ Effect Grappled (Present2 (Just 1) [0]) Nothing Nothing
                        go1 _ = Nothing

sneakAttack :: Character -> [Result (Maybe Int)] -> Maybe Int -> IO (Character -> Character)
sneakAttack char results i
  | i /= (Just 6) && (i /= (Just 7)) && (i /= (Just 66)) && (i /= (Just 77)) = return id
  | otherwise = do
    g <- newStdGen
    let sitb = sneakattack results g char
    go
    return sitb
      where
        go
          | foldr (||) False (isHit <$> results) = do
            putStrLn "Sneak Attack!"
          | otherwise = putStrLn "Failure!"

distanceshot :: Maybe Int -> IO (Maybe Int)
distanceshot i
    | (i == (Just 8)) || (i == (Just 9)) || (i == (Just 88)) || (i == (Just 99)) = do
      putStrLn "Enter Distance"
      int <- getInt
      return int
    | otherwise = return Nothing

bonuscheck :: Maybe Int -> IO (Maybe Int, Maybe Int)
bonuscheck i
    | i >= (Just 11) = do
      putStrLn "Attack Bonus"
      int3 <- getInt
      putStrLn "Damage Bonus"
      int4 <- getInt
      return (int3, int4)
    | otherwise = return (Nothing, Nothing)

charToAttack :: Maybe Int -> Character -> [Attack [Int] [Int]]
charToAttack mi c = case mi of
  (Just 3) -> multiplema
  (Just 33) -> multiplema
  (Just 4) -> [s2afunction singlema]
  (Just 44) -> [s2afunction singlema]
  (Just 5) -> [s2afunction cmb]
  (Just 55) -> [s2afunction cmb]
  (Just 10) -> [s2afunction specialattack]
  (Just 100) -> [s2afunction specialattack]
  _ -> weaponattack
  where
      monsterattacks = view (#status . #monsterAttack) c
      multiplema = s2afunction <$> ([NE.head monsterattacks] ++ NE.tail monsterattacks)
      singlema = NE.head monsterattacks
      specialattack = view (#status . #specialAttack) c
      cmb = view (#status . #cmb) c
      s2afunction = snd . s2a True (view (#status) c)
      weaponattack = [s2afunction $ w2a' True (view (#status) c) (view (#status . #primaryhand) c)] ++ (fmap runF . offhand $ view (#status) c)

simpleshow :: (Show a, Show b) => Attack a b -> String
simpleshow a = names ++ ": " ++  (go2 . view (#info . #material) $ a) ++ (go $ view (#info . #attackbonus) a) ++ ", " ++ (go1 $ view (#ddice) a)
  where
    names = (view (#info . #attackname) a)
    go2 x
      | show x /= (takeWhile (/= ' ') names) = show x ++ ", "
      | otherwise = ""
    go ma = case ma of
      Nothing -> ""
      Just y -> show y
    go1 mdd = case mdd of
      Just (D x b c d _) -> show x ++ " " ++ show b ++ " "  ++ go c ++ " "  ++  go d
      Nothing -> ""

simpleshow2 :: Attack Int Int -> String
simpleshow2 a = names ++ ": " ++  (go2 . view (#info . #material) $ a) ++ (go3 $ view (#info . #attackbonus) a) ++ ", " ++ (go1 $ view (#ddice) a)
  where
    names = (view (#info . #attackname) a)
    attackroll = view (#rolls . #tohitroll) a
    go2 x
      | show x /= (takeWhile (/= ' ') names) = show x ++ ", "
      | otherwise = ""
    go3 ma = case ma of
      Nothing -> ""
      Just y -> show $ y + attackroll
    go ma = case ma of
      Nothing -> ""
      Just y -> show y 
    go1 mdd = case mdd of
      Just (D x b c d _) -> show ( x + b ) ++ " "  ++ go c ++ " "  ++  go d
      Nothing -> ""

attacks :: NonEmpty Character -> IO (NonEmpty Character)
attacks a = do
  putStrLn "Choose Attack: 1) F Weapon, 2) S Weapon, 3) Monster Attack, 4) S Monster Attack, 5) Combat Maneuver" 
  putStrLn "6) FW Sneak Attack, 7) SW Sneak Attack, 8) FW Distance Shot, 9) SW Distance Shot, 10) Special Attack"
  int2 <- getInt
  effect <- chooseCombatManeuver int2
  int5 <- distanceshot int2
  bonuses <- bonuscheck int2
  let (int3, int4) = bonuses
  putStrLn ""
  _ <- index' a
  putStrLn ""
  g <- newStdGen
  putStrLn . join . fmap simpleshow . fmap runF . snd $ chooseAttack2 int5 int3 int4 effect int2 a
  putStrLn ""
  putStrLn "Choose Target"
  putStrLn ""
  int1 <- getInt
  putStrLn ""
  g1 <- newStdGen
  let (oldresults, oldcharacters) = runState (go effect int1 int2 g int3 int4 int5) a
  let newstatus = fst $ runState (go2 effect int2 g int3 int4 int5) a
  let newstatus1 = go4 oldresults newstatus
  --let (results, characters) = (takeWhile (\x -> not (x == CriticalMiss)) oldresults, safemodifyat int1 (undoresults oldresults) oldcharacters )
  sitb <- sneakAttack (NE.head a) oldresults int2
  let damage = view (#status . #damage) (NE.head oldcharacters)
  let injury = view (#status . #injury) (NE.head oldcharacters)
  let newstatus2 = set (#damage) damage newstatus1
  let newstatus3 = healthupdate $ set (#injury) injury newstatus2
  let newcharacters = set (#status) newstatus3 (NE.head oldcharacters) :| (NE.tail oldcharacters)
  let (attacker :| defenders) = safemodifyat int1 sitb newcharacters
  let finalcharacters = (go5 g1 int2 int1 attacker :| defenders)
  putStrLn . join . fmap simpleshow2 $ go6 a effect int2 g int3 int4 int5
  putStrLn (show oldresults)
  putStrLn ""
  putStrLn (show $ detailedview (NE.head finalcharacters))
  putStrLn (show $ fmap detailedview (finalcharacters !!? int1))
  putStrLn ""
  (ioEndTurn finalcharacters)
    where
      go :: Maybe (Effect (Temporal2 [Int])) -> Maybe Int -> Maybe Int -> StdGen -> Maybe Int -> Maybe Int -> Maybe Int -> Game [Result (Maybe Int)]
      go test i1 i2 g i3 i4 i5 = do
        blah <- chooseAttack i5 i3 i4 test i2 g
        let attack = snd blah
        (action2gameresult attack i1)
      go2 :: Maybe (Effect (Temporal2  [Int])) -> Maybe Int -> StdGen -> Maybe Int -> Maybe Int -> Maybe Int -> Game Status
      go2 test i2 g i3 i4 i5 = do
        blah <- chooseAttack i5 i3 i4 test i2 g
        let attack = fst blah
        return attack
      go4 :: Eq a => [Result a] -> Status -> Status
      go4 oldresults newstatus
        | CriticalMiss `elem` oldresults = t2ix Prone (Present2 (Just 2) Nothing) newstatus
        | otherwise = newstatus
      go5 :: StdGen -> Maybe Int -> Maybe Int -> Character -> Character
      go5 g2 int2 int1
        | (int2 >= (Just 3)) && (int2 <= (Just 5)) = fireshieldcharacter g2 (a !!? int1)
        | rangefinder (view (#status . #primaryhand . #weapontype) (NE.head a)) /= Missile = fireshieldcharacter g2 (a !!? int1)
        | otherwise = id
      go6 :: NonEmpty Character -> Maybe (Effect (Temporal2 [Int])) -> Maybe Int -> StdGen -> Maybe Int -> Maybe Int -> Maybe Int -> [Attack Int Int]
      go6 as test i2 g i3 i4 i5 = blah
        where
          (_, blah) = fmap (fmap (fmap runRoll)) $ fmap (fmap runF) . fst $ runState (chooseAttack i5 i3 i4 test i2 g) as

aoo :: NonEmpty Character -> IO (NonEmpty Character)
aoo a = do
  _ <- index' a
  putStrLn "Choose Attackers"
  putStrLn ""
  ints1 <- getInts []
  putStrLn ""
  g <- newStdGen
  let attacksofoppurtunity = concat $ fmap (snd . weapon21attacks . (view (#status))) (catMaybes $ fmap (\x -> a !!? x) ints1)
  let rolledattacks = fst $ runState (attacksRolls attacksofoppurtunity) g
  let (results, status) = runState (attackings rolledattacks) (view (#status) (NE.head a))
  putStrLn (show results)
  let (car :| chars) = a
  let newcharacters = (set (#status) status car :| chars)
  ioEndTurn newcharacters

maybeint2int :: Maybe Int -> Int
maybeint2int mi
  | isJust mi = fromJust mi
  | otherwise = 0

putStrNLn :: [String] -> IO [a]
putStrNLn [] = return []
putStrNLn (a : as) = do
  putStrLn a
  putStrNLn as

function :: [Maybe a] -> [Maybe a]
function a = fmap (Just) (catMaybes a)

spelltargets :: Maybe SpellTarget -> NonEmpty Character -> IO [Maybe Int]
spelltargets a b
  | (a == (Just Caster)) || (a == (Just Allies)) || (a == (Just Enemies)) = return []
  | (a == (Just SingleTarget)) = do
    putStrLn "Select Target"
    _ <- index' b
    putStrLn (show a)
    int1 <- getInt
    return [int1]
  | otherwise = do
    _ <- index' b
    putStrLn (show a)
    ints1 <- getInts []
    return ints1

spells :: NonEmpty Character -> IO (NonEmpty Character)
spells a = do
  putStrLn "Choose Spell Type"
  putStrLn "1) Druid, 2) Cleric, 3) Paladin, 4) Ranger, 5) Wizard, 6) Sorceror, 7) Bard, 8) Special Abilitites, 9) Equipment"
  spelltype <- getInt
  putStrLn $ (show . (view (#status . (spellsperday spelltype))) . NE.head) a
  putStrLn "Choose Spell Level"
  spelllevel <- getInt
  _  <- putStrNLn $ ((go <$>) . indexed) (getspells spelltype spelllevel ((view (#status) . NE.head) a))
  putStrLn "Choose Spell"
  chosenspell <- getInt
  targets <- spelltargets (fmap (view (#info . #spelltarget)) $ safedoublexclam (getspells spelltype spelllevel ((view (#status) . NE.head) a)) chosenspell) a
  g <- newStdGen
  {-let spell = over (#status) (\x -> (snd <$> (castspell spelltype spelllevel chosenspell g ((view (#status) . NE.head) a))) x $ x)
  let (caster :| newcharacters) = snd $ runState (modifygamestate spell ~> targets) a
  let makenewcaster = over (#status) (\x -> (fst <$> (castspell spelltype spelllevel chosenspell g (view (#status) caster))) x $ x)-}
  let (caster :| newcharacters) = fullcastspell targets spelltype spelllevel chosenspell g a
  _ <- (go1 . fromJust) ((detailedview <$>) <$> (((caster :| newcharacters) !!?) ~> (function targets)))
  (ioEndTurn (caster :| newcharacters))
    where
      go (a1, b1) = show a1 ++ ": " ++ (show $ view (#info . #name) b1) ++ ", "
      go1 [] = return []
      go1 (a' : a's) = do
        putStrLn $ show a'
        go1 a's

punish :: NonEmpty Character -> IO (NonEmpty Character)
punish a = do
  putStrLn "Choose Elemental"
  putStrLn "1) Fire, 2) Earth, 3) Ice, 4) Wind, 5) Acid, 6) Lightning, 7) Negative, 8) Positive"
  int5 <- getInt
  let elemental = i2me int5
  putStrLn "Choose Defense"
  putStrLn "1) Normal, 2) Touch, 3) Flatfooted, 4) Reflex, 5) Will, 6) Fortitude, 7) CMB, 8) Nothing"
  int6 <- getInt
  putStrLn "Choose Bonus"
  int7 <- getInt
  let defense = i2md int6 int7
  putStrLn ""
  putStrLn "Enter x 'd' y + z dice" 
  int1 <- getInt
  int2 <- getInt
  int3 <- getInt
  putStrLn ""
  _ <- index' a
  putStrLn ""
  ints <- getInts []
  g <- newStdGen
  g' <- newStdGen
  let rollresult = (runRoll . fst) (roll 20 g')
  let damagedice = D ((maybeint2int int1) `đ` (maybeint2int int2)) (maybeint2int int3) elemental defense Negates
  let newdamagedice = (fmap runRoll) . fst $ runState (sumdice ~> damagedice) g
  putStrLn ""
  putStrLn (show rollresult)
  putStrLn ""
  putStrLn (show newdamagedice)
  let characters = snd $ runState (modifygamestate (over (#status) (mddstatus 0 rollresult (Just newdamagedice))) ~> ints) a
  putStrLn ""
  putStrLn (show $ characters)
  putStrLn ""
  (ioEndTurn characters)

harm :: NonEmpty Character -> IO (NonEmpty Character)
harm a = do
  _ <- index' a
  putStrLn "Choose poop"
  putStrLn ""
  int1 <- getInt
  putStrLn "Damage Amount"
  int2 <- getInt
  let characters = safemodifyat int1 (over (#status) healthupdate . over (#status) (damager (#damage) int2)) a
  (ioEndTurn characters)

availableabilities :: Character -> [(StatusEffect, Temporal2 (Maybe Int))]
availableabilities c = ifoldMap go effects
    where
        effects = temporary <$> view (#status . #effects) c
        go :: StatusEffect -> Temporal2 (Maybe Int) -> [(StatusEffect, Temporal2 (Maybe Int))]
        go se t
            | se == Prone = [(se, (Present2 Nothing Nothing))]
            | se == Stealth = [(se, (Present2 Nothing Nothing))]
            | isOff2 t = [(se, on t)]
            | otherwise = []

disavailableabilities :: Character -> [(StatusEffect, Temporal2 (Maybe Int))]
disavailableabilities c = ifoldMap go effects
    where
        effects = temporary <$> view (#status . #effects) c
        go :: StatusEffect -> Temporal2 (Maybe Int) -> [(StatusEffect, Temporal2 (Maybe Int))]
        go se t
            | se == Prone = [(se, (Absent2 Nothing Nothing))]
            | se == Stealth = [(se, (Absent2 Nothing Nothing))]
            | isOn2 t = [(se, off t)]
            | otherwise = []

safedoublexclam :: [a] -> Maybe Int -> Maybe a
safedoublexclam _ Nothing = Nothing
safedoublexclam as (Just i)
    | i < 0 = Nothing
    | otherwise = go as i
      where
        go :: [a] -> Int -> Maybe a
        go (x:_) 0 = Just x
        go (_:ys) j = go ys (j -1)
        go [] _ = Nothing

maybedeleteat :: Maybe Int -> [a] -> [a]
maybedeleteat Nothing as = as
maybedeleteat (Just i) as = deleteAt i as

alleffects :: [String]
alleffects = go <$> (indexed effectslist)
  where
    go (a1, b1) = show a1 ++ ": " ++ (show b1) ++ ", "

thosethatthegodswouldpunish :: NonEmpty Character -> IO (NonEmpty Character)
thosethatthegodswouldpunish a = do
    _ <- putStrNLn alleffects
    int1 <- getInt
    let statuseffect =  safedoublexclam effectslist int1
    putStrLn "Choose Temporal: 0) NA, 1) Absent, 2) Off, 3) On, 4) Present, 5) Permanent"
    int2 <- getInt
    putStrLn "Choose Duration"
    int3 <- getInt
    putStrLn "Choose Modifyer"
    int4 <- getInt
    let madness = twtgwp statuseffect (selecttemporal2 int2 int3 int4)
    _ <- index' a
    putStrLn "Choose Target"
    int5 <- getInt
    let newcharacters = safemodifyat int5 (over (#status) madness) a
    ioEndTurn newcharacters

activateabilities :: NonEmpty Character -> IO (NonEmpty Character)
activateabilities (a :| as) = do
    let abilities1 = availableabilities a
    _ <- putStrNLn (go <$> (indexed $ fst <$> abilities1))
    int1 <- getInt
    let statuseffect = safedoublexclam abilities1 int1
    go1 statuseffect
        where
            go :: (Int, StatusEffect) -> String
            go (a1, Prone) = show a1 ++ ": " ++ "Get Down, "
            go (a1, b1) = show a1 ++ ": " ++ (show b1) ++ ", "
            go1 :: Maybe (StatusEffect, Temporal2 (Maybe Int)) -> IO (NonEmpty Character)
            go1 Nothing = ioEndTurn (a :| as)
            go1 (Just tup)
              | (fst tup == ElementalBodyIII) || (fst tup == ElementalBodyIV) = do
                putStrLn "Choose Elemental: 1) Air, 2) Water, 3) Fire, 4) Earth"
                int <- getInt
                (ioEndTurn $ over (#status) (uncurry t2ix ((fst tup), ((\_ -> int) <$> (snd tup)))) a :| as)
              | fst tup == Stigmata = do
                putStrLn "Choose Bonus: 1) attacks, 2) Damage, 3) AC, 4) Caster Level, 5) Saves"
                int <- getInt
                (ioEndTurn $ over (#status) (uncurry t2ix ((fst tup), ((\_ -> int) <$> (snd tup)))) a :| as)
              | fst tup == Ki = do
                putStrLn "Choose Bonus: 1) Attacks, 2) Defense"
                int <- getInt
                (ioEndTurn $ over (#status) (uncurry t2ix ((fst tup), ((\_ -> int) <$> (snd tup)))) a :| as)
              | otherwise = ioEndTurn $ over (#status) (uncurry t2ix tup) a :| as

disactivateabilities :: NonEmpty Character -> IO (NonEmpty Character)
disactivateabilities (a :| as) = do
    let abilities2 = disavailableabilities a
    _ <- putStrNLn (go <$> (indexed $ fst <$> abilities2))
    int1 <- getInt
    let statuseffect = safedoublexclam abilities2 int1
    go1 statuseffect
        where
            go :: (Int, StatusEffect) -> String
            go (a1, Prone) = show a1 ++ ": " ++ "Get Up, "
            go (a1, b1) = show a1 ++ ": " ++ (show b1) ++ ", "
            go1 :: Maybe (StatusEffect, Temporal2 (Maybe Int)) -> IO (NonEmpty Character)
            go1 Nothing = ioEndTurn (a :| as)
            go1 (Just tup) = ioEndTurn $ over (#status) (uncurry t2ix tup) a :| as

changeequip :: NonEmpty Character -> IO (NonEmpty Character)
changeequip (a :| as) = do
    let equipment' = view (#status . #otherweapons) a
    putStrLn $ join (go1 <$> (indexed equipment'))
    int1 <- getInt
    let selection = safedoublexclam equipment' int1
    let newcharacter = changeweapons selection a
    ioEndTurn (over (#status . #otherweapons) (go selection) newcharacter :| as)
        where
            go :: Maybe (Either EquipedArmour Weapon) -> [Either EquipedArmour Weapon] ->  [Either EquipedArmour Weapon]
            go selection
                | selection == Nothing = id
                | otherwise = delete (fromJust selection)
            go1 :: (Int, Either EquipedArmour Weapon) -> String
            go1 (a1, b) = show a1 ++ ": " ++ (either aname wname b) ++ ", "
            wname weapon
                | en /= [] = (show . head) en ++ " " ++ (show m) ++ " " ++ (show wt) ++ " +" ++ (show x)
                | bonus == Nothing = (show m) ++ " " ++ (show wt)
                | otherwise = (show m) ++ " " ++ (show wt) ++ " +" ++ (show x)
                    where
                      x = maybeint2int bonus
                      (Weapon m wt bonus en) = weapon
            aname armour
                | bonus == 0 = (show m) ++ " " ++ (show at)
                | otherwise = (show m) ++ " " ++ (show at) ++ " +" ++ (show bonus)
                    where
                      (EA at m bonus) = armour

int2action :: Maybe Int -> (NonEmpty Character -> IO (NonEmpty Character))
int2action (Just 0) = punish
int2action (Just 1) = attacks
int2action (Just 2) = activateabilities
int2action (Just 3) = disactivateabilities
int2action (Just 4) = changeequip
int2action (Just 5) = thosethatthegodswouldpunish
int2action (Just 6) = spells
int2action (Just 7) = aoo
int2action (Just 8) = lootspree
int2action (Just 9) = takeitem
int2action _ = attacks

equipment :: Maybe Int -> (NonEmpty Character -> IO (NonEmpty Character))
equipment (Just 0) = changeequip
equipment (Just 1) = lootspree
equipment (Just 2) = takeitem
equipment (Just 3) = deleteitem
equipment (Just 4) = addweapon
equipment (Just 5) = addarmour
equipment _ = changeequip 

chooseEquipment :: NonEmpty Character -> IO (NonEmpty Character)
chooseEquipment a = do
  putStrLn "Choose Action: 0) Change, 1) Loot, 2) Take, 3) Delete, 4) Add Weapon, 5) Add Armour"
  putStrLn ""
  int1 <- getInt
  (equipment int1 a)

abilities :: Maybe Int -> (NonEmpty Character -> IO (NonEmpty Character))
abilities (Just 0) = activateabilities
abilities (Just 1) = disactivateabilities
abilities (Just 2) = thosethatthegodswouldpunish
abilities (Just 3) = punish
abilities (Just 4) = aoo
abilities _ = activateabilities

chooseAbilities :: NonEmpty Character -> IO (NonEmpty Character)
chooseAbilities a = do
  putStrLn "Choose Action: 0) Activate, 1) Deactivate, 2) Inflict, 3) Harm, 4) AOO"
  putStrLn ""
  int1 <- getInt
  (abilities int1 a)

chooseactiontype :: Maybe Int -> (NonEmpty Character -> IO (NonEmpty Character))
chooseactiontype (Just 0) = chooseSimpleAttack
chooseactiontype (Just 1) = attacks
chooseactiontype (Just 2) = spells
chooseactiontype (Just 3) = chooseAbilities
chooseactiontype (Just 4) = chooseEquipment
chooseactiontype (Just 5) = aoo
chooseactiontype (Just 6) = harm
chooseactiontype (Just 7) = ioEndTurn
chooseactiontype _ = chooseSimpleAttack

chooseSimpleAttack :: NonEmpty Character -> IO (NonEmpty Character)
chooseSimpleAttack a = do
  putStrLn "Choose Attack: 0) Full, 1) Single"
  int7 <- getInt
  let int2 = go6 a int7
  effect <- chooseCombatManeuver int2
  int5 <- distanceshot int2
  bonuses <- bonuscheck int2
  let (int3, int4) = bonuses
  putStrLn ""
  _ <- index' a
  putStrLn ""
  putStrLn . join . fmap simpleshow . fmap runF . snd $ chooseAttack2 int5 int3 int4 effect int2 a
  putStrLn ""
  putStrLn "Choose Target"
  putStrLn ""
  int1 <- getInt
  putStrLn ""
  g <- newStdGen
  g1 <- newStdGen
  let (oldresults, oldcharacters) = runState (go effect int1 int2 g int3 int4 int5) a
  let newstatus = fst $ runState (go2 effect int2 g int3 int4 int5) a
  let newstatus1 = go4 oldresults newstatus
  let (results, characters) = (takeWhile (\x -> not (x == CriticalMiss)) oldresults, safemodifyat int1 (undoresults oldresults) oldcharacters )
  sitb <- sneakAttack (NE.head a) results int2
  let damage = view (#status . #damage) (NE.head characters)
  let injury = view (#status . #injury) (NE.head characters)
  let newstatus2 = set (#damage) damage newstatus1
  let newstatus3 = healthupdate $ set (#injury) injury newstatus2
  let newcharacters = set (#status) newstatus3 (NE.head characters) :| (NE.tail characters)
  let (attacker :| defenders) = safemodifyat int1 sitb newcharacters
  let finalcharacters = (go5 g1 int2 int1 attacker :| defenders)
  putStrLn . join . fmap simpleshow2 $ go7 a effect int2 g int3 int4 int5
  putStrLn (go3 results oldresults)
  putStrLn ""
  putStrLn (show $ detailedview (NE.head finalcharacters))
  putStrLn (show $ fmap detailedview (finalcharacters !!? int1))
  putStrLn ""
  (ioEndTurn finalcharacters)
    where
      go :: Maybe (Effect (Temporal2 [Int])) -> Maybe Int -> Maybe Int -> StdGen -> Maybe Int -> Maybe Int -> Maybe Int -> Game [Result (Maybe Int)]
      go test i1 i2 g i3 i4 i5 = do
        blah <- chooseAttack i5 i3 i4 test i2 g
        let attack = snd blah
        (action2gameresult attack i1)
      go2 :: Maybe (Effect (Temporal2  [Int])) -> Maybe Int -> StdGen -> Maybe Int -> Maybe Int -> Maybe Int -> Game Status
      go2 test i2 g i3 i4 i5 = do
        blah <- chooseAttack i5 i3 i4 test i2 g
        let attack = fst blah
        return attack
      go3 :: (Show a, Eq a) => [Result a] -> [Result a] -> String
      go3 results oldresults
        | results == oldresults = show results
        | otherwise = show $ results ++ [CriticalMiss]
      go4 :: Eq a => [Result a] -> Status -> Status
      go4 oldresults newstatus
        | CriticalMiss `elem` oldresults = t2ix Prone (Present2 (Just 2) Nothing) newstatus
        | otherwise = newstatus
      go5 :: StdGen -> Maybe Int -> Maybe Int -> Character -> Character
      go5 g2 int2 int1
        | (int2 >= (Just 3)) && (int2 <= (Just 5)) = fireshieldcharacter g2 (a !!? int1)
        | rangefinder (view (#status . #primaryhand . #weapontype) (NE.head a)) /= Missile = fireshieldcharacter g2 (a !!? int1)
        | otherwise = id
      go6 :: NonEmpty Character -> Maybe Int -> Maybe Int
      go6 cs mi'
        | (isMonster . NE.head) cs && (mi' == (Just 1)) = Just 4
        | (isMonster . NE.head) cs = Just 3
        | mi' == (Just 1) = Just 2
        | otherwise = Just 1
      go7 :: NonEmpty Character -> Maybe (Effect (Temporal2 [Int])) -> Maybe Int -> StdGen -> Maybe Int -> Maybe Int -> Maybe Int -> [Attack Int Int]
      go7 as test i2 g i3 i4 i5 = blah
        where
          (_, blah) = fmap (fmap (fmap runRoll)) $ fmap (fmap runF) . fst $ runState (chooseAttack i5 i3 i4 test i2 g) as



chooseAction :: NonEmpty Character -> IO (NonEmpty Character)
chooseAction a = do
  putStrLn $ show (view (#name) $ NE.head a) ++ "'s Turn"
  putStrLn $ (detailedview . NE.head) a
  putStrLn ""
  putStrLn "Choose Action: 0) Simple Attack, 1) Attack, 2) Spells, 3) Abilities 4) Equipment, 5) A.o.O., 6) Harm, 7) End Turn"
  putStrLn ""
  int1 <- getInt
  chooseactiontype int1 a

lootspree :: NonEmpty Character -> IO (NonEmpty Character)
lootspree (a :| as) = do
    _ <- index' $ a :| as
    putStrLn "Choose Target"
    int1 <- getInt
    let mchar = (a :| as) !!? int1
    let newa = go mchar a
    ioEndTurn (newa :| as)
    where
      go :: Maybe Character -> Character -> Character
      go mchar1 char1 = case mchar1 of
        Nothing -> char1
        Just char2 -> loot char1 char2

mcharToEquip :: Maybe Character -> [Either EquipedArmour Weapon]
mcharToEquip mchar = case mchar of
  Nothing -> []
  Just y -> view (#status . #otherweapons) y

takeitem :: NonEmpty Character -> IO (NonEmpty Character)
takeitem (a :| as) = do
    _ <- index' $ a :| as
    putStrLn "Choose Target"
    int1 <- getInt
    let mchar = (a :| as) !!? int1
    putStrLn . join $ go1 <$> (indexed $ mcharToEquip mchar)
    int2 <- getInt
    let meaw = mcharToEquip mchar `safedoublexclam` int2
    ioEndTurn (safemodifyat int1 (removefromequipment meaw) $ addtoequipment meaw a :| as)
    where
            go1 :: (Int, Either EquipedArmour Weapon) -> String
            go1 (a1, b) = show a1 ++ ": " ++ (either aname wname b) ++ ", "
            wname weapon
                | en /= [] = (show . head) en ++ " " ++ (show m) ++ " " ++ (show wt) ++ " +" ++ (show x)
                | bonus == Nothing = (show m) ++ " " ++ (show wt)
                | otherwise = (show m) ++ " " ++ (show wt) ++ " +" ++ (show x)
                    where
                      x = maybeint2int bonus
                      (Weapon m wt bonus en) = weapon
            aname armour
                | bonus == 0 = (show m) ++ " " ++ (show at)
                | otherwise = (show m) ++ " " ++ (show at) ++ " +" ++ (show bonus)
                    where
                      (EA at m bonus) = armour

deleteitem :: NonEmpty Character -> IO (NonEmpty Character)
deleteitem (a :| as) = do
    putStrLn . join $ go1 <$> (indexed $ mcharToEquip $ Just a)
    int2 <- getInt
    let meaw = mcharToEquip (Just a) `safedoublexclam` int2
    ioEndTurn (removefromequipment meaw a :| as)
    where
            go1 :: (Int, Either EquipedArmour Weapon) -> String
            go1 (a1, b) = show a1 ++ ": " ++ (either aname wname b) ++ ", "
            wname weapon
                | en /= [] = (show . head) en ++ " " ++ (show m) ++ " " ++ (show wt) ++ " +" ++ (show x)
                | bonus == Nothing = (show m) ++ " " ++ (show wt)
                | otherwise = (show m) ++ " " ++ (show wt) ++ " +" ++ (show x)
                    where
                      x = maybeint2int bonus
                      (Weapon m wt bonus en) = weapon
            aname armour
                | bonus == 0 = (show m) ++ " " ++ (show at)
                | otherwise = (show m) ++ " " ++ (show at) ++ " +" ++ (show bonus)
                    where
                      (EA at m bonus) = armour

weaponslist :: [WeaponType]
weaponslist = (foldr (:) [] $ imap (\x _ -> x) (pure () :: Weapons () ))

maybeIntToMat :: Maybe Int -> Material
maybeIntToMat (Just 0) = Bronze
maybeIntToMat (Just 1) = Iron
maybeIntToMat (Just 2) = ColdIron
maybeIntToMat (Just 3) = Silver
maybeIntToMat (Just 4) = Mithral
maybeIntToMat (Just 5) = Adamantine
maybeIntToMat _ = Bronze

weaponmaker :: Maybe WeaponType -> Material -> Maybe Int -> Maybe (Either EquipedArmour Weapon)
weaponmaker mwt mat int = case mwt of
  Nothing -> Nothing
  Just y -> Just . Right $ Weapon mat y int []

addweapon :: NonEmpty Character -> IO (NonEmpty Character)
addweapon (a :| as) = do
  putStrLn . join $ go1 <$> (indexed $ weaponslist)
  int1 <- getInt
  let mwt = weaponslist `safedoublexclam` int1
  putStrLn "0) Bronze, 1) Iron, 2) Cold iron, 3) Silver, 4) Mithral, 5) Admantine"
  int2 <- getInt
  let mat = maybeIntToMat int2
  putStrLn "Choose Enchantment"
  int3 <- getInt
  let meaw = weaponmaker mwt mat int3
  ioEndTurn (addtoequipment meaw a :| as)
    where
            go1 :: (Int, WeaponType) -> String
            go1 (a1, b1) = show a1 ++ ": " ++ (show b1) ++ ", "

armourlist :: [ArmourType]
armourlist = [Padded, Leather, Studded, ChainShirt, Hide, Scale 
       , ChainMail, Breastplate, Splint, Banded, HalfPlate, FullPlate
       , LightShield, HeavyShield, TowerShield]

armourmaker :: Maybe ArmourType -> Material -> Int -> Maybe (Either EquipedArmour Weapon)
armourmaker mart mat int = case mart of
  Nothing -> Nothing
  Just y -> Just . Left $ EA y mat int

addarmour :: NonEmpty Character -> IO (NonEmpty Character)
addarmour (a :| as) = do
  putStrLn . join $ go1 <$> (indexed $ armourlist)  
  int1 <- getInt
  let mart = armourlist `safedoublexclam` int1
  putStrLn "0) Bronze, 1) Iron, 2) Cold iron, 3) Silver, 4) Mithral, 5) Admantine"
  int2 <- getInt
  let mat = maybeIntToMat int2
  putStrLn "Choose Enchantment"
  int3 <- getInt
  let meaw = armourmaker mart mat (maybeint2int int3)
  ioEndTurn (addtoequipment meaw a :| as)
    where
            go1 :: (Int, ArmourType) -> String
            go1 (a1, b1) = show a1 ++ ": " ++ (show b1) ++ ", "  


ioEndTurn :: NonEmpty Character -> IO (NonEmpty Character)
ioEndTurn a = do
  putStrLn "End Turn: 1) Yes, 2) No, 0) End Encounter"
  int1 <- getInt
  putStrLn ""
  go int1
    where
      go int1
        | int1 == (Just 1) = do
          g <- newStdGen
          let newcharacters = snd $ runState (endturn g) a
          (chooseAction newcharacters)
        | int1 == (Just 0) = do
          (endEncounter a)
        | otherwise = do
          (chooseAction a)

endEncounter :: NonEmpty Character -> IO (NonEmpty Character)
endEncounter (a :| as) = do
  putStrLn "Clear Characters: 0) Dead or Enemy, 1) Dead, 2) Enemy, 3) None"
  int1 <- getInt
  putStrLn ""
  case int1 of
    Nothing -> (startprogram $ a :| filter (\x -> Dead /= (view (#status . #health) x) && 
                Enemy /= (temporary . view (#status . #team) $ x)) as)
    (Just 0) -> (startprogram $ a :| filter (\x -> Dead /= (view (#status . #health) x) && 
                Enemy /= (temporary . view (#status . #team) $ x)) as)
    (Just 1) -> (startprogram $ a :| filter (\x -> Dead /= (view (#status . #health) x)) as)
    (Just 2) -> (startprogram $ a :| filter (\x -> Enemy /= (temporary . view (#status . #team) $ x)) as)
    _ -> startprogram (a :| as)

chooseAction' :: NonEmpty Character -> IO (NonEmpty Character)
chooseAction' a = do
  putStrLn $ show (view (#name) $ NE.head a) ++ "'s Turn"
  putStrLn $ (detailedview . NE.head) a
  putStrLn ""
  putStrLn "Choose Action: 0) Punish, 1) Attack, 2) Activate Ability, 3) De-Activate Ability 4) Change Weapons, 5) Inflict Effect, 6) Cast Spell, 7) AOO, 8) Loot, 9) Take Item"
  putStrLn ""
  int1 <- getInt
  (int2action int1 a)

startgame :: NonEmpty Character -> IO (NonEmpty Character)
startgame a = do
  g <- newStdGen
  let newa = initiativefinal g a
  chooseAction newa

chooseteam :: Maybe Int -> NonEmpty Character -> NonEmpty Character
chooseteam mi a
  | mi == (Just 0) = testgame
  | otherwise = a

startprogram :: NonEmpty Character -> IO (NonEmpty Character)
startprogram a = do
  putStrLn "Choose Activity: 0) New Encounter, 1) Continue Encounter, 2) Pass Time, 3) Rest, 4) End Game"
  int1 <- getInt
  let newa = chooseteam int1 a
  (go int1 newa)
    where
      go :: Maybe Int -> NonEmpty Character -> IO (NonEmpty Character)
      go mi cas
        | mi == (Just 4) = return cas
        | mi == (Just 3) = rest cas
        | mi == (Just 2) = timepasser cas
        | mi == (Just 0) = chooseenemies testgame
        | otherwise = chooseenemies cas

timepasser :: NonEmpty Character -> IO (NonEmpty Character)
timepasser a = do
  putStrLn "Enter Time in Minutes"
  int1 <- getInt
  newa <- timepasser' a int1
  startprogram newa

timepasser' :: NonEmpty Character -> Maybe Int -> IO (NonEmpty Character)
timepasser' a int1 = do
  g <- newStdGen
  g1 <- newStdGen
  g2 <- newStdGen
  g3 <- newStdGen
  let newa = snd $ runState (passtime3 g int1) a
  let newa1 = snd $ runState (passtime3 g1 int1) newa
  let newa2 = snd $ runState (passtime3 g2 int1) newa1
  let newa3 = snd $ runState (passtime3 g3 int1) newa2
  startprogram newa3

rest :: NonEmpty Character -> IO (NonEmpty Character)
rest a = do
  putStrLn "Rest 1) Yes, 2) Unsafe, 3) No"
  int1 <- getInt
  case int1 of
    (Just 3) -> startprogram a
    (Just 2) -> monsterest a
    _ -> rest' a

rest' :: NonEmpty Character -> IO (NonEmpty Character)
rest' a = do
  putStrLn "You awake feeling refressed!"
  timepasser' (resteveryone (go2 testgame []) a) (Just 480)
  where
    go2 :: NonEmpty a -> [a] -> NonEmpty a
    go2 (b :| bs) cs = b :| (bs ++ cs)

monsterest :: NonEmpty Character -> IO (NonEmpty Character)
monsterest a = do
  putStrLn "Rest 1) Forrest, 2) Underground, 3) Choose, 4) Cancel"
  int1 <- getInt
  putStrLn "Enter Survival Check"
  int2 <- getInt
  case int1 of
    (Just 1) -> survivalcheck (Just 13) (presetenemies' int1) int2 a 
    (Just 2) -> survivalcheck (Just 17) (presetenemies' int1) int2 a
    (Just 3) -> survivalcheck (Just 13) selectmonsters int2 a
    (Just 4) -> startprogram a
    _ -> survivalcheck (Just 13) (presetenemies' int1) int2 a

survivalcheck :: Maybe Int -> (NonEmpty Character -> IO (NonEmpty Character)) -> Maybe Int -> (NonEmpty Character -> IO (NonEmpty Character))
survivalcheck mi1 f mi2
  | mi1 > mi2 = f
  | otherwise = rest'

chooseenemies :: NonEmpty Character -> IO (NonEmpty Character)
chooseenemies a = do
  putStrLn "Pick Enemies: 0) Pre-Set Encounters, 1) Individual Monsters, 2) None"
  int1 <- getInt
  go int1
  where
    go :: Maybe Int -> IO (NonEmpty Character)
    go mi
      | mi == (Just 2) = chooseAction a
      | mi == (Just 1) = selectmonsters a
      | mi == (Just 0) = presetenemies a
      | otherwise = presetenemies a

selectmonsters :: NonEmpty Character -> IO (NonEmpty Character)
selectmonsters (a :| as) = do
  putStrLn "Select Monster"
  _ <- index' enemies
  int1 <- getInts []
  let monsters = catMaybes $ fmap (\x -> enemies !!? x) int1
  newmonsters <- multiplymonsters monsters
  startgame (a :| (as ++ newmonsters))

multiplymonsters :: [Character] -> IO [Character]
multiplymonsters [] = return []
multiplymonsters (a : as) = do
  putStrLn (show a)
  putStrLn "Choose Number"
  int1 <- getInt
  let newas = gamemaker a (maybeint2int int1)
  putStrLn "Finished: 1) Yes, 2) No"
  int2 <- getInt
  g <- newStdGen
  case int2 of
    (Just 1) -> return . fst $ runState (randomizehealthandscores2 ~> (newas ++ as)) g
    _ -> multiplymonsters (as ++ newas)
 
presetenemies :: NonEmpty Character -> IO (NonEmpty Character)
presetenemies a = do
  putStrLn "0) UnderGround, 1) Forrest"
  int1 <- getInt
  presetenemies' int1 a

presetenemies' :: Maybe Int -> NonEmpty Character -> IO (NonEmpty Character)
presetenemies' int1 (a :| as) = do
  g <- newStdGen
  let (roll1, g2) = roll 35 g
  go g2 int1 roll1
  where
    go gen mint roll' = case mint of 
      Nothing -> do 
        _ <- index' (a :| (as ++ (underground35 (runRoll roll') gen)))
        startgame (a :| (as ++ (underground35 (runRoll roll') gen)))
      Just 1 -> do
        _ <- index' (a :| (as ++ (forrestencounters35 (runRoll roll') gen)))
        startgame (a :| (as ++ (forrestencounters35 (runRoll roll') gen)))
      _ -> do
        _ <- index' (a :| (as ++ (underground35 (runRoll roll') gen)))
        startgame (a :| (as ++ (underground35 (runRoll roll') gen)))
