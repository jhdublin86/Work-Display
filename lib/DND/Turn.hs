{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}

module DND.Turn
where

import Control.Lens (view, set, over)
import Control.Lens.Indexed (imap)
import Control.Monad.Trans.State.Strict
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Generics.Labels ()
import GHC.Generics (Generic)
import Data.List (find, delete)
import System.Random (StdGen)
import Control.Monad (join)
import Data.Maybe (fromJust, catMaybes)
import Control.Applicative (liftA2)
import Data.Foldable (minimumBy)

import DND.ATTACKS.Attack (Attack, Result)
import DND.ATTACKS.AttackFunctions (attackings, singleeffectAttack, isHit)
import DND.ATTACKS.Weapon (Weapon (Weapon), Range (Missile), rangefinder)

import DND.CHARACTER.AbilityScores (AbilityScores)
import DND.CHARACTER.Character
import DND.CHARACTER.ClassUpdate2 ()
import DND.CHARACTER.Sleep (sleep)
import DND.CHARACTER.Status (Status, layonhands, channelenergy, healthupdate, damager, damager', initiative, currenthitpoints, 
                            turnundead, channelenergy', Team (Ally), MagicItem, VisionCategory (..), SpellTemplate, Vision (Vision), 
                            Size (..), Health (..), resolvepoints)

import DND.DAMAGEDICE.DamageDice (DamageDice (D))

import DND.DEFENSE.Armour (Armour )
import DND.DEFENSE.Defense (Target (Target), Defense (Defense))
import DND.DEFENSE.DefenseFunctions (ffdefensetype,defense2defensetype)

import DND.SPELLS.Spell (isMapArea, getAreaRadius, testspell, isAllBurst, isAlliesBurst, isEnemiesBurst, getRange, rollspelldice, 
                          Summon (Summon), SpellTarget (Caster, Enemies, Allies, SingleTarget), rollingspelldice, Spell, isAllies, isEnemies, 
                          isSingleTarget)
import DND.SPELLS.SpellFunctions (w2sp, mstring2s2s, castspell2, castAreaspell, castspell, getspells, (!?), msix, msix', function2, applyspell)
import DND.SPELLS.Summons (creaturefinder, ghoul, zombie, muskcreep, giantspider, assassinvine, giantmantis, unicorn, direbat, boar, direboar, 
  direwolf, goblin, wolf, direrat, griffon, guard, skeletonchampion, batswarm, carrionstorm, giantleach, choaker, ghoulstirge, shadow, croaker, nixie, 
  bombadierbeetle, woundedjaguar, moltengolem, lurcher, woodgolem, giantfrog, ghoulpriest, pirate, constrictorsnake, wyrmlingforrestdragon, octopus, 
  allosaurus, younggreendragon, troll, bulette, centipedeswarm, goldenskeleton, palestranger, cannongolem, iceDevil, ysokiAlchemist, hGustweld)

import DND.STATDAMAGE.StatDamage (StatDamage (SD))
import DND.STATDAMAGE.StatDamageFunctions (storedpoisens, reducedurations, rolldurations)

import DND.STATUSEFFECTS.Effect (Effect (Effect))
import DND.STATUSEFFECTS.Effects (Effects, (-$-), isActive, StatusEffect (Sing, Despair, Summoned, Invisible, ImprovedInvisibility, Fatigued, 
  Exhausted, Grappled, Ki, Prone, DarkHeal, FlatFooted), Temporal (Present, Permanent, Absent, Off), isOn, viewtempmodifyier, temporalduration, (&), effix, 
  removerepeatdurations, removeemptydurations, isOff)
import DND.STATUSEFFECTS.StatusEffectFunctions2 (t2ix)
import DND.STATUSEFFECTS.StatusEffectFunctions3 (reducet2durations)

import DND.Action (Action (runF))
import DND.Roll (Roll (Roll), Dice, runRoll, roll, sumdice, (~>), đ)
import DND.TemporaryValue (Temporary (T), temporary, modifytemporary)
import DND.Maps (Portal (..), Maze, finalvisionfield, visionfield', linealgo, eucliddistance)
import DND.Time
import Graphics.Gloss

type GameState = NonEmpty Character

type Game = State GameState

data World = World {runCharacters :: GameState
                   , runOldMouseLocation :: (Point, Point, Bool)
                   , runGlobalOffSet :: Point
                   , runGlobalZoom :: Float
                   , runGameMode :: GameMode
                   , runTarget :: [Maybe Int]
                   , runTime :: Time Int
                   , runGameMenu :: GameMenu
                   , runGameText :: [String]
                   , inCombat :: Bool
                   , backGround :: Picture
                   , backGroundEx :: Picture
                   , explore :: [Location]
                   , runMaze :: Maze
                   , runLocations :: [Location]
                   , runBrightness :: Maybe VisionCategory
                   , runMonsterRest :: Maybe Int
                   , runWorldNumber :: Maybe Int} deriving (Generic)

data GameMode = DMMode {runPortal :: Either Portal Vision} | StartCombat | Combat | Map | TargetMode | CastSpell {runIsCrit :: Bool} | CombatTargetMode | SpellTargetMode | SingleSpellTargetMode | ToggleMode | AttackChoice | EquipmentTarget | EquipmentTargeting {runMaybeLocation :: Maybe Location} deriving (Generic, Eq, Show)

isToggleMode :: GameMode -> Bool
isToggleMode ToggleMode = True
isToggleMode _ = False

isDMMode :: GameMode -> Bool
isDMMode (DMMode _) = True
isDMMode _ = False

getPortal :: GameMode -> Either Portal Vision
getPortal gm = case gm of
  DMMode x -> x
  _ -> Left Hall

showPortal :: Either Portal Vision -> String
showPortal port = case port of
  Left (DoorClosed _ _ Nothing) -> "Door Closed"
  Left (DoorClosed _ _ (Just y)) -> "Door Locked " ++ show y
  Left z -> show z
  Right (Vision _ y) -> "Light " ++ show y

advancePortal :: GameMode -> GameMode
advancePortal gm = case gm of
  DMMode (Left Hall) -> DMMode (Left $ Wall Nothing Nothing)
  DMMode (Left (Wall _ _)) -> DMMode (Left $ DoorClosed Nothing Nothing Nothing)
  DMMode (Left (DoorClosed _ _ _)) -> DMMode (Right $ Vision NormalVision 5)
  DMMode (Right (Vision NormalVision 5)) -> DMMode (Left Hall)
  x -> x

advanceDC :: GameMode -> GameMode
advanceDC gm = case gm of
  DMMode (Left (DoorClosed x y Nothing)) -> DMMode (Left $ DoorClosed x y $ Just 15)
  DMMode (Left (DoorClosed x y z)) -> DMMode (Left . (DoorClosed x y) $ (+) <$> (Just 1) <*> z)
  DMMode (Right (Vision light x)) -> DMMode (Right $ Vision light (x + 5))
  x -> x

regressDC :: GameMode -> GameMode
regressDC gm = case gm of
  DMMode (Left (DoorClosed x y Nothing)) -> DMMode (Left . DoorClosed x y $ Just 10)
  DMMode (Left (DoorClosed x y z)) -> DMMode (Left . (DoorClosed x y) $ (+) <$> (Just (-1)) <*> z)
  DMMode (Right (Vision light x)) -> DMMode (Right $ Vision light (x - 5))
  x -> x

isTargetMode :: GameMode -> Bool
isTargetMode a = case a of
  TargetMode -> True
  CombatTargetMode -> True
  SpellTargetMode -> True
  SingleSpellTargetMode -> True
  EquipmentTarget -> True
  _ -> False

isTargetMode' :: GameMode -> Bool
isTargetMode' a = case a of
  TargetMode -> True
  _ -> False

isEquipmentTarget :: GameMode -> Bool
isEquipmentTarget a = case a of
  EquipmentTarget -> True
  _ -> False
  
getEquipmentTarget :: GameMode -> Maybe Location
getEquipmentTarget a = case a of
  EquipmentTargeting a -> a
  _ -> Nothing

data GameMenu = SkillMenu {runMenuText :: [String]} |  SkillMenu2 {runMenuText :: [String]} |  ChangePresetEncounterMenu {runMenuText :: [String]} | ChangeLevelMenu {runMenuText :: [String]} | TakeEquipmentMenu {runMenuText :: [String]} | EquipmentMenu {runMenuText :: [String]} | MainMenu {runMenuText :: [String]} | ChooseAttackMenu {runMenuText :: [String], runMenuChoice :: Maybe Int}| ChooseEnemiesMenu {runMenuText :: [String]}| ToggleAbilitiesMenu {runMenuText :: [String]}| ActionMenu {runMenuText :: [String]}| EndTurnMenu {runMenuText :: [String]}| SpellsMenu {runClass :: Maybe Int, runLevel :: Maybe Int, runSpell :: Maybe Int, runMenuText :: [String]} deriving (Generic)

isCPEMenu :: GameMenu -> Bool
isCPEMenu (ChangePresetEncounterMenu _) = True
isCPEMenu _ = False

isCEMenu :: GameMenu -> Bool
isCEMenu (ChooseEnemiesMenu _) = True
isCEMenu _ = False

isSkMenu :: GameMenu -> Bool
isSkMenu (SkillMenu _) = True
isSkMenu _ = False

isSk2Menu :: GameMenu -> Bool
isSk2Menu (SkillMenu2 _) = True
isSk2Menu _ = False

isCLMenu :: GameMenu -> Bool
isCLMenu (ChangeLevelMenu _) = True
isCLMenu _ = False

isCAMenu :: GameMenu -> Bool
isCAMenu (ChooseAttackMenu _ _) = True
isCAMenu _ = False

isSpellsMenu :: GameMenu -> Bool
isSpellsMenu (SpellsMenu _ _ _ _) = True
isSpellsMenu _ = False

isEndTurnMenu :: GameMenu -> Bool
isEndTurnMenu (EndTurnMenu _) = True
isEndTurnMenu _ = False

isMainMenu :: GameMenu -> Bool
isMainMenu (MainMenu _ ) = True
isMainMenu _ = False

isActionMenu :: GameMenu -> Bool
isActionMenu (ActionMenu _) = True
isActionMenu _ = False

isToggleMenu :: GameMenu -> Bool
isToggleMenu (ToggleAbilitiesMenu _) = True
isToggleMenu _ = False

isEquipmentMenu :: GameMenu -> Bool
isEquipmentMenu (EquipmentMenu _) = True
isEquipmentMenu _ = False

isTakeEquipmentMenu :: GameMenu -> Bool
isTakeEquipmentMenu (TakeEquipmentMenu _) = True
isTakeEquipmentMenu _ = False

msummon2character :: StdGen -> Character -> Maybe Summon -> [Character]
msummon2character _ _ Nothing = []
msummon2character g car (Just (Summon name duration number)) = gamemaker (Character (view (#name) car ++ "'s " ++ name) location location defaultPicture False False (view (#inCombat) car) (view (#pC) car) (0,0) 
  (t2ix Summoned (Permanent ((Just duration) & (Just 1)) (view (#name) car) Nothing) $ 
  set (#team) (view (#status . #team) car) (creaturefinder name)) [] []) rollresult
  where
    location = view (#runLocation) car
    rollresult = runRoll . fst $ roll number g


removespell :: Maybe Int -> Maybe Int -> Maybe Int -> Status -> Status
removespell spelltype spelllevel spellchoice spellcaster = newspellcaster
    where
      newspellcaster
        | spelltype == (Just 9) = spellcaster
        | spellchoice !? (view (msix spelltype spelllevel) spellcaster) == Nothing = spellcaster
        | ((spellchoice !? (view (msix spelltype spelllevel) spellcaster)) == (Just channelenergy')) &&
          ((\x -> x <= 1) . length) (filter (\x -> x == channelenergy) (view (msix spelltype spelllevel) spellcaster)) = 
          over (msix spelltype spelllevel) ((delete channelenergy) . (delete channelenergy')) spellcaster
        | ((spellchoice !? (view (msix spelltype spelllevel) spellcaster)) == (Just channelenergy')) &&
          ((\x -> x > 1) . length) (filter (\x -> x == channelenergy) (view (msix spelltype spelllevel) spellcaster)) = 
          over (msix spelltype spelllevel) (delete channelenergy) spellcaster
        | ((spellchoice !? (view (msix spelltype spelllevel) spellcaster)) == (Just turnundead)) &&
          ((\x -> x <= 1) . length) (filter (\x -> x == channelenergy) (view (msix spelltype spelllevel) spellcaster)) = 
          over (msix spelltype spelllevel) ((delete channelenergy) . (delete turnundead)) spellcaster
        | ((spellchoice !? (view (msix spelltype spelllevel) spellcaster)) == (Just turnundead)) &&
          ((\x -> x > 1) . length) (filter (\x -> x == channelenergy) (view (msix spelltype spelllevel) spellcaster)) = 
          over (msix spelltype spelllevel) (delete channelenergy) spellcaster
        | ((spellchoice !? (view (msix spelltype spelllevel) spellcaster)) == (Just channelenergy)) &&
          ((\x -> x == 1) . length) (filter (\x -> x == channelenergy) (view (msix spelltype spelllevel) spellcaster)) =
          over (msix spelltype spelllevel) ((delete channelenergy) . (delete channelenergy')) spellcaster
        | {-(spelltype == (Just 5)) || (spelltype == (Just 2)) || -}(spelltype == (Just 8)) = 
            over (msix spelltype spelllevel) (delete ((fromJust . (spellchoice !?)) (view (msix spelltype spelllevel) spellcaster))) spellcaster
        | otherwise = over (msix' spelltype spelllevel) function2 spellcaster
{-begin here for persistant area spells-}
fullcastspell :: Maze -> [Maybe Int] -> Maybe Int -> Maybe Int -> Maybe Int -> StdGen -> GameState -> (GameState, [Maybe Int])
fullcastspell maze targets spelltype spelllevel spellchoice g game@(c :| cs)
  | isrepulsionspell = (repulsion spells targets (over (#status) (removespell spelltype spelllevel spellchoice) c :| cs),generatedint)
  | isblackholespell = (blackHole spells targets (over (#status) (removespell spelltype spelllevel spellchoice) caster :| newcharacters),generatedint)
  | issummonspell = (((over (#status) (removespell spelltype spelllevel spellchoice) c) :| ((msummon2character g c ((join . (view (#msummon) <$>)) spell)) ++ cs)),[Just 1])
  | otherwise = ((over (#status) (removespell spelltype spelllevel spellchoice) caster :| newcharacters),generatedint)
    where
      spell
        | spelltype == Nothing && spelllevel == Nothing && spellchoice == Nothing = Just . rollspelldice g $ w2sp Nothing Nothing (view (#status) c)
        | otherwise = (((rollspelldice g) <$>) . (spellchoice !?)) (fst $ getspells spelltype spelllevel (view (#status) c))
      maybespelltarget = fmap (view (#info . #spelltarget)) spell
      spells 
        | isMapArea . fmap (view (#info . #spelltarget)) $ spell = fmap (fmap (over (#meffect) (fmap (over (#temporal) (\x -> (-$-) (const (Just 1)) x))))) thespells
        | otherwise = thespells
          where
            thespells
              | spelltype == Nothing && spelllevel == Nothing && spellchoice == Nothing = fst $ runState ((rollingspelldice ~>) ~> ((take (length targets) . repeat . Just . w2sp Nothing Nothing $ view (#status) c))) g
              | otherwise = fst $ runState ((rollingspelldice ~>) ~> ((take (length targets) . repeat . (spellchoice !?)) (fst $ getspells spelltype spelllevel (view (#status) c)))) g
      issummonspell = ((\x -> x /= Nothing) . join . (view (#msummon) <$>)) spell
      isblackholespell = ((\x -> x == Just "Black Hole") . (view (#info . #name) <$>)) spell
      isrepulsionspell = ((\x -> x == Just "Repulsion") . (view (#info . #name) <$>)) spell
      spellfunction = over (#status) (\x -> (snd <$> (castspell spelltype spelllevel spellchoice g (view (#status) c))) x $ x)
      ((caster :| newcharacters), generatedint)
        | maybespelltarget == (Just Caster) =
          (((over (#status) (\x -> (fst <$> (castspell spelltype spelllevel spellchoice g (view (#status) c))) x $ x) . spellfunction) c :| cs),[Just 0])
        | isAllies maybespelltarget = 
          (safemodifiesat spells (indexfunction filterallies game) (c :| cs), indexfunction filterallies game)
        | isAlliesBurst maybespelltarget = 
          (safemodifiesat spells (indexfunction filteralliesburst game) (c :| cs), indexfunction filteralliesburst game)
        | isEnemies maybespelltarget = 
          (safemodifiesat spells (indexfunction filterenemies game) (c :| cs), indexfunction filterenemies game)
        | isEnemiesBurst  maybespelltarget = 
          (safemodifiesat spells (indexfunction filterenemiesburst game) (c :| cs), indexfunction filterenemiesburst game)
        | isAllBurst  maybespelltarget = 
          (safemodifiesat spells (indexfunction filterallburst game) (c :| cs), indexfunction filterallburst game)
        | isSingleTarget maybespelltarget = 
          (snd $ runState (modifygamestate spellfunction ~> targets) 
          (over (#status) (\x -> (fst <$> (castspell spelltype spelllevel spellchoice g (view (#status) c))) (view (#status) targetedcharacter) $ x) c :| cs), [])
        | otherwise = (safemodifiesat spells targets (c :| cs), [])
          where
            targetedcharacter = case targets of
              [] -> defaultcharacter
              (a: _) -> case (!!?) game a of
                Nothing -> defaultcharacter
                Just y -> y
            tileblockrange = case ((getRange . view (#info . #spelltarget)) <$> spell) of
              Nothing -> 0
              Just y -> y
            filterblockedtiles = (\x -> runLocation x `elem` finalvisionfield maze (runLocation c) tileblockrange)
            filterallies = (\x -> (temporary $ view (#status . #team) c) == (temporary $ view (#status . #team) x) 
              && (Just $ meleedistancechar2 x c) <= ((getRange . view (#info . #spelltarget)) <$> spell))
            filteralliesburst = (\x -> (temporary $ view (#status . #team) c) == (temporary $ view (#status . #team) x) 
              && (Just $ meleedistancechar2 x c) <= ((getRange . view (#info . #spelltarget)) <$> spell) 
              && x /= c && filterblockedtiles x)
            filterenemies = (\x -> (temporary $ view (#status . #team) c) /= (temporary $ view (#status . #team) x) 
              && (Just $ meleedistancechar2 x c) <= ((getRange . view (#info . #spelltarget)) <$> spell))
            filterenemiesburst = (\x -> (temporary $ view (#status . #team) c) /= (temporary $ view (#status . #team) x) 
              && (Just $ meleedistancechar2 x c) <= ((getRange . view (#info . #spelltarget)) <$> spell) 
              && x /= c && filterblockedtiles x)
            filterallburst = (\x -> (Just $ meleedistancechar2 x c) <= ((getRange . view (#info . #spelltarget)) <$> spell) 
              && x /= c && filterblockedtiles x)

blackHole :: [Maybe (Spell Int)] -> [Maybe Int] -> GameState -> GameState
blackHole spells targets game@(c:|cs) = safemodifiesatgeneral spells targets go game
  where
    casterlocation = view (#runLocation) c
    go :: Character -> Character
    go char = set (#runLocation) location char
      where
        charlocation = view (#runLocation) char
        potenitallocations = linealgo charlocation casterlocation
        location = minimumBy (\x y -> compare (abs (2 - (eucliddistance x charlocation))) (abs (2 - (eucliddistance y charlocation)))) potenitallocations

repulsion :: [Maybe (Spell Int)] -> [Maybe Int] -> GameState -> GameState
repulsion spells targets game@(c:|cs) = safemodifiesat newspells targets game
  where
    castername = view (#name) c
    go :: Maybe (Spell Int) -> Maybe (Spell Int)
    go mspint = (over (#meffect) (fmap (set (#temporal . #character) castername))) <$> mspint
    newspells = fmap go spells

safemodifiesatgeneral :: [Maybe (Spell Int)] -> [Maybe Int] -> (Character -> Character) -> NonEmpty Character -> NonEmpty Character
safemodifiesatgeneral [] _ func ns = ns
safemodifiesatgeneral _ [] func ns = ns
safemodifiesatgeneral (f : fs) (i : is) func ns = 
  safemodifiesatgeneral fs is func $ safemodifyat i (if spellbooltest f char then func else id) ns
  where
    char = case ns !!? i of
      Nothing -> defaultcharacter
      Just y -> y

spellbooltest :: Maybe (Spell Int) -> Character -> Bool
spellbooltest mspell char = case mspell of
  Nothing -> False
  Just spell -> go
        where
            go :: Bool
            go
              | view (#info . #target) spell == Nothing = True
              | i2 == 1 = False
              | i2 == 20 = True
              | otherwise = saveFails
            s = view (#status) char
            i = view (#info . #spellbonus) spell
            i2 = view (#rolls . #ddroll) spell
            spelltarget = case view (#info . #target) spell of
              Nothing -> Target Defense 1000
              Just y -> y
            saveFails = (pure $ i + i2 + (view (#bonus) spelltarget)) > target
            ishelpless = 
                (isActive . temporary) (view (#effects . #held) s) || ((isActive . temporary) (view (#effects . #paralyzed) s)) || ((isActive . temporary) (view (#effects . #sleep) s))
            target
                | ishelpless = ffdefensetype (view (#defensetype) spelltarget) $
                    (set (#abilityscores . #wisdom) (pure 10 :: Temporary Int) $ 
                    set (#abilityscores . #dexterity) (pure 0 :: Temporary Int) $ 
                    over (#bonuses . #abilityscores . #wisdom) (fmap (\_ -> pure 0 :: Temporary Int)) $
                    over (#bonuses . #abilityscores . #dexterity) (fmap (\_ -> pure 0 :: Temporary Int)) s)
                | (isActive . temporary) (view (#effects . #prone) s) = ffdefensetype (view (#defensetype) spelltarget) s
                | (isActive . temporary) (view (#effects . #stunned) s) = ffdefensetype (view (#defensetype) spelltarget) s
                | (isActive . temporary) (view (#effects . #grappled) s) = ffdefensetype (view (#defensetype) spelltarget) s
                | (isActive . temporary) (view (#effects . #laughter) s) = ffdefensetype (view (#defensetype) spelltarget) s   
                | (isActive . temporary) (view (#effects . #flatfooted) s) = ffdefensetype (view (#defensetype) spelltarget) s       
                | otherwise = defense2defensetype (view (#defensetype) spelltarget) s

fullcastAreaspell :: Maybe (Spell [Int]) -> Maze -> StdGen -> Location -> GameState -> (GameState, [String])
fullcastAreaspell Nothing _ _ _ game = (game, [])
fullcastAreaspell mspell maze g loc game@(c :| cs) = (safemodifiesat' spells targets (c :| cs), gametext)
    where
      spells = fst $ runState ((rollingspelldice ~>) ~> ((take (length targets) . repeat ) mspell)) g
      targets = areatargets
      tileblockrange = case ((getRange . view (#info . #spelltarget)) <$> mspell) of
        Nothing -> 0
        Just y -> y
      spell :: Spell [Int]
      spell = case mspell of
        Nothing -> testspell
        Just y -> y
      spelltarget :: SpellTarget
      spelltarget = view (#info . #spelltarget) spell
      areatargets = indexfunction (\x -> view (#runLocation) x `elem` locationtargets) game
      locationtargets :: [Location]
      locationtargets = finalvisionfield maze loc (getAreaRadius spelltarget)
      gametext = deleteAll [] $ [go3 spells] ++ (simpleshow4list $ catMaybes spells) ++ (catMaybes $ fmap (fmap detailedview) $ go2 targets (safemodifiesat' spells targets (c :| cs)))
      go2 :: [Maybe Int] -> NonEmpty Character -> [Maybe Character]
      go2 [] _ = []
      go2 (x : xs) y = (y !!? x) : go2 xs y
      go3 spell2 = case catMaybes spell2 of
        [] -> "Name"
        (sp : sps) -> view (#info . #name) sp

fullcastAuraspell :: Maybe (Spell [Int]) -> Maze -> StdGen -> Character -> GameState -> (GameState, [String])
fullcastAuraspell Nothing _ _ _ game = (game, [])
fullcastAuraspell mspell maze g char game@(c :| cs) = (safemodifiesat' spells targets (c :| cs), gametext)
    where
      loc = view (#runLocation) char
      spells = fst $ runState ((rollingspelldice ~>) ~> ((take (length targets) . repeat ) mspell)) g
      targets = areatargets
      tileblockrange = case ((getRange . view (#info . #spelltarget)) <$> mspell) of
        Nothing -> 0
        Just y -> y
      spell :: Spell [Int]
      spell = case mspell of
        Nothing -> testspell
        Just y -> y
      spelltarget :: SpellTarget
      spelltarget = view (#info . #spelltarget) spell
      maybespelltarget = Just spelltarget
      locationtargets :: [Location]
      locationtargets = finalvisionfield maze loc (getAreaRadius spelltarget)
      gametext = deleteAll [] $ [go3 spells] ++ (simpleshow4list $ catMaybes spells) ++ (catMaybes $ fmap (fmap detailedview) $ go2 targets (safemodifiesat' spells targets (c :| cs)))
      go2 :: [Maybe Int] -> NonEmpty Character -> [Maybe Character]
      go2 [] _ = []
      go2 (x : xs) y = (y !!? x) : go2 xs y
      go3 spell2 = case catMaybes spell2 of
        [] -> "Name"
        (sp : sps) -> view (#info . #name) sp
      areatargets
        | isAllies maybespelltarget = (indexfunction filterallies game)
        | isAlliesBurst maybespelltarget = (indexfunction filteralliesburst game)
        | isEnemies maybespelltarget = (indexfunction filterenemies game)
        | isEnemiesBurst  maybespelltarget = (indexfunction filterenemiesburst game)
        | isAllBurst  maybespelltarget = (indexfunction filterallburst game)
        | otherwise = []
          where
            tileblockrange = getRange . view (#info . #spelltarget) $ spell
            filterblockedtiles = (\x -> runLocation x `elem` finalvisionfield maze (runLocation c) tileblockrange)
            filterallies = (\x -> (temporary $ view (#status . #team) c) == (temporary $ view (#status . #team) x) 
              && (meleedistancechar2 x c) <= ((getRange . view (#info . #spelltarget)) $ spell))
            filteralliesburst = (\x -> (temporary $ view (#status . #team) c) == (temporary $ view (#status . #team) x) 
              && (meleedistancechar2 x c) <= ((getRange . view (#info . #spelltarget)) $ spell) 
              && x /= c && filterblockedtiles x)
            filterenemies = (\x -> (temporary $ view (#status . #team) c) /= (temporary $ view (#status . #team) x) 
              && (meleedistancechar2 x c) <= ((getRange . view (#info . #spelltarget)) $ spell))
            filterenemiesburst = (\x -> (temporary $ view (#status . #team) c) /= (temporary $ view (#status . #team) x) 
              && (meleedistancechar2 x c) <= ((getRange . view (#info . #spelltarget)) $ spell) 
              && x /= c && filterblockedtiles x)
            filterallburst = (\x -> (meleedistancechar2 x c) <= ((getRange . view (#info . #spelltarget)) $ spell) 
              && x /= c && filterblockedtiles x)


simpleshow4list :: [Spell Int] -> [String]
simpleshow4list [] = [[]]
simpleshow4list (a : []) = (simpleshow4' a)
simpleshow4list (a : as) = (simpleshow4' a) ++ (simpleshow4list as)

simpleshow4' :: Spell Int -> [String]
simpleshow4' a = deleteAll [] $ [mtargetshow ddroll spellbonustotal mspelltarget ++ (go32 $ view (#mdamdice) a) ++ (go3 $ view (#mdamdice) a)] ++
               [(go42 $ view (#mstatdamage) a) ++ (go4 $ view (#mstatdamage) a)] ++
               [(go52 $ view (#meffect) a) ++ (go5 $ view (#meffect) a)]
  where
    names = (view (#info . #name) a)
    spellbonustotal = view (#info . #spellbonus) a
    mspelltarget = view (#info . #target) a
    effectroll = view (#rolls . #efroll) a
    statdamageroll = view (#rolls . #sdroll) a
    ddroll = view (#rolls . #ddroll) a
    mtargetshow :: Int -> Int -> Maybe Target -> String
    mtargetshow int1 int2 mtarget = case mtarget of
      Nothing -> ""
      Just target -> (show $ view (#defensetype) target) ++ " DC " ++ (show . (int1 + int2 + ) $ view (#bonus) target) ++ "(" ++ show int1 ++ "), "
    diceshow2 :: Int -> Int -> String
    diceshow2 i1 i2 = show ( i1 + i2)
    go ma = case ma of
      Nothing -> ""
      Just y -> show y
    go0 ma = case ma of
      Nothing -> ""
      Just y -> show y ++ ", "
    go02 ma = case ma of
      Nothing -> ""
      Just y -> " " ++ show y
    go3 ldd = case ldd of
      Nothing -> ""
      Just (D x b c d _) -> diceshow2 x b ++ go02 c ++ ", " ++ mtargetshow ddroll spellbonustotal d
    go32 ldd = case ldd of
      Nothing -> ""
      _ -> "Damage: "
    go4 :: Maybe (StatDamage Int) -> String
    go4 msd = case msd of
      Nothing -> ""
      Just (SD mtype mtarg mabil _ dice dicebonus duration durationbonus) -> go0 mtype ++ mtargetshow statdamageroll spellbonustotal mtarg ++ diceshow2 dice dicebonus ++ " " ++ go mabil ++ " Damage" ++ go43 duration durationbonus
    go42 msd = case msd of
      Nothing -> ""
      _ -> "SD: "
    go43 dur dbonus
      | ((dur == [0]) || (dur == [])) && (dbonus == 0) = ""
      | otherwise = "for " ++ diceshow2 (go44 dur) dbonus ++ " r, "
    go44 ls = case ls of
      [] -> 0
      as -> sum as
    go5 :: Maybe (Effect (Temporal Int)) -> String
    go5 meff = case meff of
      Nothing -> ""
      Just (Effect seff temp _ mtarg) -> show seff ++ (go53 . temporalduration $ temp) ++ ", " ++ mtargetshow effectroll spellbonustotal mtarg
    go52 :: Maybe (Effect (Temporal Int)) -> String
    go52 meff = case meff of
      Nothing -> ""
      _ -> "E: "
    go53 mint = case mint of
      Nothing -> ""
      Just y -> show y ++ " r"

deleteAll :: Eq a => a -> [a] -> [a]
deleteAll a as = if a `elem` as then deleteAll a (delete a as) else as

indexfunction :: (Character -> Bool) -> NonEmpty Character -> [Maybe Int]
indexfunction filterfunc (char :| chars) = if filterfunc char then [Just 0] ++ go1 1 chars else go1 1 chars
  where
    go1 :: Int -> [Character] -> [Maybe Int]
    go1 _ [] = []
    go1 int (a: as) = if filterfunc a then [Just int] ++ go1 (int + 1) as else go1 (int + 1) as

indexes :: Eq a => [Maybe Int] -> [a] -> [a]
indexes as bs = catMaybes $ (\x -> x !? bs) <$> as

characterinitiative :: Character -> StdGen -> ((Int, Character), StdGen)
characterinitiative c g = ((initiativetotal, c), g')
  where
    (initiativeroll, g') = roll 20 g
    initiativetotal
      | not $ view (#inCombat) c = -10
      | otherwise = initiative (view (#status) c) + (runRoll initiativeroll)

characterinitiativedice :: Character -> Dice (Int, Character)
characterinitiativedice c = state $ characterinitiative c

rollinitiative :: GameState -> Dice (NonEmpty (Int, Character))
rollinitiative cs = characterinitiativedice ~> cs

initiativefinal :: StdGen -> GameState -> GameState
initiativefinal g cs = finalcs
    where
      rollresult = fst $ runState (rollinitiative cs) g
      finalcs = safemodifyat (Just 0) (set (#topofOrder) True) $ snd <$> (NE.reverse $ NE.sort rollresult)

{-startgame :: StdGen -> Game ()
startgame g = do
  modify (initiativefinal g)
  startturn-}
  
gamemaker :: Character -> Int -> [Character]
gamemaker _ 0 = []
gamemaker c 1 = namechanger c 1 : []
gamemaker c n = namechanger c n : gamemaker c (n - 1) 

namechanger :: Character -> Int -> Character
namechanger c n = over (#name) (\x -> x ++ " " ++ (show n)) c

testgame' :: GameState
testgame' = pri :| ([bor, kas, sep, hud])

testgame :: GameState
testgame = bob :| ([bor, kas, sep, hud, michelle, ancillary, stephen])

testgame2 :: GameState
testgame2 = ten :| ([kit, orc, orcmonk, tenrogue, sorceror, (set (#status . #team) (pure Ally) ronin), temple, sorcerorrogue])

enemies :: GameState
enemies = skeleton :| [Character "Ghoul" (0,0) (0,0) defaultPicture  False False False False (0,0) ghoul [] [],Character "Zombie" (0,0) (0,0) defaultPicture  False False False False (0,0) zombie [] [],Character "Yellow Musk Creep" (0,0) (0,0) defaultPicture  False False False False (0,0) muskcreep [] [],
                      Character "Giant Spider" (0,0) (0,0) defaultPicture  False False False False (0,0) giantspider [] [],Character "Assassin Vine" (0,0) (0,0) defaultPicture  False False False False (0,0) assassinvine [] [],
                      Character "Giant Mantis" (0,0) (0,0) defaultPicture  False False False False (0,0) giantmantis [] [],Character "Unicorn" (0,0) (0,0) defaultPicture  False False False False (0,0) unicorn [] [],Character "Dire Bat" (0,0) (0,0) defaultPicture  False False False False (0,0) direbat [] [],
                      Character "Boar" (0,0) (0,0) defaultPicture  False False False False (0,0) boar [] [],Character "Dire Boar" (0,0) (0,0) defaultPicture  False False False False (0,0) direboar [] [],Character "Dire Wolf" (0,0) (0,0) defaultPicture  False False False False (0,0) direwolf [] [],
                      Character "Goblin" (0,0) (0,0) defaultPicture  False False False False (0,0) goblin [] [],Character "Wolf" (0,0) (0,0) defaultPicture  False False False False (0,0) wolf [] [],Character "Dire Rat" (0,0) (0,0) defaultPicture  False False False False (0,0) direrat [] [],
                      Character "Griffon" (0,0) (0,0) defaultPicture  False False False False (0,0) griffon [] [],thug, preacher, ronin, roninna, oracle, daimyo, Character "Pirate" (0,0) (0,0) defaultPicture  False False False False (0,0) pirate [] [],
                      Character "Skeleton Champion" (0,0) (0,0) defaultPicture  False False False False (0,0) skeletonchampion [] [],Character "Bat Swarm" (0,0) (0,0) defaultPicture  False False False False (0,0) batswarm [] [],Character "Carrion Storm" (0,0) (0,0) defaultPicture  False False False False (0,0) carrionstorm [] [],
                      Character "Giant Leach" (0,0) (0,0) defaultPicture  False False False False (0,0) giantleach [] [],Character "Choaker" (0,0) (0,0) defaultPicture  False False False False (0,0) choaker [] [],Character "Ghoul Stirge" (0,0) (0,0) defaultPicture  False False False False (0,0) ghoulstirge [] [],
                      Character "Shadow" (0,0) (0,0) defaultPicture  False False False False (0,0) shadow [] [],Character "Croaker" (0,0) (0,0) defaultPicture  False False False False (0,0) croaker [] [],Character "Lurcher" (0,0) (0,0) defaultPicture  False False False False (0,0) lurcher [] [],Character "Wood Golem" (0,0) (0,0) defaultPicture  False False False False (0,0) woodgolem [] [],
                      Character "Giant Frog" (0,0) (0,0) defaultPicture  False False False False (0,0) giantfrog [] [],Character "Ghoul Priest" (0,0) (0,0) defaultPicture  False False False False (0,0) ghoulpriest [] [], warriormonk, Character "Wako" (0,0) (0,0) defaultPicture  False False False False (0,0) pirate [] [],wakochief, sorcerormonk,
                      sorcerormonkp, samuraiagent, samuraiagentna, Character "Constrictor Snake" (0,0) (0,0) defaultPicture  False False False False (0,0) constrictorsnake [] [],Character "Wyrmling Forest Dragon" (0,0) (0,0) defaultPicture  False False False False (0,0) wyrmlingforrestdragon [] [],
                      Character "Octopus" (0,0) (0,0) defaultPicture  False False False False (0,0) octopus [] [],Character "Allosaurus" (0,0) (0,0) defaultPicture  False False False False (0,0) allosaurus [] [],Character "Young Green Dragon" (0,0) (0,0) defaultPicture  False False False False (0,0) younggreendragon [] [],Character "Troll" (0,0) (0,0) defaultPicture  False False False False (0,0) troll [] [],
                      Character "Bulette" (0,0) (0,0) defaultPicture  False False False False (0,0) bulette [] [],Character "Centipede Swarm" (0,0) (0,0) defaultPicture  False False False False (0,0) centipedeswarm [] [],Character "Golden Skeleton" (0,0) (0,0) defaultPicture  False False False False (0,0) goldenskeleton [] [],Character "Garudo Bushi" (0,0) (0,0) defaultPicture  False False False False (0,0) moltengolem [] [],
                      Character "Nixie" (0,0) (0,0) defaultPicture  False False False False (0,0) nixie [] [],Character "Bombadier Beetle" (0,0) (0,0) defaultPicture  False False False False (0,0) bombadierbeetle [] [],Character "Wounded Jaguar" (0,0) (0,0) defaultPicture  False False False False (0,0) woundedjaguar [] [], 
                      Character "Guard" (0,0) (0,0) defaultPicture  False False False False (0,0) guard [][], Character "Ice Devil" (0,0) (0,0) defaultPicture  False False False False (0,0) iceDevil [][], Character "Cannon Golem" (0,0) (0,0) defaultPicture  False False False False (0,0) cannongolem [][], Character "Pale Stranger" (0,0) (0,0) defaultPicture  False False False False (0,0) palestranger [][],
                      Character "Ysoki Alchemist" (0,0) (0,0) defaultPicture  False False False False (0,0) ysokiAlchemist [][], Character "Gurstweld" (0,0) (0,0) defaultPicture  False False False False (0,0) hGustweld [][]]

grif :: Character
grif = Character "Griffon" (0,0) (0,0) defaultPicture  False False False False (0,0) griffon [] []

underground35 :: Int -> StdGen -> [Character]
underground35 i1 g
  | i1 < 4 = [fst $ randomizehealthandscores (Character "Yellow Musk Creep" (0,0) (0,0) defaultPicture  False False False False (0,0) muskcreep [] []) g]
  | i1 < 12 = fst $ runState (randomizehealthandscores2 ~> (gamemaker (Character "Giant Spider" (0,0) (0,0) defaultPicture  False False False False (0,0) giantspider [] []) (runRoll . fst $ roll 2 g) )) (snd $ roll 2 g)
  | i1 < 16 = [fst $ randomizehealthandscores (Character "Dire Rat" (0,0) (0,0) defaultPicture  False False False False (0,0) direrat [] []) g]
  | i1 < 20 = [fst $ randomizehealthandscores (Character "Giant Mantis" (0,0) (0,0) defaultPicture  False False False False (0,0) giantmantis [] []) g]
  | i1 < 22 = [fst $ randomizehealthandscores (Character "Ghoul" (0,0) (0,0) defaultPicture  False False False False (0,0) ghoul [] []) g]
  | i1 < 26 = fst $ runState (randomizehealthandscores2 ~> (gamemaker (Character "Dire Bat" (0,0) (0,0) defaultPicture  False False False False (0,0) direbat [] []) (runRoll . fst $ roll 2 g))) (snd $ roll 2 g)
  | i1 < 32 = fst $ runState (randomizehealthandscores2 ~> (gamemaker (Character "Zombie" (0,0) (0,0) defaultPicture  False False False False (0,0) zombie [] []) (runRoll . fst $ roll 3 g))) (snd $ roll 3 g)
  | i1 < 34 = fst $ runState (randomizehealthandscores2 ~> [Character "Ghoul 1" (0,0) (0,0) defaultPicture  False False False False (0,0) ghoul [] [], Character "Ghoul 2" (0,0) (0,0) defaultPicture  False False False False (0,0) ghoul [] []]) g
  | otherwise = fst $ runState (randomizehealthandscores2 ~> [(Character "Zombie 1" (0,0) (0,0) defaultPicture  False False False False (0,0) zombie [] []), (Character "Zombie 2" (0,0) (0,0) defaultPicture  False False False False (0,0) zombie [] []), (Character "Zombie 3" (0,0) (0,0) defaultPicture  False False False False (0,0) zombie [] [])]) g

forrestencounters35 :: Int -> StdGen -> [Character]
forrestencounters35 i1 g 
  | i1 < 4 = [fst $ randomizehealthandscores (Character "Yellow Musk Creep" (0,0) (0,0) defaultPicture  False False False False (0,0) muskcreep [] []) g]
  | i1 < 12 = [fst $ randomizehealthandscores (Character "Boar" (0,0) (0,0) defaultPicture  False False False False (0,0) boar [] []) g]
  | i1 < 16 = [fst $ randomizehealthandscores (Character "Assassin Vine" (0,0) (0,0) defaultPicture  False False False False (0,0) assassinvine [] []) g]
  | i1 < 20 = [fst $ randomizehealthandscores (Character "Giant Mantis" (0,0) (0,0) defaultPicture  False False False False (0,0) giantmantis [] []) g]
  | i1 < 22 = [fst $ randomizehealthandscores (Character "Unicorn" (0,0) (0,0) defaultPicture  False False False False (0,0) unicorn [] []) g]
  | i1 < 26 = [fst $ randomizehealthandscores (Character "Dire Wolf" (0,0) (0,0) defaultPicture  False False False False (0,0) direwolf [] []) g]
  | i1 < 32 = fst $ runState (randomizehealthandscores2 ~> (gamemaker (Character "Wolf" (0,0) (0,0) defaultPicture  False False False False (0,0) wolf [] []) (runRoll . fst $ roll 3 g))) (snd $ roll 3 g)
  | i1 < 34 = [fst $ randomizehealthandscores (Character "Dire Boar" (0,0) (0,0) defaultPicture  False False False False (0,0) direboar [] []) g]
  | otherwise = [fst $ randomizehealthandscores (Character "Griffon" (0,0) (0,0) defaultPicture  False False False False (0,0) griffon [] []) g]

terror :: Character -> Character
terror c = over (#name) ("Terror " ++) $
           over (#status) (t2ix DarkHeal (Permanent Nothing [] Nothing)) c 

isleofterror35 :: Int -> StdGen -> [Character]
isleofterror35 i1 g 
  | i1 < 6 = fmap terror . fst $ runState (randomizehealthandscores2 ~> (gamemaker (Character "Wolf" (0,0) (0,0) defaultPicture  False False False False (0,0) wolf [] []) (runRoll . fst $ roll 4 g))) (snd $ roll 4 g)
  | i1 < 11 = fst $ runState (randomizehealthandscores2 ~> (gamemaker (Character "Golden Skeleton" (0,0) (0,0) defaultPicture  False False False False (0,0) goldenskeleton [] []) (runRoll . fst $ roll 8 g))) (snd $ roll 8 g)
  | i1 < 16 = fmap terror . fst $ runState (randomizehealthandscores2 ~> (gamemaker (Character "Giant Frog" (0,0) (0,0) defaultPicture  False False False False (0,0) giantfrog [] []) (runRoll . fst $ roll 8 g))) (snd $ roll 8 g)
  | i1 < 21 = fst $ runState (randomizehealthandscores2 ~> (gamemaker (Character "Centipede Swarm" (0,0) (0,0) defaultPicture  False False False False (0,0) centipedeswarm [] []) (runRoll . fst $ roll 4 g))) (snd $ roll 4 g)
  | i1 < 26 = [fst $ randomizehealthandscores (Character "Bulette" (0,0) (0,0) defaultPicture  False False False False (0,0) bulette [] []) g]
  | i1 < 31 = fmap terror . fst $ runState (randomizehealthandscores2 ~> (gamemaker (Character "Troll" (0,0) (0,0) defaultPicture  False False False False (0,0) troll [] []) (runRoll . fst $ roll 4 g))) (snd $ roll 4 g)
  | i1 < 35 = fmap terror [fst $ randomizehealthandscores (Character "Allosaurus" (0,0) (0,0) defaultPicture  False False False False (0,0) allosaurus [] []) g]
  | otherwise = [fst $ randomizehealthandscores (Character "Young Green Dragon" (0,0) (0,0) defaultPicture  False False False False (0,0) younggreendragon [] []) g]

abilityscoredice :: AbilityScores [Int]
abilityscoredice = pure [3, 3, 3]

randomizescores1 :: AbilityScores Int -> AbilityScores (Temporary Int) -> AbilityScores (Temporary Int)
randomizescores1 asi asti = liftA2 go asi asti
    where
        go :: Int -> Temporary Int -> Temporary Int
        go int (T ts p)
            | p == 0 = (T ts p)
            | int == 3 && p < 4 = (T ts 1)
            | int == 4 && p < 3 = (T ts 1)
            | int == 5 && p < 2 = (T ts 1)            
            | int == 3 = (T ts (p - 3))
            | int == 4 = (T ts (p - 2))
            | int == 5 = (T ts (p - 1))
            | int == 6 = (T ts p)
            | int == 7 = (T ts (p + 1))
            | int == 8 = (T ts (p + 2))
            | int == 9 = (T ts (p + 3))
            | otherwise = (T ts p)   

randomizescores2 :: Character -> StdGen -> (Character, StdGen)
randomizescores2 c g = (over (#status . #abilityscores) (randomizescores1 (fmap runRoll asi)) c, g2)
  where
    (asi, g2) = runState (sumdice ~> abilityscoredice) g

randomizescores3 :: Character -> Dice Character
randomizescores3 c = state $ randomizescores2 c

healthdicecheck :: Character -> (Int, Int)
healthdicecheck c = ( (view (#status . #primclass . #hp . #permanent) c) `div` 
  (1 + view (#status . #primclass . #level . #permanent) c), 
  view (#status . #primclass . #level . #permanent) c)

rollhealthdice :: Character -> StdGen -> (Character, StdGen)
rollhealthdice c g = (set (#status . #primclass . #hp) (T (newhealth :| []) newhealth) c, g2)
  where
    (dice, level) = healthdicecheck c
    newhealth = (+ ((2 * dice) + 1 - level)) . runRoll . fst $ runState (sumdice $ (2 * (level - 1)) `đ` dice) g
    g2 = snd $ runState (sumdice $ level `đ` dice) g

randomizehealthandscores :: Character -> StdGen -> (Character, StdGen)
randomizehealthandscores c g = (newc2, g3)
  where
    (newc, g2) = randomizescores2 c g
    (newc2, g3) = rollhealthdice newc g2

randomizehealthandscores2 :: Character -> Dice Character
randomizehealthandscores2 c = state $ randomizehealthandscores c

currentCharacter :: Game Character
currentCharacter = gets NE.head

rotateCharacters :: Game ()
rotateCharacters = modify go
  where
    go2 :: Character -> [Character]
    go2 car
      | (temporalduration . temporary) (view (#status . #effects . #summoned) car) == (Just 1) = []
      | otherwise = [car]
    go (c :| cs) = snoc cs c
      where
        snoc :: [Character] -> Character -> NonEmpty Character
        snoc [] b = b :| []
        snoc (a : as) b = a :| (as ++ (go2 b))

nextCharacter :: Game Character
nextCharacter = do
    rotateCharacters
    character <- currentCharacter
    return character

getCharacter :: Character -> Game (Maybe Character)
getCharacter a = gets $ find (\x -> x == a)

infix 9 !!?
(!!?) :: NonEmpty a -> Maybe Int -> Maybe a
(!!?) _ Nothing = Nothing
(!!?) zs (Just i)
    | i < 0     = Nothing
    | otherwise = go i zs
  where
    go :: Int -> NonEmpty a -> Maybe a
    go 0 (x:|_)  = Just x
    go j (_:|xs) = go1 j xs
    go1 :: Int -> [a] -> Maybe a
    go1 1 (y : _) = Just y
    go1 _ [] = Nothing
    go1 j (_: ys) = go1 (j - 1) ys

charsize :: Character -> Int    
charsize x
      | (temporary . view (#status . #tempsize) $ x) <= Medium = 1
      | (temporary . view (#status . #tempsize) $ x) <= Large = 2
      | (temporary . view (#status . #tempsize) $ x) <= Huge = 3
      | (temporary . view (#status . #tempsize) $ x) <= Gargantuan = 4
      | (temporary . view (#status . #tempsize) $ x) <= Collosal = 5
      | otherwise = 1

charsize2 :: Character -> Int    
charsize2 x
      | (temporary . view (#status . #tempsize) $ x) <= Large = 1
      | (temporary . view (#status . #tempsize) $ x) <= Gargantuan = 2
      | (temporary . view (#status . #tempsize) $ x) <= Collosal = 3
      | otherwise = 1

additionalsquare :: Character -> [Location]
additionalsquare c
  | Large == (temporary . view (#status . #tempsize) $ c) = largefunction . runLocation $ c
  | Gargantuan == (temporary . view (#status . #tempsize) $ c) = garganfunction . runLocation $ c
  | otherwise = []
        where
          largefunction (x,y) = [(x+1, y+1), (x+1, y), (x, y+1)]
          garganfunction (x,y) = [(x-1, y+2), (x, y+2), (x+1, y+2), (x+2,y+2), (x+2,y+1), (x+2,y), (x+2,y-1)]

inhabitssquare :: Character -> [Location]
inhabitssquare char = (visionfield' (view (#runLocation) char) (additionalradius . temporary . view (#status . #tempsize) $ char)) ++ (additionalsquare char)
  where
    additionalradius :: Size -> Int
    additionalradius size
      | size <= Large = 0
      | size <= Gargantuan = 5
      | size <= Collosal = 10
      | otherwise = 0

inhabits :: Character -> Location -> Bool
inhabits char loc = loc `elem` (visionfield' (view (#runLocation) char) (additionalradius . temporary . view (#status . #tempsize) $ char)) || loc `elem` (additionalsquare char)
  where
    additionalradius :: Size -> Int
    additionalradius size
      | size <= Large = 0
      | size <= Gargantuan = 5
      | size <= Collosal = 10
      | otherwise = 0

inhabitats :: Character -> [Location] -> Bool
inhabitats char locs = foldr (\x y -> y || inhabits char x) False locs

characterThreatens :: [Location] -> Character -> Bool
characterThreatens locs c = foldl (\x y -> x || (go (a1, b1) y <= (charsize2 c)) || (foldl (\x' y' -> x' || (go y' y <= 1)) False (additionalsquare c))) False locs
    where
      (a1, b1) = view (#runLocation) c
      go (a1', b1') (a2, b2) = max da db
        where
        da = abs $ a1' - a2
        db = abs $ b1' - b2

meleedistancechar :: Character -> Character -> Int
meleedistancechar c1 c2 = distance
    where
      (a1, b1) = view (#runLocation) c1
      (a2, b2) = view (#runLocation) c2
      da
        | a1 >= a2 && ((temporary . view (#status . #tempsize) $ c2) == Large) = a1 - a2 - 1
        | a1 >= a2 && ((temporary . view (#status . #tempsize) $ c2) == Gargantuan) = a1 - a2 - 1
        | a1 <= a2 && ((temporary . view (#status . #tempsize) $ c1) == Large) = a2 - a1 - 1
        | a1 <= a2 && ((temporary . view (#status . #tempsize) $ c1) == Gargantuan) = a2 - a1 - 1
        | otherwise = abs $ a1 - a2
      db
        | b1 >= b2 && ((temporary . view (#status . #tempsize) $ c2) == Large) = b1 - b2 - 1
        | b1 >= b2 && ((temporary . view (#status . #tempsize) $ c2) == Gargantuan) = b1 - b2 - 1
        | b1 <= b2 && ((temporary . view (#status . #tempsize) $ c1) == Large) = b2 - b1 - 1
        | b1 <= b2 && ((temporary . view (#status . #tempsize) $ c1) == Gargantuan) = b2 - b1 - 1
        | otherwise  = abs $ b1 - b2
      offset = (charsize2 c1 - 1) + (charsize2 c2 - 1)
      distance = max da db - offset

meleedistancechar2 :: Character -> Character -> Int
meleedistancechar2 c1 c2
  | weaponrange /= Missile = distance
  | otherwise = eucliddistance
    where
      weaponrange = rangefinder . view (#status . #primaryhand . #weapontype) $ c1
      (a1, b1) = view (#runLocation) c1
      (a2, b2) = view (#runLocation) c2
      da 
        | a1 >= a2 && ((temporary . view (#status . #tempsize) $ c2) == Large) = a1 - a2 - 1
        | a1 >= a2 && ((temporary . view (#status . #tempsize) $ c2) == Gargantuan) = a1 - a2 - 1
        | a1 <= a2 && ((temporary . view (#status . #tempsize) $ c1) == Large) = a2 - a1 - 1
        | a1 <= a2 && ((temporary . view (#status . #tempsize) $ c1) == Gargantuan) = a2 - a1 - 1
        | otherwise = abs $ a1 - a2
      db
        | b1 >= b2 && ((temporary . view (#status . #tempsize) $ c2) == Large) = b1 - b2 - 1
        | b1 >= b2 && ((temporary . view (#status . #tempsize) $ c2) == Gargantuan) = b1 - b2 - 1
        | b1 <= b2 && ((temporary . view (#status . #tempsize) $ c1) == Large) = b2 - b1 - 1
        | b1 <= b2 && ((temporary . view (#status . #tempsize) $ c1) == Gargantuan) = b2 - b1 - 1
        | otherwise  = abs $ b1 - b2
      offset = (charsize2 c1 - 1) + (charsize2 c2 - 1)
      distance = max da db - offset
      eucliddistance = (round . sqrt . fromIntegral $ da * da + db * db) - offset

flanked :: NonEmpty Character -> Maybe Int -> (Bool, String)
flanked (c:|cas) mi = if length flankers >= 1 && closerange then (True, string) else (False, string)
    where
      string = case defender of
        Nothing -> ""
        Just y -> (show $ (\x -> (view (#name) x, meleedistancechar y x)) <$> cas) ++ "Def" ++ (show $ (\x -> (view (#name) x, meleedistancechar c x)) <$> cas) ++ "Att" ++ (show $ (\x -> (view (#name) x, temporary $ view (#status . #team) x)) <$> cas) ++ "Team" ++ (concat $ fmap detailedview flankers)
      defender = (c:|cas) !!? mi
      flankers = filter go cas
      closerange = case defender of
            Nothing -> False
            Just y -> (meleedistancechar c y == 1)
      go :: Character -> Bool
      go char = case defender of
            Nothing -> False
            Just y -> (meleedistancechar char y == 1) && 
                      (meleedistancechar char c == (charsize y + 1)) && 
                      (temporary $ view (#status . #team) c) == (temporary $ view (#status . #team) char) && 
                      (not . isActive . temporary $ view (#status . #effects . #uncannydodge) c)

safemodifyat :: Eq a  => Maybe Int -> (a -> a) -> NonEmpty a -> NonEmpty a
safemodifyat Nothing _ cs = cs
safemodifyat (Just i) f (a :| as)
    | (a :| as) !!? (Just i) == Nothing = (a :| as)
    | i == 0 = f a :| as
    | otherwise = a :| (take (i - 1) as ++ [f $ as !! (i - 1)] ++ (drop i as))

safemodifyats :: (Character -> Character) -> [Maybe Int] -> NonEmpty Character -> NonEmpty Character
safemodifyats _ [] ns = ns
safemodifyats f (i : is) ns = 
  safemodifyats f is $ safemodifyat i f ns

modifygamestate :: (Character -> Character) -> Maybe Int -> Game ()
modifygamestate f i = modify $ safemodifyat i f

safemodifiesat' :: [Maybe (Spell Int)] -> [Maybe Int] -> NonEmpty Character -> NonEmpty Character
safemodifiesat' [] _ ns = ns
safemodifiesat' _ [] ns = ns
safemodifiesat' (f : fs) (i : is) ns = 
  safemodifiesat' fs is $ safemodifyat i (over (#status) ((castspell2 f (view (#status) . NE.head $ ns)))) ns

safemodifiesat :: [Maybe (Spell Int)] -> [Maybe Int] -> NonEmpty Character -> NonEmpty Character
safemodifiesat [] _ ns = ns
safemodifiesat ms@(mspell:mss) mi ns = safemodifyat (Just 0) (over (#status) (fst $ mstring2s2s (join $ view (#mspecial) <$> mspell))) $ safemodifiesat' ms mi ns

modifywhile :: (a -> a) -> (a -> Bool) -> [a] -> [a]
modifywhile _ _ [] = []
modifywhile f g (a : as)
    | g a = [f a] ++ (modifywhile f g as)
    | otherwise = [a] ++ (modifywhile f g as) 


nemodifywhile :: (a -> a) -> (a -> Bool) -> NonEmpty a -> NonEmpty a
nemodifywhile f g ( a :| as)
    | g a = f a :| (modifywhile f g as)
    | otherwise = a :| (modifywhile f g as)

selectCharacter :: Maybe Int ->  Game (Maybe Character)
selectCharacter a = gets (\x -> x !!? a )

cleanattacks :: [Action (Attack  Int) Roll] -> [Action (Attack  Int) Roll]
cleanattacks as
  | length resultsuntilfirstcm == length as = resultsuntilfirstcm
  | otherwise = take (length resultsuntilfirstcm + 1) as
  where
    resultsuntilfirstcm = takeWhile (\x -> view (#rolls . #tohitroll) (runF x) /= Roll 1) as

action2result :: [Action (Attack  Int) Roll] -> Maybe Int -> NonEmpty Character -> ([Result (Maybe Int)], NonEmpty Character)
action2result as i cs = (finalresults , finalcharacters)
      where
        go :: [Action (Attack  Int) Roll] -> Status -> ([(Result (Maybe Int))], Status)
        go a s = runState (attackings a) s
        go2 :: [Action (Attack  Int) Roll] -> Character -> Character
        go2 [] c = c
        go2 xs c = newcharacter
          where
            (_, newstatus) = go xs (status c)
            newcharacter = set (#status) newstatus c
        go1 ::  [Action (Attack  Int) Roll] -> Maybe Character -> [Result (Maybe Int)]
        go1 _ Nothing = []
        go1 [] _ = []
        go1 xs (Just c) = results
          where
            (results, _) = go xs (status c)
        finalresults = go1 (cleanattacks as) $ cs !!? i
        characters = safemodifyat i (go2 (cleanattacks as)) cs
        singlattackfunction = case as of
          [] -> id
          (a : _) -> over (#status) (singleeffectAttack a)
        finalcharacters
          | any isHit finalresults = safemodifyat i singlattackfunction characters
          | otherwise = characters

action2gameresult :: [Action (Attack  Int) Roll] -> Maybe Int -> Game [Result (Maybe Int)]
action2gameresult as i = state $ action2result as i

deleteCharacterHead :: Game ()
deleteCharacterHead  = do
  character <- currentCharacter
  rotateCharacters
  modify $ go character
  where
    go :: Character -> NonEmpty Character -> NonEmpty Character
    go char (a :| as) = a :| delete char as

endturn :: StdGen -> Game ()
endturn g = do
    doStuffWithCharacter g
    rotateCharacters

doStuffWithCharacter :: StdGen -> Game ()
doStuffWithCharacter g = modify go
    where
        go :: NonEmpty Character -> NonEmpty Character
        go (a :| as) = ( go3 . go7 . go6 . go4) $ (go10 . go9 . go5 . go2 . go8) a :| as
        go2 :: Character -> Character
        go2 c = over (#status) reducet2durations c
        go3 :: NonEmpty Character -> NonEmpty Character
        go3 (c :| as)
          | (isActive . temporary) $ view (#status . #effects . #bleeding) c = over (#status) (healthupdate . (damager modifyer)) c :| as
          | otherwise = c :| as
            where
              modifyer = (viewtempmodifyier . temporary) (view (#status . #effects . #bleeding) c)
        go4 :: NonEmpty Character -> NonEmpty Character
        go4 (a :| as)
          | (isOn . temporary) (view (#status . #effects . #sing) a) = a :| 
            (modifywhile (\x -> over (#status) (t2ix Sing (Present (Just 1) [] ((viewtempmodifyier . temporary) (view (#status . #effects . #sing) a)))) x) 
            (\x -> (temporary $ view (#status . #team) a) == (temporary $ view (#status . #team) x)) as)
          | otherwise = a :| as
        go5 :: Character -> Character
        go5 c = over (#status) healthupdate newcharacter
          where
            poisenlist = view (#status . #statdamage) c
            (newpoisenlist1 , g') = rolldurations g poisenlist
            newcharacter = set (#status . #statdamage) (reducedurations newpoisenlist1) $ over (#status) (storedpoisens newpoisenlist1 g') c
        go6 ::  NonEmpty Character -> NonEmpty Character
        go6 (a :| as)
          | (isOn . temporary) (view (#status . #effects . #auraofdespair) a) = a :| 
            (modifywhile (\x -> over (#status) (t2ix Despair (Present (Just 1) [] Nothing)) x) 
            (\x -> (temporary $ view (#status . #team) a) /= (temporary $ view (#status . #team) x)) as)
          | otherwise = a :| as
        go7 ::  NonEmpty Character -> NonEmpty Character
        go7 (a :| as)
          | (isActive . temporary) (view (#status . #effects . #seeinvisibility) a) = a :| 
            (modifywhile (\x -> over (#status) (t2ix Invisible (Absent Nothing [] Nothing)) $ over (#status) (t2ix ImprovedInvisibility (Absent Nothing [] Nothing)) x) 
            (\x -> (temporary $ view (#status . #team) a) /= (temporary $ view (#status . #team) x)) as)
          | otherwise = a :| as
        go8 :: Character -> Character
        go8 c
          | (temporalduration . temporary) (view (#status . #effects . #summoned) c) == (Just 1) && (isActive . temporary $ view (#status . #effects . #summoned) c) = 
            over (#status . #effects . #summoned) (modifytemporary (fmap (+1) -$-)) c
          | otherwise = c
        go9 :: Character -> Character
        go9 c
          | (isActive . temporary) (view (#status . #effects . #regeneration) c) = over (#status) (healthupdate . (damager modifyer)) c
          | otherwise = c
            where
              modifyer = (fmap negate . viewtempmodifyier . temporary) (view (#status . #effects . #regeneration) c)
        go10 :: Character -> Character
        go10 c
          | (isActive . temporary $ view (#status . #effects . #haste) c) = set (#status . #ap) 4 c
          | (isActive . temporary $ view (#status . #effects . #slow) c) = set (#status . #ap) 2 c
          | otherwise = set (#status . #ap) 3 c

doStuffWithCharacter' :: StdGen -> Game ()
doStuffWithCharacter' g = modify go
    where
        go :: NonEmpty Character -> NonEmpty Character
        go (a :| as) = (go5 . go2) <$> 
          (a :| filter (\x -> (not . isActive . temporary $ view (#status . #effects . #summoned) x) || ((Just 1 /=) . temporalduration . temporary $ view (#status . #effects . #summoned) x)) as)
        go2 :: Character -> Character
        go2 c = over (#status) reducet2durations c
        go5 :: Character -> Character
        go5 c = over (#status) healthupdate $ newcharacter
          where
            poisenlist = view (#status . #statdamage) c
            (newpoisenlist1 , g') = rolldurations g poisenlist
            newcharacter = set (#status . #statdamage) (reducedurations newpoisenlist1) $ over (#status) (storedpoisens newpoisenlist1 g') c

startturn :: StdGen -> Maze -> Game [String]
startturn g maze = state $ startturn' g maze

startturn' :: StdGen -> Maze -> NonEmpty Character -> ([String], NonEmpty Character)
startturn' g maze game = ( go . go1 . go2 . go3 . go4 . go5 . go6 . go7 . go8 . go9 . go10)  <$> (go11 game)
  where
    isgravitonmode :: Character -> Bool
    isgravitonmode c = isActive . temporary $ view (#status . #effects . #gravitonmode) c
    isfftarget :: Character -> Bool
    isfftarget c = (\x -> x /= []) . view (#character) . temporary $ view (#status . #effects . #trickattack) c
    isphotonmode :: Character -> Bool
    isphotonmode c = isActive . temporary $ view (#status . #effects . #photonmode) c
    isgrappled :: Character -> Bool
    isgrappled c = ((Just 1 ==) . temporalduration . temporary $ view (#status . #effects . #grappled) c) && (isActive . temporary $ view (#status . #effects . #grappled) c)
    isprone :: Character -> Bool
    isprone c = ((Just 1 ==) . temporalduration . temporary $ view (#status . #effects . #prone) c) && (isActive . temporary $ view (#status . #effects . #prone) c)
    kidefenseactive :: Character -> Bool
    kidefenseactive c = (isOn . temporary . view (#status . #effects . #ki) $ c) && (((Just 2) ==) . viewtempmodifyier . temporary . view (#status . #effects . #ki) $ c)
    forcefieldactive :: Character -> Bool
    forcefieldactive c = isActive . temporary . view (#status . #effects . #forcefield) $ c
    go :: NonEmpty Character -> NonEmpty Character
    go (a :| as)
      | isgrappled a = (over (#status) (t2ix Grappled (Absent Nothing [] Nothing)) a :| as)
      | otherwise = (a :| as)
    go1 :: NonEmpty Character -> NonEmpty Character
    go1 (a :| as)
      | kidefenseactive a = (over (#status) (t2ix Ki (Off Nothing [] Nothing)) a :| as)
      | otherwise = (a :| as)
    go2 :: NonEmpty Character -> NonEmpty Character
    go2 (a :| as)
      | isprone a = (over (#status) (t2ix Prone (Absent Nothing [] Nothing)) a :| as)
      | otherwise = (a :| as)
    go3 :: NonEmpty Character -> NonEmpty Character
    go3 (a :| as)
      |  (isActive . temporary $ view (#status . #effects . #haste) a) = (set (#status . #ap) 4 a :| as)
      |  (isActive . temporary $ view (#status . #effects . #slow) a) = (set (#status . #ap) 2 a :| as)
      | otherwise = (set (#status . #ap) 3 a :| as)
    go4 :: NonEmpty Character -> NonEmpty Character
    go4 (a :| as) = (set (#status . #movementtotal) 0 a :| as)
    go5 :: NonEmpty Character -> NonEmpty Character
    go5 (a :| as) = (set (#runStartLocation) (view (#runLocation) a) a :| as)
    go6 :: NonEmpty Character -> NonEmpty Character
    go6 (a :| as)
      | isgravitonmode a = over (#status . #effects . #gravitonmode) (modifytemporary (fmap ((Just 1) &))) a :| as
      | otherwise = (a :| as)
    go7 :: NonEmpty Character -> NonEmpty Character
    go7 (a :| as)
      | isphotonmode a = over (#status . #effects . #photonmode) (modifytemporary (fmap ((Just 1) &))) a :| as
      | otherwise = (a :| as)
    go8 :: NonEmpty Character -> NonEmpty Character
    go8 (a :| as)
      | forcefieldactive a = over (#status . #effects . #forcefield) (modifytemporary (fmap (\x -> if x & increment > base then base else x & increment))) a :| as
      | otherwise = (a :| as)
      where
        (base, increment) = case view (#status . #equipment . #forcefield) a of
          Nothing -> (Nothing, Nothing)
          Just (x,y) -> (Just x, Just y)
    go9 :: NonEmpty Character -> NonEmpty Character
    go9 (c :| as)
      | (view (#status . #health) c == Stabelized) && (fst . resolvepoints $ view (#status) c) >= 1 = (over (#status) (set (#health) Danger . 
                                                                                                    damager' (#injury) (Just (-1)) . 
                                                                                                    damager' (#resolve) (Just 1)) $ c) :| as
      | (view (#status . #health) c == Dieing) && (fst . resolvepoints $ view (#status) c) >= cost = (over (#status) (set (#health) Stabelized . 
                                                                                                    damager' (#resolve) (Just cost)) $ c) :| as
      | (view (#status . #health) c == Dieing) = (over (#status) (healthupdate . (damager' (#resolve) (Just 1))) c) :| as
      | otherwise = (c :| as)
        where
          cost = min 1 $ (snd . resolvepoints $ view (#status) c) `div` 4
    go10 :: NonEmpty Character -> NonEmpty Character
    go10 world@(c :| as)
      | isfftarget c = nemodifywhile (over (#status) (t2ix FlatFooted (Absent Nothing [] Nothing))) 
        (\x -> (view (#character) . temporary $ view (#status . #effects . #trickattack) c) == (view (#name) x)) 
        ((over (#status . #effects . #trickattack) (modifytemporary (set (#character) [])) c) :| as)
      | otherwise = world
    go11 :: NonEmpty Character -> ([String], NonEmpty Character)
    go11 world@(c :| as) = case view (#status . #aura) c of
      Nothing -> ([],world)
      Just y -> tupleswitch $ fullcastAuraspell (Just $ y Nothing Nothing (view (#status) c)) maze g c world
      where
        tupleswitch (a,b) = (b,a)

passtime1 :: Maybe Int -> NonEmpty Character -> NonEmpty Character
passtime1 mi cs = fmap (over (#status . #effects) (fmap go)) cs
  where
    go :: Temporary (Temporal (Maybe Int)) -> Temporary (Temporal (Maybe Int))
    go t = over (#temporaries) (fmap go3) t
    go2 :: Maybe Int -> Maybe Int -> Maybe Int
    go2 mi1 mi2 = case mi2 of
                  Nothing -> Nothing
                  _ -> max (Just 1) (mi2 & (fmap negate mi1))
    go3 :: Temporal (Maybe Int) -> Temporal (Maybe Int)
    go3 temp
      | isOff temp = temp
      | otherwise = (-$-) (go2 mi) temp

passtime2 :: Maybe Int -> Game ()
passtime2 i = modify (passtime1 i)

passtime3 :: StdGen -> Maybe Int -> Game ()
passtime3 g mi = do
  passtime2 (fmap (\x -> ((x - 1) * 10) + 6) mi)
  doStuffWithCharacter' g 
    
removeeffects :: Game ()
removeeffects = modify go
  where
  go :: NonEmpty Character -> NonEmpty Character
  go as = go2 <$> as
  go2 :: Character -> Character
  go2 c = over (#status) reducet2durations c

passtime :: Maybe Int -> Game ()
passtime i = do
  passtime2 (fmap (* 10) i)
  removeeffects

getstuff :: Character -> ([Either (MagicItem Int) (Either (Armour Int) Weapon)], Weapon, (Armour Int))
getstuff c = (equipment , primaryhand , armour)
  where
    equipment = view (#status . #otherweapons) c
    primaryhand = view (#status . #primaryhand) c
    armour = view (#status . #equipedarmour) c

setstuff :: Character -> ([Either (MagicItem Int) (Either (Armour Int) Weapon)], Weapon, (Armour Int)) -> Character
setstuff c (equipment , primaryhand, armour) = set (#status . #otherweapons) equipment $
                                                      set (#status . #primaryhand) primaryhand $
                                                      set (#status . #equipedarmour) armour c

lootstuff :: Character -> ([Either (MagicItem Int) (Either (Armour Int) Weapon)], Weapon, (Armour Int)) -> Character
lootstuff c (equipment , primaryhand, armour) = over (#status . #otherweapons) (++ equipment ++ go1 ++ go2) c
  where
    go1 = [Right . Right $ primaryhand]
    go2 =  [Right . Left $ armour]

loot :: Character -> Character -> Character
loot looter lootee = lootstuff looter $ getstuff lootee

removefromequipment :: Maybe (Either (Either (MagicItem Int) SpellTemplate) (Either (Armour Int) Weapon)) -> Character -> Character
removefromequipment meaw c = case meaw of
                              Nothing -> c
                              Just z -> case z of
                                Left (Right y) -> over (#status . #itemspells) (delete $ y) c
                                Left (Left y) -> over (#status . #otherweapons) (delete $ Left y) c
                                Right y -> over (#status . #otherweapons) (delete $ Right y) c

addtoequipment :: Maybe (Either (Either (MagicItem Int) SpellTemplate) (Either (Armour Int) Weapon)) -> Character -> Character
addtoequipment meaw c = case meaw of
                              Nothing -> c
                              Just z -> case z of
                                Left (Right y) -> over (#status . #itemspells) (++ [y]) c
                                Left (Left y) -> over (#status . #otherweapons) (++ [Left y]) c
                                Right y -> over (#status . #otherweapons) (++ [Right y]) c

seteffects :: Effects (Temporal (Maybe Int)) -> Character -> Character
seteffects neweffects c = set (#status) newstatus c
    where
        status = view (#status) c
        newstatus = over (#effects) ((removerepeatdurations . removeemptydurations) <$>) $ foldr (.) id (imap go neweffects) status
        go :: StatusEffect -> Temporal (Maybe Int) -> Status -> Status
        go seff temp s
          | temporary (effix seff (view (#status . #effects) c)) == temp = s
          | otherwise = t2ix seff temp s

transfereffects :: Character -> Character -> Character
transfereffects weary rested = seteffects (fmap temporary $ view (#status . #effects) weary) rested

transferstatdamage :: Character -> Character -> Character
transferstatdamage weary rested = set (#status . #primclass) (view (#status . #primclass) weary) $
                                  set (#status . #abilityscores) (view (#status . #abilityscores) weary) rested

rester :: Character -> Character -> Character
rester rested weary = releasefatigued . releaseexhausted . transferstatdamage weary . transfereffects weary . go $ 
  set (#status . #injury) (view (#status . #injury) weary) $ setstuff rested $ getstuff weary
  where
    go c'
      | (0 > ) . temporary . view (#vitalitypoints) . currenthitpoints . view (#status) $ weary = 
        over (#status) (damager' (#injury)  (Just . temporary . view (#vitalitypoints) . currenthitpoints . view (#status) $ weary)) c'
      | otherwise = over (#status) (damager' (#injury)  (Just (-1))) c'
    releaseexhausted c' = over (#status) (t2ix Exhausted (Absent Nothing [] Nothing)) c'
    releasefatigued c' = over (#status) (t2ix Fatigued (Absent Nothing [] Nothing)) c'

newrester :: Character -> Character
newrester c = go . releaseexhausted . releasefatigued $ over (#status) sleep c
  where
    go c'
      | (0 > ) . temporary . view (#vitalitypoints) . currenthitpoints . view (#status) $ c = 
        over (#status) (damager' (#injury)  (Just . temporary . view (#vitalitypoints) . currenthitpoints . view (#status) $ c)) c'
      | otherwise = set (#status . #damage) (pure 0) $ over (#status) (damager' (#injury)  (Just (-1))) c'
    releaseexhausted c' = over (#status) (t2ix Exhausted (Absent Nothing [] Nothing)) c'
    releasefatigued c' = over (#status) (t2ix Fatigued (Absent Nothing [] Nothing)) c'

restone :: Character -> Character -> Character
restone c1 c2
  | c1 == c2 = rester c1 c2
  | otherwise = c2

restall :: Character -> [Character] -> [Character]
restall c cs = modifywhile  (rester c) (== c) cs

restoneandall :: Character -> NonEmpty Character -> NonEmpty Character
restoneandall c1 (c2 :| cs2) = restone c1 c2 :| restall c1 cs2

restmost :: [Character] -> NonEmpty Character -> NonEmpty Character
restmost cs c2s = case cs of
      [] -> c2s
      (a : as) -> restmost as $ restoneandall a c2s

resteveryone :: NonEmpty Character -> NonEmpty Character -> NonEmpty Character
resteveryone (c :| cs) c2s = restmost cs $ restoneandall c c2s

newresteveryone :: NonEmpty Character -> NonEmpty Character
newresteveryone = fmap newrester
{-
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE

data Game = Game
    { characters :: !(NonEmpty Character)
    }

turn :: Game -> (Character, Game)
turn (Game (c :| cs)) = (c, Game (snoc cs c))
  where
    snoc :: [a] -> a -> NonEmpty a
    snoc [] b = b :| []
    snoc (a : as) b = a :| (as ++ [b])

type Character = ()

-}

{-
f :: a -> b
g :: b -> c

g . f :: a -> c

id :: forall a. a -> a
(.) :: (b -> c) -> (a -> b) -> a -> c

g . id = g:: b -> c
id . g = g :: b -> c

f . id = f :: a -> b
id . f = f :: a -> b

f :: a -> m b
g :: b -> m c

pure :: forall a. a -> m a
(<=<) :: (b -> m c) -> (a -> m b) -> a -> m c

f <=< pure = f :: a -> m b
pure <=< f = f :: a -> m b

(<=<) :: (b -> Maybe c) -> (a -> Maybe b) -> a -> Maybe c
(<=<) f g a = case g a of
    Nothing -> Nothing
    Just b -> f b

(<=<) :: (b -> [c]) -> (a -> [b]) -> a -> [c]
(<=<) f g a = go (g a)
  where
    go [] = []
    go (b : _) = f b <> go f bs
-}

{-
newtype State s a = State (s -> (s, a))

instance Functor (State s) where
    fmap f (State m) = State (fmap f . m)

instance Applicative (State s) where
    pure a = State (\s -> (s, a))
    -- liftA2 :: (a -> b -> c) -> State a -> State b -> State c
    liftA2 f (State ma) (State mb) = State go
      where
        go s = (s'', f a b)
          where
            (s', a) = ma s
            (s'', b) = mb s'

instance Monad (State s) where
    return = pure
    -- (>>=) :: State a -> (a -> State b) -> State b
    State ma >>= f = State go
      where
        go s = mb s'
          where
            (s', a) = ma s
            State mb = f a
-}