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
import Data.List.Index (indexed, deleteAt)
import System.Random
import Control.Monad (join)
import Data.Char (isDigit)
import Data.Maybe (catMaybes)
import Control.Lens.Indexed (ifoldMap, imap)
import Data.List (delete, nub, find, (\\), minimumBy)
import System.IO (Handle, IOMode (WriteMode, ReadMode), hPrint, hClose, openFile, hShow, hGetContents)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Codec.BMP
import Graphics.UI.GLUT.Fonts
import Data.ByteString (empty)
import GHC.Float (int2Float)
import qualified Data.Map as MAP

import DND.ATTACKS.Attack (Attack, Result (CriticalMiss), showresults)
import DND.ATTACKS.AttackFunctions (attackRolls, attacksRolls, attackings, function3, isHit)
import DND.ATTACKS.AttackFunctions2 (updateattack, longshots, weapon2listattacks, weapon21attacks, w2a', s2a, chargeupdate)
import DND.ATTACKS.Weapon (WeaponType (..), Weapons, Weapon (Weapon), Range (Missile), rangefinder, Enchantment (..))

import DND.CHARACTER.Character (Character (Character, status, runPicture, mousePoint, runLocation), detailedview, cm2a, sa21a, sa2a, changeweapons, fireshieldcharacter, isMonster, 
  ishelpless, removeequip, showc, samuraiagent, preacher, oracle, warriormonk, ronin, sorcerormonkp, daimyo, defaultPicture, Location, detailedview', helplessornotincombat, changeitem, skillcheck, grelm, pug, nirashi, knu, abess, conquistador, sherrif, ringofswimmingleft, skilldice,
  guildrogue, defaultcharacter, skillcheckint, stephen)
import DND.CHARACTER.Sleep (regainstamina)
import DND.CHARACTER.Class (getLevel)
import DND.CHARACTER.Status (SpellTemplate, Status, Health (Dead, Healthy, Danger), healthupdate, Team (Ally), damager, currenthitpoints, emptystatus, abilitybonuses, gatherbonuses, MagicItem, VisionCategory (..), Size (..), damager', resolvepoints)

import DND.DAMAGEDICE.DamageDice (DamageDice (D), SaveProfile (Negates, Half))
import DND.DAMAGEDICE.DamageDiceFunctions (mddstatus, dDEP2MI)
import DND.DAMAGEDICE.Elemental (Elemental (Fire), i2me, ElementalResistance, Profile)

import DND.DEFENSE.Armour (Armour)
import DND.DEFENSE.DefenseFunctions (i2md)
import DND.DEFENSE.Defense (Target (Target), Defense (Reflex, Defense))

import DND.SPELLS.SpellFunctions (fallpittrap, prepareAreaSpell, getspells, spellsperday, (!?), abix, maix, entangletrap, curewounds, w2sp, dispelllist)
import DND.SPELLS.Spell (getRange, isSphere, isAreaTarget, getAreaRadius, isMapArea, getArea, isBurst, isAllies, isEnemies, isSingleTarget, SpellTarget (Allies, Enemies, SingleTarget, Caster), Spell, rollingspelldice, testspell, SpellArea (..))
import DND.SPELLS.Summons

import DND.STATDAMAGE.StatDamage (StatDamage (SD))

import DND.STATUSEFFECTS.Effect (Effect (Effect))
import DND.STATUSEFFECTS.Effects (StatusEffect (..), on, off, 
  Temporal (Present, Absent, On), isOff, isOn, selecttemporal2, isActive, (&), temporalduration, viewtempmodifyier, setmodifyer)
import DND.STATUSEFFECTS.StatusEffectFunctions2 (t2ix)
import DND.STATUSEFFECTS.StatusEffectFunctions3 (effectslist, twtgwp)


import DND.Action (Action, runF)
import DND.Parser
import DND.Roll (Roll, (~>), sumdice, đ, roll, runRoll, listdice)
import DND.Maps
import DND.TemporaryValue (Temporary, temporary, settemporary)
import DND.Time (Time (Time), addrounds)
import DND.Turn (inhabitats, safemodifyats, charsize, inhabits, regressDC, advanceDC, showPortal, getPortal, advancePortal, deleteAll, simpleshow4', simpleshow4list, fullcastAreaspell, indexfunction, characterThreatens, meleedistancechar2, 
  deleteCharacterHead, meleedistancechar, nemodifywhile, flanked, Game, GameState, endturn, modifygamestate, action2gameresult, (!!?), safemodifyat, testgame, initiativefinal, fullcastspell, gamemaker, enemies, passtime3, terror,
  newresteveryone, loot, addtoequipment, removefromequipment, forrestencounters35, isleofterror35, underground35, randomizehealthandscores2, doStuffWithCharacter', rotateCharacters, testgame2, startturn,
  World (World, runBrightness, runGameMenu, runGameText, runGlobalOffSet, runCharacters, runTarget, runTime, inCombat, runMaze, runGameMode, runGlobalZoom, backGroundEx, backGround, explore), 
  GameMode (DMMode, Map, ToggleMode, EquipmentTargeting, CastSpell, Combat, StartCombat, TargetMode, SpellTargetMode, SingleSpellTargetMode, EquipmentTarget, CombatTargetMode), isDMMode, 
  GameMenu (SkillMenu, SkillMenu2, ChangePresetEncounterMenu, ChangeLevelMenu, ChooseEnemiesMenu, ChooseAttackMenu, ToggleAbilitiesMenu, TakeEquipmentMenu, SpellsMenu, MainMenu, ActionMenu, EndTurnMenu, EquipmentMenu, runMenuText, runClass, runLevel, runSpell, runMenuChoice), 
  isSk2Menu, isSkMenu, isCPEMenu, isToggleMode, isTargetMode, isTargetMode', isEquipmentTarget, getEquipmentTarget, isCEMenu, isCAMenu, isSpellsMenu, isEndTurnMenu, isMainMenu, isActionMenu, isToggleMenu, isEquipmentMenu, isTakeEquipmentMenu, isCLMenu,
  inhabitssquare)

profilethin :: ElementalResistance (Temporary (Profile (Maybe Int)))
profilethin = view (#status . #elementalresistance) stephen

ddint :: DamageDice Int
ddint = D 5 0 (Just Fire) (Just $ Target Defense 0) Negates

addrounds2 :: Int -> World -> World
addrounds2 int world
  | (not . isnight $ time) && isnight newtime = setallyimmediatevsion3 . over (#runGameText) (++ ["Darkness!"]) . over (#runMaze) (set (#runIllumination) (Dark) <$>) .  over (#runTime) (addrounds int) $ world  
  | (not . islate $ time) && islate newtime = setallyimmediatevsion3 . over (#runGameText) (++ ["Twighlight!"]) . over (#runMaze) (set (#runIllumination) (Low) <$>) .  over (#runTime) (addrounds int) $ world  
  | (not . isday $ time) && isday newtime = setallyimmediatevsion3 . over (#runGameText) (++ ["DayBreak!"]) . over (#runMaze) (set (#runIllumination) (NormalVision) <$>) .  over (#runTime) (addrounds int) $ world  
  | otherwise = over (#runTime) (addrounds int) world  
  where
    time = runTime world
    newtime = addrounds int time
    isnight time1 = (view (#hours) time1) >= 23 || (view (#hours) time1) < 3
    islate time1 = ((view (#hours) time1) >= 21 || (view (#hours) time1) < 6) && ((view (#hours) time1) < 23) && ((view (#hours) time1) >= 3)
    isday time1 = (view (#hours) time1) < 21 && (view (#hours) time1) >= 6

runBitmapData :: Picture -> BitmapData
runBitmapData (Bitmap x) = x
runBitmapData _ = bitmapDataOfBMP defaultBMP

getmenuchoice :: World -> Maybe Int
getmenuchoice world
  | (isCAMenu . runGameMenu $ world) = runMenuChoice . runGameMenu $ world
  | otherwise = Nothing

updateFunc :: Float -> World -> IO World
updateFunc _ = go
  where
    go :: World -> IO World
    go world
      | view (#runGameMode) world == StartCombat = startCombat world
      | view (#runGameMode) world == Combat = do
          g <- newStdGen
          newworld <- go1 g world
          makeAttack (getmenuchoice world) newworld
      | (view (#runGameMode) world == CastSpell True) || (view (#runGameMode) world == CastSpell False) = do
          g <- newStdGen
          newworld <- aoonew (fst $ view (#blockaoo) spell) world
          go2 (snd $ view (#blockaoo) spell) world newworld
      | view (#inCombat) world = findAvailableCharacter 0 world
      | otherwise = return world
            where
              target = case view (#runTarget) world of
                [] -> Nothing
                (x: _) -> x
              enemylocation = case (runCharacters world) !!? target of
                Nothing -> view (#runLocation) (NE.head $ runCharacters world)
                Just y ->  view (#runLocation) y
              a = runCharacters world
              (spelltype, spelllevel, chosenspell) = goprime world
              goprime world2
                  | isSpellsMenu menu = (x, y, z)
                  | otherwise = (Nothing, Nothing, Nothing)
                  where
                    menu = (view (#runGameMenu) world2)
                    x = runClass menu
                    y = runLevel menu
                    z = runSpell menu
              spell :: Spell [Int]
              spell = case chosenspell !? (fst $ getspells spelltype spelllevel (view (#status) . NE.head $ (runCharacters world))) of
                Nothing -> testspell
                Just y -> y
              spellapcost = int2Float $ view (#info . #apcost) spell
              go1 g' w'
                | snd (fst $ runState (getAttack True enemylocation (getmenuchoice w') w' g') (runCharacters w')) == [] = return w'
                | (view (#info . #range) (view (#runF) (head (snd (fst $ runState (getAttack True enemylocation (getmenuchoice w') w' g') (runCharacters w'))))) == Missile) = aoonew (isActive . temporary . view (#status . #effects . #combatShot) . NE.head . view (#runCharacters) $ w') w'
                | otherwise = return w'
              go2 bool w1' w2'
                | (fromIntegral . view (#status . #ap) $ (NE.head . runCharacters) w1') - (distanceCheck w1') < spellapcost  && (inCombat world) = do
                  putStrLn $ view (#name) (NE.head . runCharacters $ w1') ++ 
                    ", MT" ++ (show $ view (#status . #movementtotal) (NE.head . runCharacters $ w1')) ++ 
                    ", AP" ++ (show $ (fromIntegral . view (#status . #ap) . NE.head . runCharacters $ w1') - (distanceCheck w1')) ++ 
                    ", SL" ++ (show $ view (#runStartLocation) (NE.head . runCharacters $ w1'))
                  ioEndTurn (over (#runGameText) (++ ["Not Enough AP "]) $ set (#runGameMode) Map w2')
                | ((temporary . (view (#status . #damage)) . NE.head . runCharacters) w1' + (temporary . (view (#status . #injury)) . NE.head . runCharacters) w1') >= 
                    ((temporary . (view (#status . #damage)) . NE.head . runCharacters) w2' + (temporary . (view (#status . #injury)) . NE.head . runCharacters) w2') || bool = castSpell w2'
                | otherwise = ioEndTurn (over (#runGameText) (++ ["Spell Disrupted"]) $ set (#runGameMode) Map $ over (#runCharacters) (safemodifyat (Just 0) (over (#status . #ap) (\x -> x - (ceiling $ spellapcost + (distanceCheck w2'))))) w2')
              go4 gmenu
                | isSpellsMenu gmenu = runClass gmenu
                | otherwise = Nothing

loadBMP2 :: String -> IO (Either Error BMP)
loadBMP2 string = do 
       outh <- readBMP string
       return outh

locationToCoords :: (Float, Float) -> Float -> Location -> CellCoordinates
locationToCoords (xOffset, yOffset) cellSize (x, y) = CellCoordinates
  (centerX, centerY) -- Center
  (centerX - halfCell, centerY + halfCell) -- Top Left
  (centerX + halfCell, centerY + halfCell) -- Top Right
  (centerX - halfCell, centerY - halfCell) -- Bottom Left
  (centerX + halfCell, centerY - halfCell) -- Bottom Right
  where
    (centerX, centerY) =
      ( xOffset + (fromIntegral x) * cellSize
      , yOffset + (fromIntegral y) * cellSize)
    halfCell = cellSize / 2.0

textwidth :: String -> IO (Float, Float)
textwidth str = do
    width <- stringWidth Roman str
    height <- fontHeight Roman
    return (fromIntegral width, height)

brighttime :: Maybe VisionCategory -> Time Int -> VisionCategory
brighttime mvc time = case mvc of
  Just x -> x
  Nothing -> lighttime time

lighttime :: Time Int -> VisionCategory
lighttime time
  | (view (#hours) time) >= 23 || (view (#hours) time) < 3 = Dark
  | (view (#hours) time) >= 21 || (view (#hours) time) < 6 = Low
  | otherwise = NormalVision

charactervisionfield :: VisionCategory -> Maze -> Character -> NonEmpty Character -> ([Location],[Location])
charactervisionfield vcat maze1 char chars = if isActive . temporary . view (#status . #effects . #blind) $ char then ([],[]) else 
  (filter 
  (\x ->  (>=) (view (#runCategory) charvision) . runIllumination . MAP.findWithDefault (MazeBlock Hall Hall Hall Hall Nothing vcat Nothing Nothing) x $ fst $ illuminatecharmap newcharlist vcat maze1) 
  $ finalvisionfield (fst $ illuminatecharmap newcharlist vcat maze1) (runLocation char) (view (#runDistance) charvision),(snd $ illuminatecharmap newcharlist vcat maze1))
  where
    newcharlist = char : filter (char /=) (NE.toList chars)
    charvision = temporary . view (#status . #vision) $ char

drawingFunc :: World -> IO Picture
drawingFunc world = do
  textmenu <- menuTextDisplays 0 (view (#runGameMenu . #runMenuText) world)
  textdisplay <- gameTextDisplays 0 (gameTextChoice 0 (runGameText world))
  functionndrawcharacter <- charPicFunction (xOffset, yOffset) (runGlobalZoom world)
  let function2 x = if view (#mouseSelect) x == False then scale scalingFactor scalingFactor $ uncurry translate (tupleFunction scalingFactor $ function1 x) (functionndrawcharacter x) else scale scalingFactor scalingFactor $ uncurry translate (tupleFunction scalingFactor $ mousePoint x) (functionndrawcharacter x)
  let playerTokens = if (not . inCombat $ world) then reverse $ fmap function2 $ (NE.filter (\x -> view (#pC) x) . runCharacters $ world) ++ ((filter (\x -> (view (#runLocation) x) `elem` immediatevision)) . (NE.filter (\x -> not $ view (#pC) x)) . runCharacters $ world) else  [function2 . NE.head . runCharacters $ world] ++ (reverse $ function2 <$> (NE.filter (\x -> (view (#runLocation) x) `elem` immediatevision || (view (#inCombat) x)) . runCharacters $ world))
  return . Pictures $ (background2 <$> (explore world \\ immediatevision)) ++ background3 ++ (background <$> immediatevision) ++ (playerTokens) ++ [textmenu] ++ [selectedchar] ++ [textdisplay] ++ targetdisplay ++ equipmentdisplay ++ gmmodedisplay
  where
    immediatevision = if (view (#inCombat) world) then (view (#immediateVision) . NE.head . view (#runCharacters) $ world) 
      else nub . join . (view (#immediateVision) <$>) . filter (\x -> view (#pC) x) . NE.toList . view (#runCharacters) $ world
    background :: Location -> Picture
    background charlocation 
      | isDMMode . runGameMode $ world = Pictures [] 
      | hasObject . MAP.lookup charlocation $ runMaze world = 
        Pictures [scale (scalingFactor * 20) (scalingFactor * 20) $ uncurry translate (tupleFunction2 (scalingFactor * 20) (scalingFactor * 20)  $ cellCenter . conversion $ charlocation) (BitmapSection (Rectangle locationtup (35,35)) (runBitmapData $ backGround world)), 
        function3 (getObjectName . MAP.lookup charlocation $ runMaze world) charlocation, 
        wallanddoordrawer charlocation]
      | otherwise = Pictures [scale (scalingFactor * 20) (scalingFactor * 20) $ uncurry translate (tupleFunction2 (scalingFactor * 20) (scalingFactor * 20)  $ cellCenter . conversion $ charlocation) (BitmapSection (Rectangle locationtup (35,35)) (runBitmapData $ backGround world)), wallanddoordrawer charlocation]
      where
        (x, y) = cellCenter . locationToCoords (0,0) 70 $ charlocation
        locationtup = (round $ (x / 2), round $ (y / 2))
    background2 :: Location -> Picture
    background2 charlocation 
      | isDMMode . runGameMode $ world = Pictures []
      | otherwise = Pictures $ [scale (scalingFactor * 20) (scalingFactor * 20) $ uncurry translate (tupleFunction2 (scalingFactor * 20) (scalingFactor * 20)  $ cellCenter . conversion $ charlocation) (BitmapSection (Rectangle locationtup (35,35)) (runBitmapData $ backGroundEx world))] ++ [wallanddoordrawer charlocation]
      where
        (x, y) = cellCenter . locationToCoords (0,0) 70 $ charlocation
        locationtup = (round $ (x / 2), round $ (y / 2))    
    background3 :: [Picture]
    background3
      | isDMMode . runGameMode $ world = (scale (scalingFactor * 20) (scalingFactor * 20) $ 
        uncurry translate (tupleFunction2 (scalingFactor * 20) (scalingFactor * 20) $ conversion2) $ 
        backGround world) : (((wallanddoordrawer . fst) <$> (MAP.toList . runMaze $ world)) ++ 
        ((\x -> if (hasObject2 . MAP.lookup x $ runMaze world) then function3 (getObjectName . MAP.lookup x $ runMaze world) x else Pictures []) <$> (visionfield' (20,20) 100)))
      | otherwise = []
    targetdisplay
      | isTargetMode (runGameMode world) = [Color white $ Polygon [(-315,440), (-315,540), (315,540), (315,440)], translate (-240) 470 $ scale (0.5) (0.5) (Color black (Text "Choose Target"))]
      | otherwise = []
    gmmodedisplay
      | isDMMode (runGameMode world) = [Color white $ Polygon [(-315,440), (-315,540), (315,540), (315,440)], translate (-240) 470 $ scale (0.5) (0.5) (Color black (Text (showPortal . getPortal . runGameMode $ world)))]
      | otherwise = []
    equipmentdisplay
      | isEquipmentMenu (runGameMenu world) = [equipTextDisplays 0 go1]
      | isTakeEquipmentMenu (runGameMenu world) = [equipTextDisplays 0 $ go1 ++ (fmap (either miname (either aname wname)) . view (#status . #otherweapons) $ characterhead)]
      | otherwise = []
        where
            characterhead = NE.head . runCharacters $ world
            go1 :: [String]
            go1 = [(wname . view (#status . #primaryhand) $ characterhead)] ++ 
              [(aname . view (#status . #equipedarmour) $ characterhead)] ++ (catMaybes $ foldl (\x y -> y : x) [] (fmap miname <$> (view (#status . #equipment) characterhead)))
            wname weapon = view (#name) weapon
            aname armour = view (#name) armour
            miname mitem = view (#name) mitem
    selectedchar = selectedcharTextDisplay world
    tupleFunction f (a,b) = ( a / f, b / f)
    tupleFunction2 f f2 (a,b) = ( (a / f) , (b / f2))
    tupleFunction3 f (a,b) = (f a , f b)
    scalingFactor = runGlobalZoom world / 700
    (xOffset, yOffset) = runGlobalOffSet world
    (mapsizeX, mapsizeY) = tupleFunction 70 . tupleFunction3 fromIntegral . tupleFunction3 (\x -> x - 35) . bitmapSize . runBitmapData $ backGround world
    conversion 
      | bool = locationToCoords (xOffset + cmx - omx, yOffset + cmy - omy) (runGlobalZoom world)
      | otherwise = locationToCoords (xOffset, yOffset) (runGlobalZoom world)
      where
        ((omx, omy), (cmx, cmy), bool) = view (#runOldMouseLocation) world
    conversion2 
      | bool = ( xOffset + cmx - omx + mapsizeX * (runGlobalZoom world), yOffset + cmy - omy + mapsizeY * (runGlobalZoom world))
      |otherwise = ( xOffset + mapsizeX * (runGlobalZoom world), yOffset + mapsizeY * (runGlobalZoom world))
      where
        ((omx, omy), (cmx, cmy), bool) = view (#runOldMouseLocation) world
    function1 = cellCenter . conversion . runLocation
    function3 names x = scale scalingFactor scalingFactor $ uncurry translate (tupleFunction scalingFactor . cellCenter . conversion $ x) (objectPicFunction names (xOffset, yOffset) (runGlobalZoom world))
    wallanddoordrawer :: Location -> Picture
    wallanddoordrawer loc = Pictures $ northline ++ southline ++ eastline ++ westline
      where
        mazetype = (runMaze world)
        mmazeblock :: Maybe (MazeBlock Portal) -> MazeBlock Portal
        mmazeblock mmb = case mmb of
          Nothing -> (emptyblock Dark)
          Just y -> y
        northline :: [Picture]
        northline 
          | isWall . runSouth . mmazeblock . MAP.lookup ((\(x, y) -> (x, y + 1)) loc) $ mazetype = [line [cellTopLeft $ conversion loc, cellTopRight $ conversion loc]]
          | isWall . runNorth . mmazeblock . MAP.lookup loc  $ mazetype = [line [cellTopLeft $ conversion loc, cellTopRight $ conversion loc]]
          | isDoorClosed . runSouth . mmazeblock . MAP.lookup ((\(x, y) -> (x, y + 1)) loc) $ mazetype = [Color blue $ line [cellTopLeft $ conversion loc, cellTopRight $ conversion loc]]
          | isDoorClosed . runNorth . mmazeblock . MAP.lookup loc  $ mazetype = [Color blue $ line [cellTopLeft $ conversion loc, cellTopRight $ conversion loc]]
          | loc `elem` (foldr (\x y -> getTrapLoc2 x ++ y) [] mazetype) = [Color red $ line [cellTopLeft $ conversion loc, cellTopRight $ conversion loc]]
          | ((\(x, y) -> (x, y + 1)) loc) `elem` (foldr (\x y -> getTrapLoc2 x ++ y) [] mazetype) = [Color red $ line [cellTopLeft $ conversion loc, cellTopRight $ conversion loc]]
          |otherwise = []
        eastline :: [Picture]
        eastline 
          | isWall . runWest . mmazeblock . MAP.lookup ((\(x, y) -> (x + 1, y)) loc) $ mazetype = [line [cellTopRight $ conversion loc, cellBottomRight $ conversion loc]]
          | isWall . runEast . mmazeblock . MAP.lookup loc $ mazetype = [line [cellTopRight $ conversion loc, cellBottomRight $ conversion loc]]
          | isDoorClosed . runWest . mmazeblock . MAP.lookup ((\(x, y) -> (x + 1, y)) loc) $ mazetype = [Color blue $ line [cellTopRight $ conversion loc, cellBottomRight $ conversion loc]]
          | isDoorClosed . runEast . mmazeblock . MAP.lookup loc $ mazetype = [Color blue $ line [cellTopRight $ conversion loc, cellBottomRight $ conversion loc]]
          | loc `elem` (foldr (\x y -> getTrapLoc2 x ++ y) [] mazetype) = [Color red $ line [cellTopRight $ conversion loc, cellBottomRight $ conversion loc]]
          | ((\(x, y) -> (x + 1, y)) loc) `elem` (foldr (\x y -> getTrapLoc2 x ++ y) [] mazetype) = [Color red $ line [cellTopRight $ conversion loc, cellBottomRight $ conversion loc]]
          |otherwise = []
        southline :: [Picture]
        southline 
          | isWall . runNorth . mmazeblock . MAP.lookup ((\(x, y) -> (x, y - 1)) loc) $ mazetype = [line [cellBottomLeft $ conversion loc, cellBottomRight $ conversion loc]]
          | isWall . runSouth . mmazeblock . MAP.lookup loc $ mazetype = [line [cellBottomLeft $ conversion loc, cellBottomRight $ conversion loc]]
          | isDoorClosed . runNorth . mmazeblock . MAP.lookup ((\(x, y) -> (x, y - 1)) loc) $ mazetype = [Color blue $ line [cellBottomLeft $ conversion loc, cellBottomRight $ conversion loc]]
          | isDoorClosed . runSouth . mmazeblock . MAP.lookup loc $ mazetype = [Color blue $ line [cellBottomLeft $ conversion loc, cellBottomRight $ conversion loc]]
          | loc `elem` (foldr (\x y -> getTrapLoc2 x ++ y) [] mazetype) = [Color red $ line [cellBottomLeft $ conversion loc, cellBottomRight $ conversion loc]]
          | ((\(x, y) -> (x, y - 1)) loc) `elem` (foldr (\x y -> getTrapLoc2 x ++ y) [] mazetype) = [Color red $ line [cellBottomLeft $ conversion loc, cellBottomRight $ conversion loc]]
          |otherwise = []
        westline :: [Picture]
        westline 
          | isWall . runEast . mmazeblock . MAP.lookup ((\(x, y) -> (x - 1, y)) loc) $ mazetype = [line [cellTopLeft $ conversion loc, cellBottomLeft $ conversion loc]]
          | isWall . runWest . mmazeblock . MAP.lookup loc $ mazetype = [line [cellTopLeft $ conversion loc, cellBottomLeft $ conversion loc]]
          | isDoorClosed . runEast . mmazeblock . MAP.lookup ((\(x, y) -> (x - 1, y)) loc) $ mazetype = [Color blue $ line [cellTopLeft $ conversion loc, cellBottomLeft $ conversion loc]]
          | isDoorClosed . runWest . mmazeblock . MAP.lookup loc $ mazetype = [Color blue $ line [cellTopLeft $ conversion loc, cellBottomLeft $ conversion loc]]
          | loc `elem` (foldr (\x y -> getTrapLoc2 x ++ y) [] mazetype) = [Color red $ line [cellTopLeft $ conversion loc, cellBottomLeft $ conversion loc]]
          | ((\(x, y) -> (x - 1, y)) loc) `elem` (foldr (\x y -> getTrapLoc2 x ++ y) [] mazetype) = [Color red $ line [cellTopLeft $ conversion loc, cellBottomLeft $ conversion loc]]
          |otherwise = []

charPicFunction :: (Float, Float) -> Float -> IO (Character -> Picture)
charPicFunction _ _ = do
  return $ (\char -> translate (int2Float $ sizeoffset char) (int2Float $ sizeoffset char) . scale (int2Float $ charsize char) (int2Float $ charsize char) $ Pictures [picturebase char, Pictures [Color black nameplate, Color white (translate (-300) (-315) $ Text (take 10 $ name char))], healthbar char, vitbar char, ffbar char])
  where
    hitpoints x = view (#hitpoints) $ currenthitpoints $ view (#status) x
    currenthp x = temporary $ hitpoints x
    maxhp x = view (#permanent) $ hitpoints x
    xvaluehp x = ((700 :: Float) * (fromIntegral $ currenthp x :: Float) / (fromIntegral $ maxhp x :: Float)) - 350
    vitpoints x = view (#vitalitypoints) $ currenthitpoints $ view (#status) x
    currentvp x = temporary $ vitpoints x
    maxvp x = view (#permanent) $ vitpoints x
    xvaluevp x = ((700 :: Float) * (fromIntegral $ currentvp x :: Float) / (fromIntegral $ maxvp x :: Float)) - 350
    currentff x = if (not . isActive . temporary . view (#status . #effects . #forcefield) $ x) then 0 
                    else (maybeint2int . viewtempmodifyier . temporary . view (#status . #effects . #forcefield) $ x)
    maxff x = case view (#status . #equipment . #forcefield) x of
      Nothing -> if (maybeint2int . viewtempmodifyier . temporary . view (#status . #effects . #forcefield) $ x) < 1 then 1 else (maybeint2int . viewtempmodifyier . temporary . view (#status . #effects . #forcefield) $ x)
      Just y -> fst y
    xvalueff x = ((700 :: Float) * (fromIntegral $ currentff x :: Float) / (fromIntegral $ maxff x :: Float)) - 350
    name x = view (#name) x
    picturebase x = runPicture x
    nameplate = Polygon [(-315,-350), (-315,-200), (315, -200), (315,-350)]
    healthbar x = Color green $ Polygon [(-350,350), (xvaluehp x, 350), (xvaluehp x, 317), (-350,317)]
    vitbar x = Color red $ Polygon [(-350,317), (xvaluevp x, 317), (xvaluevp x, 282), (-350,282)]
    ffbar x = Color blue $ Polygon [(-350,282), (xvalueff x, 282), (xvalueff x, 250), (-350,250)]
    sizeoffset x
      | (temporary . view (#status . #tempsize) $ x) == Large || (temporary . view (#status . #tempsize) $ x) == Gargantuan = 350
      | otherwise = 0

objectPicFunction :: String -> (Float, Float) -> Float -> Picture
objectPicFunction name _ _ = Pictures [Pictures [Color black nameplate, Color white (translate (-300) (-315) $ Text name)]]
  where
    nameplate = Polygon [(-315,-350), (-315,-200), (315, -200), (315,-350)]

menuTextDisplay :: String -> Picture
menuTextDisplay string = Pictures [Color black $ Polygon [(-150, -45), (-150, 45), (150, 45), (150, -45)], scale (0.15) (0.25) $ Color white $ translate (-800) (-50) $ Text string]

selectedcharTextDisplay :: World -> Picture
selectedcharTextDisplay world = Pictures [translate (-810) 500 $ Color go $ Polygon [(-150, -75), (-150, 45), (150, 45), (150, -75)], translate (-870) 500 $ scale (0.15) (0.25) $ Color white $ Text string, translate (-950) 470 $ scale (0.10) (0.163) $ Color white $ Text string2, translate (-950) 440 $ scale (0.10) (0.163) $ Color white $ Text string3]
  where
    go
      | ((view (#inCombat) $ (NE.head . runCharacters) world)) = red
      |otherwise = black
    string = ((view (#name) $ (NE.head . runCharacters) world))
    (string2, string3) = ((detailedview' . NE.head . runCharacters) world)

gameTextDisplay :: String -> Picture
gameTextDisplay string = Pictures [Color black $ Polygon [(-150, -12.5), (-150, 12.5), (250, 12.5), (250, -12.5)], scale (0.10) (0.163) $ translate (-1500) (-50) $ Color white $ Text string]

menuTextDisplays :: Int -> [String] -> IO Picture
menuTextDisplays _ [] = return $ Pictures []
menuTextDisplays int (a : []) = do
  (width,_) <- textwidth a
  let newa = if (0.15 * width) >= 250 then take 25 a else a
  return $ Pictures [translate 810 (fromIntegral $ go int) (menuTextDisplay newa)]
  where
    go int' = 500 - (98 * int')
menuTextDisplays int (a : as) = do
  (width,_) <- textwidth a
  let newa = if (0.15 * width) >= 250 then take 25 a else a
  fmap (\x -> Pictures $ [translate 810 (fromIntegral $ go int) (menuTextDisplay newa)] ++ go2 x) (menuTextDisplays (int + 1) as) 
  where
    go int' = 500 - (98 * int')
    go2 (Pictures x) = x

gameTextDisplays :: Int -> [String] -> IO Picture
gameTextDisplays _ [] = return $ Pictures []
gameTextDisplays int (a : []) = do
  (width,_) <- textwidth a
  let newa = if (0.1 * width) >= 400 then take 50 a else a
  return $ Pictures [translate (-810) (fromIntegral $ go int) (gameTextDisplay newa)]
  where
    go int' = 410 - (25 * int')
gameTextDisplays int (a : as) = do
  (width,_) <- textwidth a
  let newa = if (0.1 * width) >= 400 then take 50 a else a
  fmap (\x -> Pictures $  [translate (-810) (fromIntegral $ go int) (gameTextDisplay newa)] ++ go2 x) (gameTextDisplays (int + 1) as)
  where
    go int' = 410 - (25 * int')
    go2 (Pictures x) = x

equipTextDisplays :: Int -> [String] -> Picture
equipTextDisplays _ [] = Pictures []
equipTextDisplays int (a : []) = Pictures [translate 0 (fromIntegral $ go int) (gameTextDisplay a)]
  where
    go int' = 410 - (25 * int')
equipTextDisplays int (a : as) = Pictures [translate 0 (fromIntegral $ go int) (gameTextDisplay a), (equipTextDisplays (int + 1) as)] 
  where
    go int' = 410 - (25 * int')

gameTextChoice :: Int -> [String] -> [String]
gameTextChoice offset strings
  | length strings <= 20 = strings
  | otherwise = take 20 $ drop go strings
  where
    go
      | (length strings - (20 + offset)) >= 0 = length strings - (20 + offset)
      | otherwise = 0

inputHandler :: Event -> World -> IO World
inputHandler event world
  |isNumberPress event = inputHandlerMenu event world
  |view (#runGameMode) world == Map = inputHandlerMap event world
  |isTargetMode' (view (#runGameMode) world) = inputHandlerTarget event world
  |isEquipmentTarget (view (#runGameMode) world) = inputEquipmentHandlerTarget event world
  |view (#runGameMode) world == SingleSpellTargetMode = inputHandlerSpellTarget event world
  |view (#runGameMode) world == SpellTargetMode = inputHandlerSpellTargets event world
  |view (#runGameMode) world == CombatTargetMode = inputHandlerCombatTargets event world
  |isDMMode (view (#runGameMode) world) = inputHandlerDMMode event world
  |otherwise = inputHandlerMap event world
  where
    EventKey (Char a) Down _ _ = event
    menu = view (#runGameMenu) world

isNumberPress :: Event -> Bool
isNumberPress (EventKey (Char a) Down _ _) = a == '1' || a == '2' || a == '3' || a == '4' || a == '5' || a == '6' || a == '7' || a == '8' || a == '9' || a == '0' || a == 'q'
isNumberPress (EventKey (SpecialKey KeySpace) Down _ _) = True
isNumberPress _ = False

inputHandlerMenu :: Event -> World -> IO World
inputHandlerMenu event world
  | isCLMenu menu = changeLevelsMenu event world
  | isSkMenu menu = skillCheckMenu event world
  | isSk2Menu menu = skillCheckMenu2 event world
  | isCPEMenu menu = choosePresetMenu event world
  | isCEMenu menu = chooseenemiesMenu event world
  | isCAMenu menu = chooseAttacksMenu event world
  | isEndTurnMenu menu = endTurnMenu event world
  | isToggleMode (runGameMode world) = toggleMode event world
  | isEquipmentMenu menu = equipmentMenu event world
  | isTakeEquipmentMenu menu = takeequipmentMenu event world
  | isToggleMenu menu = toggleMenu event world
  | isMainMenu menu = mainMenu event world
  | isActionMenu menu = actionMenu event world
  | otherwise = spellMenuDisambiguate event world
  where
    menu = view (#runGameMenu) world

spellMenuDisambiguate :: Event -> World -> IO World
spellMenuDisambiguate (EventKey (Char 'q') Down _ _) x = cancelTarget x
spellMenuDisambiguate event world = go
  where
    menu = view (#runGameMenu) world
    spellclass = runClass menu
    spelllevel = runLevel menu
    spellchoice = runSpell menu
    menuText = runMenuText menu
    go 
      | spellclass == Nothing = spellMenu event world
      | spelllevel == Nothing = spellclassMenu event world
      | "Choose Spell" `elem` menuText = spellchoiceMenu event world
      | otherwise = spelltargets2 (fmap (view (#info . #spelltarget)) $ safedoublexclam (fst $ getspells spellclass spelllevel ((view (#status) . NE.head) (view (#runCharacters) world))) spellchoice) world

toggleMenu :: Event -> World -> IO World
toggleMenu (EventKey (Char 'q') Down _ _) x = cancelTarget x
toggleMenu (EventKey (Char y) Down _ _) x = do
  newworld <- toggleabilities (Just $ read [y]) x
  return newworld

equipmentMenu :: Event -> World -> IO World
equipmentMenu (EventKey (Char 'q') Down _ _) x = ioEndTurn x
equipmentMenu (EventKey (Char y) Down _ _) x = do
  newworld <- changeequip' (Just $ read [y]) x
  return newworld

takeequipmentMenu :: Event -> World -> IO World
takeequipmentMenu (EventKey (Char 'q') Down _ _) x = ioEndTurn x
takeequipmentMenu (EventKey (Char y) Down _ _) x = do
  newworld <- takeequip' (Just $ read [y]) x
  return newworld

toggleMode :: Event -> World -> IO World
toggleMode (EventKey (Char 'q') Down _ _) x = cancelTarget x
toggleMode (EventKey (Char y) Down _ _) x = do
  go1 (safedoublexclam (disavailableabilities a ++ availableabilities a) ability)
  where    
    (a :| as) = runCharacters x
    go1 Nothing = ioEndTurn $ set (#runGameMode) Map x
    go1 (Just tup) = (ioEndTurn $ set (#runGameMode) Map $ set (#runCharacters) (over (#status) (uncurry t2ix ((fst tup), ((\_ -> (Just $ read [y])) <$> (snd tup)))) a :| as) x)
    ability = case runTarget x of
      [] -> Nothing
      (a:_) -> a

chooseAttacksMenu :: Event -> World -> IO World
chooseAttacksMenu (EventKey (Char 'q') Down _ _) x = cancelTarget x
chooseAttacksMenu (EventKey (Char y) Down _ _) x = do
  _ <- putStrLn [y]
  return $ (set (#runGameMode) TargetMode . set (#runTarget) [] . set (#runGameMenu) (ChooseAttackMenu [] (Just $ read [y]))) x


mainMenu :: Event -> World -> IO World
mainMenu (EventKey (Char '1') Down _ _) x = return $ set (#runGameMenu) (ActionMenu actionmenutext) x
mainMenu (EventKey (Char '2') Down _ _) x = timepasserprime (Just 10) . over (#runGameText) (\x -> x ++ ["Pause for the Cause!"]) $ over (#runCharacters) (nemodifywhile (over (#status) regainstamina) (view (#pC))) x
mainMenu (EventKey (Char '3') Down _ _) x = restnew x
mainMenu (EventKey (Char '4') Down _ _) x = timepasser x
mainMenu (EventKey (Char '5') Down _ _) x = savecharacters x
mainMenu (EventKey (Char '6') Down _ _) x = addAlly x
mainMenu (EventKey (Char '7') Down _ _) x = return $ set (#runGameMenu) (ChooseEnemiesMenu  ["Choose Enemies", "0) N. Random Monster", "1) P. Random Monster", "2) Preset Encounter", "3) Add Monster", "4) Select on Map", "5) Cancel"]) x
mainMenu (EventKey (Char '8') Down _ _) x
  | isDMMode . runGameMode $ x = changeMapDM x
  | otherwise = return $ set (#runGameMenu) (ChangeLevelMenu ["0)Tonatzin","1)Glenn","2)Inn","3)Path", "4)WillowInn", "5) Tents", "6)Shop", "7)Manor", "8)Magator Facient"]) x
mainMenu (EventKey (Char '9') Down _ _) x
  | isDMMode . runGameMode $ x = return . set (#runGameMode) Map $ x
  | otherwise = do
    putStrLn . show . bitmapSize . runBitmapData $ backGround x
    return . set (#runGameMode) (DMMode (Left Hall)) $ x
mainMenu (EventKey (Char '0') Down _ _) x
  | isDMMode . runGameMode $ x = savemap "map.txt" x
  | otherwise = return x
mainMenu _ w = return w

changeLevelsMenu :: Event -> World -> IO World
changeLevelsMenu (EventKey (Char 'q') Down _ _) x = cancelTarget x
changeLevelsMenu (EventKey (Char y) Down _ _) x = changeMap (Just . read $ [y]) x

showskill :: ((Int, String), Character) -> String
showskill ((i, string),char) = (view (#name) char) ++ " gets "  ++ (show i) ++ " on their " ++ string ++ " check."

skillCheckMenu :: Event -> World -> IO World
skillCheckMenu (EventKey (Char 'q') Down _ _) x = cancelTarget x
skillCheckMenu (EventKey (SpecialKey KeySpace) Down _ _) x = return $ set (#runGameMenu) (SkillMenu2 skillmenutext2) x
skillCheckMenu (EventKey (Char '0') Down _ _) x = do
  g <- newStdGen
  return $ over (#runGameText) (++ [showskill . fst $ skillcheckint 10 (NE.head $ runCharacters x) g]) x
skillCheckMenu (EventKey (Char y) Down _ _) x = do
  g <- newStdGen
  return $ over (#runGameText) (++ [showskill . fst $ skillcheckint (read [y]) (NE.head $ runCharacters x) g]) x

skillCheckMenu2 :: Event -> World -> IO World
skillCheckMenu2 (EventKey (Char 'q') Down _ _) x = cancelTarget x
skillCheckMenu2 (EventKey (SpecialKey KeySpace) Down _ _) x = return $ set (#runGameMenu) (SkillMenu skillmenutext) x
skillCheckMenu2 (EventKey (Char '0') Down _ _) x = do
  g <- newStdGen
  return $ over (#runGameText) (++ [showskill . fst $ skillcheckint 20 (NE.head $ runCharacters x) g]) x
skillCheckMenu2 (EventKey (Char y) Down _ _) x = do
  g <- newStdGen
  return $ over (#runGameText) (++ [showskill . fst $ skillcheckint (read [y] + 10) (NE.head $ runCharacters x) g]) x

choosePresetMenu :: Event -> World -> IO World
choosePresetMenu (EventKey (Char 'q') Down _ _) x = cancelTarget x
choosePresetMenu (EventKey (Char y) Down _ _) x = do  
  pic1 <- loadBMP "DefaultToken.bmp"
  g <- newStdGen
  startgame $ set (#runCharacters) (setalliesinCombat $ a :| as ++ (randomizeMonsterLocation pic1 x . fmap (set (#inCombat) True) $ presetencounter2 g (Just . read $ [y]))) x
  where
    (a :| as) = view (#runCharacters) x

chooseenemiesMenu :: Event -> World -> IO World
chooseenemiesMenu (EventKey (Char '0') Down _ _) x = randomenemiesnew (view (#runMonsterRest) x) x
chooseenemiesMenu (EventKey (Char '1') Down _ _) x = randomenemies2new (view (#runMonsterRest) x) x
chooseenemiesMenu (EventKey (Char '2') Down _ _) x = return $ (set (#runGameMenu) (ChangePresetEncounterMenu presetencountermenutext))  x
chooseenemiesMenu (EventKey (Char '3') Down _ _) x = selectmonsters x
chooseenemiesMenu (EventKey (Char '4') Down _ _) x = return $ (set (#runGameMode) CombatTargetMode . set (#runTarget) []) x
chooseenemiesMenu (EventKey (Char '5') Down _ _) x = return $ set (#runGameMenu) (MainMenu  mainmenutext) x
chooseenemiesMenu (EventKey (Char 'q') Down _ _) x = cancelTarget x
chooseenemiesMenu _ w = return w

triggerAreaSpells :: World -> IO World
triggerAreaSpells world = do
  putStrLn "TriggerArea"
  g <- newStdGen
  let (newchars, results) = MAP.foldrWithKey (\x y z -> (go g x y z)) (chars, []) maze
  return . over (#runMaze) (fmap $ alterduration 1) . over (#runGameText) (\x -> x ++ results) $ set (#runCharacters) newchars world
  where
    chars = runCharacters world
    maze = runMaze world
    go :: StdGen -> Location -> MazeNode -> (GameState, [String]) -> (GameState, [String])
    go g' loc mnode (gs, str) = fmap go2 $ fullcastAreaspell (fmap fst $ view (#runspell) mnode) maze g' loc gs
      where
        go2 ss
          | ss == [] = str
          | otherwise = str ++ ss

alterduration :: Int -> MazeNode -> MazeNode
alterduration int mnode1 = case view (#runspell) mnode1 of
      (Just (x, y)) -> if y - int <= 0 then set (#runspell) Nothing mnode1 else set (#runspell) (Just (x, y - int)) mnode1
      Nothing -> mnode1

endTurnMenu :: Event -> World -> IO World
endTurnMenu (EventKey (Char '1') Down _ _) x
  | ((view (#topofOrder) . NE.head) a == True) = do
                                                  g <- newStdGen
                                                  let newcharacters2 = snd $ runState (endturn g) a
                                                  let (message,newcharacters) = runState (startturn g maze) newcharacters2
                                                  triggerAreaSpells $ (over (#runGameText) (++ message) . set (#runGameMenu) 
                                                    (ActionMenu actionmenutext) $ 
                                                    addrounds2 1 . set (#runCharacters) newcharacters $ x)
  | otherwise = do
                                                  g <- newStdGen
                                                  let newcharacters2 = snd $ runState (endturn g) a
                                                  let (message,newcharacters) = runState (startturn g maze) newcharacters2
                                                  return (over (#runGameText) (++ message) . set (#runGameMenu) 
                                                    (ActionMenu actionmenutext) $ 
                                                    set (#runCharacters) newcharacters $ x)
    where
      a = runCharacters x
      time = runTime x
      maze = view (#runMaze) x
endTurnMenu (EventKey (Char '0') Down _ _) x = return $ (over (#runCharacters) ((set (#topofOrder) False <$>) . (set (#inCombat) False <$>)) . set (#inCombat) False . 
                                                set (#runGameMenu) (MainMenu  mainmenutext)) $ x
endTurnMenu (EventKey (Char '2') Down _ _) x = return (set (#runGameMenu) (ActionMenu actionmenutext) x)
endTurnMenu _ w = return w

findAvailableCharacter :: Int -> World -> IO World
findAvailableCharacter 2 world = return world
findAvailableCharacter int world
  | view (#topofOrder) (NE.head a) && ((not . view (#inCombat) . NE.head) a) = findAvailableCharacter (int + 1) $ set (#runCharacters) nexta world
  | (not . view (#inCombat) . NE.head) a = findAvailableCharacter int $ set (#runCharacters) nexta world
  | otherwise = return world
  where
    a = runCharacters world
    nexta = (snd $ runState rotateCharacters a)

actionMenu :: Event -> World -> IO World
actionMenu (EventKey (Char '1') Down _ _) x = return $ (set (#runGameMode) TargetMode . set (#runTarget) []) x
actionMenu (EventKey (Char '2') Down _ _) x = return $ set (#runGameMenu) (SpellsMenu Nothing Nothing Nothing (reverse $ spellsAvailable x 1 ["Choose Spell Type"] ["1) Druid", "2) Cleric", "3) Paladin", "4) Ranger", "5) Wizard", "6) Sorceror", "7) Bard", "8) Special Abilitites", "9) Equipment"])) x
actionMenu (EventKey (Char '3') Down _ _) x = return $ set (#runGameMenu) (ToggleAbilitiesMenu (go (length (disavailableabilities (NE.head . runCharacters $ x))) <$> (indexed $ fst <$> ((disavailableabilities (NE.head . runCharacters $ x)) ++ (availableabilities (NE.head . runCharacters $ x)))))) x
  where    
    go :: Int -> (Int, StatusEffect) -> String
    go x (a1, Prone)
      | a1 > x = show a1 ++ ": " ++ "Get Down, "
      | otherwise = show a1 ++ ": " ++ "Get Up, "
    go _ (a1, b1) = show a1 ++ ": " ++ (show b1) ++ ", "
actionMenu (EventKey (Char '4') Down _ _) x = return $ (set (#runGameMenu) (EquipmentMenu (go <$> (indexed . view (#status . #otherweapons) . NE.head . view (#runCharacters) $ x))) . set (#runTarget) []) x
  where
  go :: (Int, Either (MagicItem Int) (Either (Armour Int) Weapon)) -> String
  go (a1, b) = show a1 ++ ": " ++ (either (miname) (either aname wname) b) ++ ", "
  wname weapon = view (#name) weapon
  aname armour =  view (#name) armour
  miname mitem = view (#name) mitem

actionMenu (EventKey (Char '5') Down _ _) x = return . set (#runGameMode) EquipmentTarget . set (#runTarget) [] $ x
actionMenu (EventKey (Char '6') Down _ _) x = return $ (set (#runGameMenu) (ChooseAttackMenu ["1) Trip", "2) Grapple", "3) Disarm"] Nothing) . set (#runTarget) []) x
actionMenu (EventKey (Char '7') Down _ _) x = takecover x
actionMenu (EventKey (Char '8') Down _ _) x = ioEndTurn x
actionMenu (EventKey (Char '9') Down _ _) x = chooseAbilities x
actionMenu (EventKey (Char '0') Down _ _) x = return $ set (#runGameMenu) (SkillMenu skillmenutext) x
actionMenu (EventKey (Char 'q') Down _ _) x = cancelTarget x
actionMenu _ w = return w

spellsAvailable :: World -> Int -> [String] -> [String] -> [String]
spellsAvailable _ _ as [] = as
spellsAvailable _ 10 as _ = as
spellsAvailable x int savestring (p : ps) = if length (go int 9) >= 1 then spellsAvailable x (int + 1) (p : savestring) ps else spellsAvailable x (int + 1) savestring ps
  where
    go :: Int -> Int -> [Spell [Int]]
    go _ 0 = []
    go stint slint = (fst $ getspells (Just stint) (Just slint) (view (#status) . NE.head . view (#runCharacters) $ x)) ++ (go stint (slint - 1))

spellslist :: World -> Maybe Int -> Int -> [String] -> [String]
spellslist _ _ 0 as = as
spellslist x int int2 as = if length (go (Just int2)) >= 1 then spellslist x int (int2 - 1) (as ++ [show int2]) else spellslist x int (int2 - 1) as
  where
    go :: Maybe Int -> [Spell [Int]]
    go spelllevel = (fst $ getspells int spelllevel (view (#status) . NE.head . view (#runCharacters) $ x))

spellMenu :: Event -> World -> IO World
spellMenu (EventKey (Char a) Down _ _) x
  | a == '8' || a == '9' = return $ set (#runGameMenu) (SpellsMenu go (Just 1) Nothing (["Choose Spell"] ++ spelltext)) x
  | otherwise = return $ set (#runGameMenu) (SpellsMenu go Nothing Nothing (spellslist x go 9 ["Choose Spell Level"])) x
    where
      spelltext = ((go2 <$>) . indexed) (fst $ getspells go (Just 1) ((view (#status) . NE.head . view (#runCharacters)) x))
      go2 (a1, b1) = show a1 ++ ": " ++ (show $ view (#info . #name) b1)    
      go = do
        y <- parseT integral [a]
        (fmap fst y)
spellMenu _ w = return w

spellclassMenu :: Event -> World -> IO World
spellclassMenu (EventKey (Char a) Down _ _) x = setSpellLevel go x
  where
    go = do
      y <- parseT integral [a]
      (fmap fst y)
spellclassMenu _ w = return w

spellchoiceMenu :: Event -> World -> IO World
spellchoiceMenu (EventKey (Char a) Down _ _) x = do
  newa <- parseT integral [a]
  let newint = fmap fst newa
  spelltargets2 (fmap (view (#info . #spelltarget)) $ safedoublexclam (fst $ getspells spelltype spelllevel ((view (#status) . NE.head) (view (#runCharacters) x))) (newint)) (setSpellChoice (newint) x)
  where
    menu = view (#runGameMenu) x
    spelltype
      | isSpellsMenu menu = runClass menu
      | otherwise = Nothing
    spelllevel
      | isSpellsMenu menu = runLevel menu
      | otherwise = Nothing
spellchoiceMenu _ w = return w

setSpellLevel :: Maybe Int -> World -> IO World
setSpellLevel i y = return $ over (#runGameMenu) (go i) y
  where
    (SpellsMenu spelltype _ _ _) = view (#runGameMenu) y
    go :: Maybe Int -> GameMenu -> GameMenu
    go i2 (SpellsMenu x _ z _) =  (SpellsMenu x i2 z (["Choose Spell"] ++ spelltext))
    go _ x = x
    spelltext = ((go2 <$>) . indexed) (fst $ getspells spelltype i ((view (#status) . NE.head . view (#runCharacters)) y))
    go2 (a1, b1) = show a1 ++ ": " ++ (show $ view (#info . #name) b1)

setSpellChoice :: Maybe Int -> World -> World
setSpellChoice i y = over (#runGameMenu) (go i) $ y
  where
    go :: Maybe Int -> GameMenu -> GameMenu
    go i2 (SpellsMenu x z _ _) =  (SpellsMenu x z i2 ["Spell Chosen"])
    go _ x = x

spelltargets2 :: Maybe SpellTarget -> World -> IO World
spelltargets2 a world
    | (a == (Just Caster)) || isAllies a || isEnemies a || isBurst a = return $ (set (#runGameMode) (CastSpell False) . set (#runTarget) []) world
    | isSingleTarget a = return $ (set (#runGameMode) SingleSpellTargetMode . set (#runTarget) []) world
    | otherwise = return $ (set (#runGameMode) SpellTargetMode . set (#runTarget) []) world

spelltargets :: Maybe SpellTarget -> NonEmpty Character -> IO [Maybe Int]
spelltargets a b
  | (a == (Just Caster)) || isAllies a || isEnemies a = return []
  | isSingleTarget a = do
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

castSpell :: World -> IO World
castSpell oldworld = do
  g <- newStdGen
  let a = view (#runCharacters) oldworld
  let targets = if (snd $ fullcastspell (runMaze oldworld) [] spelltype spelllevel chosenspell g a) == [] then view (#runTarget) oldworld else snd $ fullcastspell (runMaze oldworld) [] spelltype spelllevel chosenspell g a
  let (caster :| newcharacters) = fst $ fullcastspell (runMaze oldworld) targets spelltype spelllevel chosenspell g a
  let spell = case chosenspell !? (fst $ getspells spelltype spelllevel (view (#status) . NE.head $ a)) of
                  Nothing -> fst $ runState ((rollingspelldice ~>) ~> ((take (length targets) . repeat . Just . go5 . w2sp Nothing Nothing . view (#status) . NE.head $ a))) g
                  _ -> fst $ runState ((rollingspelldice ~>) ~> ((take (length targets) . repeat . (chosenspell !?)) (fst $ getspells spelltype spelllevel (view (#status) $ NE.head a)))) g
  let gametext = [] : ([go3 spell] ++ (simpleshow4list $ catMaybes spell) ++ (catMaybes $ fmap (fmap detailedview) $ go2 targets (caster :| newcharacters)))
  index'' (catMaybes $ go4 (caster :| newcharacters) (function targets))
  newworld <- dispelmagicio targets (spellcomparebool "Dispel Magic") ((set (#runCharacters) ((over (#status . #ap) (\x -> x - (ceiling $ distanceCheck oldworld + (int2Float spellapcost))) caster) :| newcharacters) . set (#runGameMode) Map . over (#runGameText) (\x -> x ++ [stringinfo] ++ gametext)) oldworld)
  let newworld2 = ironwall (spellcomparebool "Wall of Steel") newworld
  ioEndTurn newworld2
    where
      go5 :: Spell [Int] -> Spell [Int]
      go5 spelly = over (#mdamdice) (fmap $ over (#ddtarget) (fmap $ over (#bonus) (if view (#runGameMode) oldworld == CastSpell True then (\x -> x + 3) else id))) spelly
      go4 :: NonEmpty a -> [Maybe Int] -> [Maybe a]
      go4 _ [] = []
      go4 ns (b : bs) = ns !!? b : go4 ns bs
      stringinfo = snd $ getspells spelltype spelllevel (view (#status) . NE.head . runCharacters $ oldworld)
      spellapcost :: Int
      spellapcost = case chosenspell !? (fst $ getspells spelltype spelllevel (view (#status) . NE.head . runCharacters $ oldworld)) of
        Nothing -> 2
        Just y -> view (#info . #apcost) y
      spellcomparebool name = case chosenspell !? (fst $ getspells spelltype spelllevel (view (#status) . NE.head . runCharacters $ oldworld)) of
        Nothing -> False
        Just y -> view (#info . #name) y == name
      go3 spell2 = case catMaybes spell2 of
        [] -> "Name"
        (sp : sps) -> view (#info . #name) sp
      go2 :: [Maybe Int] -> NonEmpty Character -> [Maybe Character]
      go2 [] _ = []
      go2 (x : xs) y = (y !!? x) : go2 xs y
      (spelltype, spelllevel, chosenspell) = go oldworld
      go world2
        | isSpellsMenu menu = (x, y, z)
        | otherwise = (Nothing, Nothing, Nothing)
        where
          menu = (view (#runGameMenu) world2)
          x = runClass menu
          y = runLevel menu
          z = runSpell menu

alldispelleffects :: [String]
alldispelleffects = go <$> (indexed dispelllist)
  where
    go (a1, b1) = show a1 ++ ": " ++ (show b1) ++ ", "

dispelmagicio :: [Maybe Int] -> Bool -> World -> IO World
dispelmagicio _ False world = return world
dispelmagicio targets True world = do
                            _ <- putStrNLn alldispelleffects
                            int1 <- getInt
                            let statuseffect = case safedoublexclam dispelllist int1 of
                                                  Nothing -> Invisible
                                                  Just y -> y
                            let newchars = safemodifyats (over (#status) (t2ix statuseffect (Absent Nothing [] Nothing))) targets (runCharacters world)
                            return . over (#runGameText) (++ [show statuseffect]) $ set (#runCharacters) newchars world

ironwall :: Bool -> World -> World
ironwall False world = world
ironwall True world = set (#runLocations) [] $ over (#runMaze) (addtomaze (view (#runLocations) world) (Just 15) (Just 180)) world

moveCharacter :: (Character -> Bool) -> Character -> Location -> World -> IO World
moveCharacter func char loc oldworld@(World chars@(x :| y) ml z z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16) = 
  if (find (char ==) y == Nothing && char /= x) || repelledbool then (return $ over (#runCharacters) (set (#mouseSelect) False <$>) oldworld) else
    do
    g <- newStdGen
    let roll1 = fst . fst $ skillcheck g (#skills . #perception) (#wisdom) (#skills . #perception) char
    putStrLn . show $ roll1
    world <- triggerAreaTrap paths oldworld
    newworld <- aoomovement char (delete loc paths) world
    return $ over (#runCharacters) (nemodifywhile changeLocationFunction (char ==) . nemodifywhile visionfunc (char /=)) .
      set (#explore) (if func char && temporary (view (#status . #team) char) == Ally 
              then (nub $ z11 ++ (fst $ charactervisionfield (brighttime z14 z5) z12 (illuminatechar z12 . set (#runLocation) loc $ char) chars)) 
              else z11) .
      over (#runMaze) (if not z8 && func char && temporary (view (#status . #team) char) == Ally 
              then searchmaze roll1 (fst $ charactervisionfield (brighttime z14 z5) z12 (illuminatechar z12 . set (#runLocation) loc $ char) chars) 
              else id) $
      (if z8 && func char then newworld else world)
      where
        visionfunc x = set (#immediateVision) (fst $ charactervisionfield (brighttime z14 z5) z12 (illuminatechar z12 x) (nemodifywhile changeLocationFunction (char ==) chars)) x
        repelledbool = (isActive. temporary $ view (#status . #effects . #repelled) char) && (repelledchar /= char) && 
                        ((Just $ eucliddistance (view (#runLocation) repelledchar) loc) < (view (#modifyer) . temporary $ view (#status . #effects . #repelled) char)) &&
                        (eucliddistance (view (#runLocation) repelledchar) loc < (eucliddistance (view (#runLocation) repelledchar) (view (#runLocation) char)))
        repelledchar = case find (\x -> view (#name) x == (view (#character) . temporary $ view (#status . #effects . #repelled) char)) y of
                        Nothing -> char
                        Just y -> y
        changelocfunction (g:|gs) = if char == g then set (#runLocation) loc g :| gs else g :| gs
        paths = linealgo (view (#runLocation) char) loc
        distance = eucliddistance (view (#runLocation) char) loc
        changeLocationFunction = if not z8 then illuminatechar z12 . set (#mousePoint) (0.0,0.0) . 
                                  set (#mouseSelect) False  . set (#runLocation) loc .
                                  (\x -> set (#immediateVision) (fst $ charactervisionfield (brighttime z14 z5) z12 (illuminatechar z12 . set (#runLocation) loc $ x) chars) x)
                                  else
                                  illuminatechar z12 . over (#status . #movementtotal) (+ (5 * distance)) . 
                                  set (#mousePoint) (0.0,0.0) . set (#mouseSelect) False . set (#runLocation) loc .
                                  (\x -> set (#immediateVision) (fst $ charactervisionfield (brighttime z14 z5) z12  (illuminatechar z12 . set (#runLocation) loc $ x) chars) x)

triggerAreaTrap :: [Location] -> World -> IO World
triggerAreaTrap loc world = do
  g <- newStdGen
  let (newworld, results) = MAP.foldrWithKey (\x y z -> (triggertrap g loc x y z)) (world, []) maze
  return . over (#runGameText) (\x -> x ++ (deleteAll [] results)) $ newworld
  where
    chars = runCharacters world
    maze = runMaze world
    triggertrap :: StdGen -> [Location] -> Location -> MazeNode -> (World, [String]) -> (World, [String])
    triggertrap g' loc1 loc2 mnode (world, str) 
      | (not . hasTrap $ mnode) = (world, str)
      | not . any (\ x -> x `elem` (getTrapLoc mnode)) $ loc1 = (world, str)
      | otherwise = (over (#runMaze) (MAP.adjust removeTrap loc2) . set (#runCharacters) (set (#runLocation) oldlocation newchar :| newchars) $ world, "Trap Triggered!" : newstring)
      where
        (x :| xs) = (runCharacters world)
        oldlocation = view (#runLocation) x
        firsttriggerlocation = case find (\x -> x `elem` (getTrapLoc mnode)) loc1 of
          Nothing -> oldlocation
          Just y -> y
        (newchar :| newchars,newstring) = fmap go2 $ fullcastAreaspell (getTrap mnode) maze g' firsttriggerlocation (set (#runLocation) firsttriggerlocation x :| xs)
        go2 ss
          | ss == [] = str
          | otherwise = str ++ ss 

inputHandlerMap :: Event -> World -> IO World
inputHandlerMap (EventKey (SpecialKey KeyUp) Down (Modifiers Up Down Up) _) (World z1 ml (x,y) z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16) = return $ World z1 ml (x, y + 35) z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16
inputHandlerMap (EventKey (SpecialKey KeyDown) Down (Modifiers Up Down Up) _) (World z1 ml (x,y) z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16) = return $ World z1 ml (x, y - 35) z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16
inputHandlerMap (EventKey (SpecialKey KeyRight) Down (Modifiers Up Down Up) _) (World z1 ml (x,y) z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16) = return $ World z1 ml (x + 35, y) z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16
inputHandlerMap (EventKey (SpecialKey KeyLeft) Down (Modifiers Up Down Up) _) (World z1 ml (x,y) z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16) = return $ World z1 ml (x - 35, y) z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16

inputHandlerMap (EventKey (SpecialKey KeyUp) Down _ _) world@(World (x :| y) ml z z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16)
  | z8 && (meleedistance (view (#runStartLocation) x) ((\(x', y') -> (x', y' + 1)) $ view (#runLocation) x) > 1) = return world
  | z8 && (meleedistance (view (#runStartLocation) x) ((\(x', y') -> (x', y' + 1)) $ view (#runLocation) x) <= 1) = return $ set (#runCharacters) (over (#runLocation) (\(x', y') -> (x', y' + 1)) x :| y) world
  | isWall wallnorth || isDoorClosed wallnorth || isWall wallsouth || isDoorClosed wallsouth = return world
  | otherwise = moveCharacter (const True) x ((\(x', y') -> (x', y' + 1)) $ view (#runLocation) x) world
    where
      wallnorth = view (#runNorth) (MAP.findWithDefault (emptyblock Dark) (view (#runLocation) x) z12)
      wallsouth = view (#runSouth) (MAP.findWithDefault (emptyblock Dark) ((\(x', y') -> (x', y' + 1)) $ view (#runLocation) x) z12)
inputHandlerMap (EventKey (SpecialKey KeyDown) Down _ _) world@(World (x :| y) ml z z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16)
  | z8 && (meleedistance (view (#runStartLocation) x) ((\(x', y') -> (x', y' - 1)) $ view (#runLocation) x) > 1) = return (World (x :| y) ml z z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16)
  | z8 && (meleedistance (view (#runStartLocation) x) ((\(x', y') -> (x', y' - 1)) $ view (#runLocation) x) <= 1) = return $ set (#runCharacters) (over (#runLocation) (\(x', y') -> (x', y' - 1)) x :| y) world
  | isWall wallnorth || isDoorClosed wallnorth || isWall wallsouth || isDoorClosed wallsouth = return (World (x :| y) ml z z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16)
  | otherwise = moveCharacter (const True) x ((\(x', y') -> (x', y' - 1)) $ view (#runLocation) x) world
    where
      wallnorth = view (#runSouth) (MAP.findWithDefault (emptyblock Dark) (view (#runLocation) x) z12)
      wallsouth = view (#runNorth) (MAP.findWithDefault (emptyblock Dark) ((\(x', y') -> (x', y' - 1)) $ view (#runLocation) x) z12)
inputHandlerMap (EventKey (SpecialKey KeyRight) Down _ _) world@(World (x :| y) ml z z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16)
  | z8 && (meleedistance (view (#runStartLocation) x) ((\(x', y') -> (x' + 1, y')) $ view (#runLocation) x) > 1) = return world
  | z8 && (meleedistance (view (#runStartLocation) x) ((\(x', y') -> (x' + 1, y')) $ view (#runLocation) x) <= 1) = return $ set (#runCharacters) (over (#runLocation) (\(x', y') -> (x' + 1, y')) x :| y) world
  | isWall wallnorth || isDoorClosed wallnorth || isWall wallsouth || isDoorClosed wallsouth = return world
  | otherwise = moveCharacter (const True) x ((\(x', y') -> (x' + 1, y')) $ view (#runLocation) x) world
    where
      wallnorth = view (#runEast) (MAP.findWithDefault (emptyblock Dark) (view (#runLocation) x) z12)
      wallsouth = view (#runWest) (MAP.findWithDefault (emptyblock Dark) ((\(x', y') -> (x' + 1, y')) $ view (#runLocation) x) z12)
inputHandlerMap (EventKey (SpecialKey KeyLeft) Down _ _) world@(World (x :| y) ml z z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16)
  | z8 && ( meleedistance (view (#runStartLocation) x) ((\(x', y') -> (x' - 1, y')) $ view (#runLocation) x) > 1) = return world
  | z8 && (meleedistance (view (#runStartLocation) x) ((\(x', y') -> (x' - 1, y')) $ view (#runLocation) x) <= 1) = return $ set (#runCharacters) (over (#runLocation) (\(x', y') -> (x' - 1, y')) x :| y) world
  | isWall wallnorth || isDoorClosed wallnorth|| isWall wallsouth || isDoorClosed wallsouth = return world
  | otherwise = moveCharacter (const True) x ((\(x', y') -> (x' - 1, y')) $ view (#runLocation) x) world
    where
      wallnorth = view (#runWest) (MAP.findWithDefault (emptyblock Dark) (view (#runLocation) x) z12)
      wallsouth = view (#runEast) (MAP.findWithDefault (emptyblock Dark) ((\(x', y') -> (x' - 1, y')) $ view (#runLocation) x) z12)

inputHandlerMap (EventKey (MouseButton WheelUp) Down _ _) (World x ml y z z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16) = return $ World x ml y (1.1 * z) z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16
inputHandlerMap (EventKey (MouseButton WheelDown) Down _ _) (World x ml y z z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16) = return $ World x ml y (0.9 * z) z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16
inputHandlerMap (EventKey (MouseButton LeftButton) Up (Modifiers Up Down Up) (a1, a2)) world@(World (x :| y) ml z z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16) = 
  if find (view (#mouseSelect)) y == Nothing && not (view (#mouseSelect) x) then return world
  else return $ set (#runCharacters) (nemodifywhile (set (#runLocation) mouseLocation . set (#mouseSelect) False) (view (#mouseSelect)) (x :| y)) world
  where
    (b1, b2) = z
    mouseLocation = (round $ (a1 - b1) / z2, round $ (a2 - b2) / z2)
inputHandlerMap (EventKey (MouseButton LeftButton) Down _ (a1, a2)) (World (x :| y) ml z z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16) = return $ World (go3 (go (x :| y))) ml z z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16
  where
    (b1, b2) = z
    mouseLocation = (round $ (a1 - b1) / z2, round $ (a2 - b2) / z2)
    changeLocationFunction = (set (#mouseSelect) True . set (#mousePoint) (a1, a2))
    go (a:|as) = if inhabits a mouseLocation then (changeLocationFunction a :| as) else a :| (go2 as)
    go2 [] = []
    go2 (a' : as') = if inhabits a' mouseLocation then (changeLocationFunction a' : as') else a' : (go2 as')
    go3 (a22 :| a2s)
      | z8 = (a22 :| a2s)
      | inhabits a22 mouseLocation = (a22 :| a2s)
      | otherwise =  go4 [] a22 a2s
    go4 :: [Character] -> Character -> [Character] -> NonEmpty Character
    go4 [] a2' [] =  a2' :| []
    go4 dcs'' a2'' [] = if inhabits a2'' mouseLocation then a2'' :| dcs'' else (head dcs'') :| ((tail dcs'') ++ [a2''])
    go4 dcs dc (c:cs) = if view (#runLocation) dc == mouseLocation then  dc :| (dcs ++ (c : cs)) else go4 (dcs ++ [dc]) c cs
inputHandlerMap (EventKey (MouseButton LeftButton) Up _ (a1, a2)) world@(World (x :| y) ml z z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16) = 
  if find (view (#mouseSelect)) y == Nothing && not (view (#mouseSelect) x) then return world
  else moveCharacter (x ==) chara mouseLocation world
  where
    (b1, b2) = z
    mouseLocation = (round $ (a1 - b1) / z2, round $ (a2 - b2) / z2)
    chara = if view (#mouseSelect) x then x
      else case find (view (#mouseSelect)) y of
        Nothing -> x
        Just y' -> y' 
inputHandlerMap (EventMotion (a1, a2)) (World (x :| y) ml z z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16)
  | bool = return $ World (x :| y) (oldlocation, (a1, a2), bool) z z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16
  | otherwise = return $ World (go <$> (x :| y)) ml z z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16
  where
    (oldlocation, _, bool) = ml
    mouseLocation = (a1 , a2)
    changeLocationFunction = (set (#mousePoint) mouseLocation)
    go x'
      |view (#mouseSelect) x' == True = changeLocationFunction x'
      |otherwise = x' 
inputHandlerMap (EventKey (MouseButton RightButton) Down _ (a1, a2)) world@(World xs (b1, b2, b3) z z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16)
  | hasVisTrap mnode = return world
  | otherwise = return $ World xs ((a1, a2),(a1, a2), True) z z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16
  where
    (b1, b2) = z
    mouseLocation = (round $ (a1 - b1) / z2, round $ (a2 - b2) / z2)
    mnode = MAP.lookup mouseLocation z12
inputHandlerMap (EventKey (MouseButton RightButton) Up _ (a1, a2)) world@(World xs ((b1, b2),d, e) (c1, c2) z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16)
  | hasVisTrap mnode = do
    g <- newStdGen
    let roll1 = fst . fst $ skillcheck g (#skills . #engineering) (#dexterity) (#skills . #engineering) (NE.head xs)
    if getVisTrapDC mnode <= roll1 then 
      return . over (#runGameText) (++ ["Success: " ++ show roll1 ++ " vs DC " ++ show (getVisTrapDC mnode)]) . over (#runMaze) (MAP.adjust removeTrap mouseLocation) $ world else 
        return $ over (#runGameText) (++ ["Failure: " ++ show roll1]) world
  | otherwise = return $ World xs ((b1, b2),d, False) (c1 + a1 - b1, c2 + a2 - b2) z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16
  where
    mouseLocation = (round $ (a1 - c1) / z2, round $ (a2 - c2) / z2)
    mnode = MAP.lookup mouseLocation z12
inputHandlerMap (EventKey (SpecialKey KeyEnter) Down _ _) x = return . over (#runCharacters) go $ x
  where
    go (c :| cs) = snoc cs c
      where
        snoc :: [Character] -> Character -> NonEmpty Character
        snoc [] b = b :| []
        snoc (a : as) b = a :| (as ++ [b])

inputHandlerMap (EventKey (SpecialKey KeyDelete) Down _ _) x = return $ over (#runCharacters) (\y -> snd $ runState deleteCharacterHead y) x
inputHandlerMap (EventKey (Char 'a') Down _ _) x = return $ (set (#runGameMode) TargetMode . set (#runTarget) []) x
inputHandlerMap (EventKey (Char 'd') Down _ _) (World game@(x:|xs) ml z z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16) = do
  g <- newStdGen
  let roll1 = Just . fst . fst . skillcheck g (#skills . #engineering) (#dexterity) (#skills . #engineering) $ x
  let visionfunction char = (set (#immediateVision) (fst $ charactervisionfield (brighttime z14 z5) (fst $ openlocaldoors (brighttime z14 z5) roll1 (runLocation x) z12) (illuminatechar (fst $ openlocaldoors (brighttime z14 z5) roll1 (runLocation x) z12) char) game) char)
  return $ World (fmap visionfunction game) ml z z2 z3 z4 z5 z6 
    (z7 ++ (deleteAll [] . snd $ openlocaldoors (brighttime z14 z5) roll1 (runLocation x) z12)) 
    z8 z9 z10 
    (if temporary (view (#status . #team) x) == Ally then (nub $ z11 ++ (fst $ charactervisionfield (brighttime z14 z5) (fst $ openlocaldoors (brighttime z14 z5) roll1 (runLocation x) z12) (illuminatechar (fst $ openlocaldoors (brighttime z14 z5) roll1 (runLocation x) z12) x) (x:|xs))) else z11) 
    (fst $ openlocaldoors (brighttime z14 z5) roll1 (runLocation x) z12) z13 z14 z15 z16
inputHandlerMap _ w = return w

inputHandlerTarget :: Event -> World -> IO World
inputHandlerTarget (EventKey (MouseButton LeftButton) Down _ x) y = do
  g <- newStdGen
  let isareabool = if snd (fst $ runState (getAttack True mouseLocation (getmenuchoice y) y g) (runCharacters y)) == [] then False else fst . view (#runF . #info . #spellproxy) . head $ snd (fst $ runState (getAttack True mouseLocation (getmenuchoice y) y g) (runCharacters y))
  if isareabool then chooseSpellAttackTarget g x y else chooseTarget x y
    where 
      (a1, a2) = x
      (b1, b2) = runGlobalOffSet y
      mouseLocation = (round $ (a1 - b1) / (runGlobalZoom y), round $ (a2 - b2) / (runGlobalZoom y))
inputHandlerTarget (EventKey (MouseButton RightButton) Down _ _) y = cancelTarget y
inputHandlerTarget _ w = return w

inputEquipmentHandlerTarget :: Event -> World -> IO World
inputEquipmentHandlerTarget (EventKey (MouseButton LeftButton) Down _ x) y = chooseEquipmentTarget x y
inputEquipmentHandlerTarget (EventKey (MouseButton RightButton) Down _ _) y = cancelTarget y
inputEquipmentHandlerTarget _ w = return w

inputHandlerSpellTarget :: Event -> World -> IO World
inputHandlerSpellTarget (EventKey (MouseButton LeftButton) Down _ x) y = (chooseSpellTarget x y)
inputHandlerSpellTarget (EventKey (MouseButton RightButton) Down _ _) y = cancelTarget y
inputHandlerSpellTarget _ w = return w

inputHandlerSpellTargets :: Event -> World -> IO World
inputHandlerSpellTargets (EventKey (MouseButton LeftButton) Down _ x) y = chooseSpellTargets x y
inputHandlerSpellTargets (EventKey (SpecialKey KeyEnter) Down _ _) y = return $ set (#runGameMode) (CastSpell False) y
inputHandlerSpellTargets (EventKey (MouseButton RightButton) Down _ _) y = cancelTarget y
inputHandlerSpellTargets _ w = return w

inputHandlerCombatTargets :: Event -> World -> IO World
inputHandlerCombatTargets (EventKey (MouseButton LeftButton) Down _ x) y = chooseCombatTargets x y
inputHandlerCombatTargets (EventKey (SpecialKey KeyEnter) Down _ _) y = return $ set (#runGameMode) StartCombat y
inputHandlerCombatTargets (EventKey (MouseButton RightButton) Down _ _) y = cancelTarget y
inputHandlerCombatTargets _ w = return w

inputHandlerDMMode :: Event -> World -> IO World
inputHandlerDMMode (EventKey (MouseButton LeftButton) Down _ x) y = choosePolygonPoints x y
inputHandlerDMMode (EventKey (MouseButton RightButton) Down (Modifiers Up Down Up) loc@(a1,a2)) world@(World (x :| y) ml z z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16) = do
  newworld <- addtrapsandtreasures mouseLocation world
  return newworld
  where    
    (b1, b2) = z
    mouseLocation = (round $ (a1 - b1) / z2, round $ (a2 - b2) / z2)  
inputHandlerDMMode (EventKey (MouseButton RightButton) Down _ loc@(a1,a2)) world@(World (x :| y) ml z z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16) = 
  return $ over (#runMaze) (either (alterlocation (brighttime z14 z5) mouseLocation mint) (addlight (brighttime z14 z5) mouseLocation) (getPortal z3)) world
  where    
    (b1, b2) = z
    mouseLocation = (round $ (a1 - b1) / z2, round $ (a2 - b2) / z2)
    (c1, c2) = cellCenter $ locationToCoords z z2 mouseLocation
    deltax = abs $ a1 - c1
    deltay = abs $ a2 - c2
    mint :: Maybe Int
    mint
      | deltay >= deltax && a2 >= c2 = Just 1
      | deltay >= deltax = Just 3
      | a1 >= c1 = Just 2
      | otherwise = Just 4
inputHandlerDMMode (EventKey (SpecialKey KeyEnter) Down _ _) y = return . set (#runLocations) [] . 
                                                                  over (#runGameText) (++ [show . finddirections $ view (#runLocations) y]) $ 
                                                                  over (#runMaze) (addtomaze (view (#runLocations) y) Nothing Nothing) y
inputHandlerDMMode (EventKey (Char 'c') Down _ _) y = return $ over (#runGameMode) advancePortal y
inputHandlerDMMode (EventKey (Char 'f') Down _ _) y = return $ over (#runMaze) removelights y
inputHandlerDMMode (EventKey (Char '+') Down _ _) y = return $ over (#runGameMode) advanceDC y
inputHandlerDMMode (EventKey (Char '-') Down _ _) y = return $ over (#runGameMode) regressDC y
inputHandlerDMMode _ w = return w

trapandtreasurelist :: [Object]
trapandtreasurelist = [HiddenObject "Disturbed Ground" 10 [], 
                      HiddenObject "Disturbed Ground 15" 15 [], 
                      Object "Breastplate" [Left . Left $ ringofswimmingleft],
                      Object "Scroll" [],
                      HiddenObject "Vial" 20 [Left . Right $ curewounds 1],
                      HiddenObject "Staff" 12 [],
                      HiddenObject "Secret Panel" 20 [],
                      HiddenObject "Man in Bushes" 16 [],
                      HiddenTrap "Falling Pit Trap" 20 20 [(17,8),(17,9),(18,8),(18,9)] (fallpittrap Nothing Nothing emptystatus)]

addtrapsandtreasures :: Location -> World -> IO World
addtrapsandtreasures loc world = do
  putStrLn $ indexlist trapandtreasurelist
  int <- getInt
  let mobject = int !? trapandtreasurelist
  return $ over (#runMaze) (MAP.alter (go mobject) loc) world
  where
    maze = runMaze world
    brightness = brighttime (runBrightness world) (runTime world)
    go :: Maybe Object -> Maybe MazeNode -> Maybe MazeNode
    go mo mmn = case mmn of
      Nothing -> Just $ set (#runObject) mo (emptyblock brightness)
      Just y -> Just $ set (#runObject) mo y

getInt :: IO (Maybe Int)
getInt = do
  linea <- getLine
  go linea
    where
      go lineb
        | lineb == [] = return Nothing
        | lineb == "" = return Nothing
        | head lineb == '-' && all isDigit (tail lineb) = return . Just . negate . read . tail $ lineb
        | foldr (&&) True (isDigit <$> lineb) = return (Just $ read lineb)
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

index :: NonEmpty Character -> String
index (c :| cs) = "0: " ++ (showc c) ++ "      " ++ (go cs)
  where
    go [] = []
    go (ec : ecs) = show (length (c :| cs) - (length (ec : ecs))) ++ ": " ++ (showc ec) ++ "      " ++ (go ecs)

index' :: NonEmpty Character -> IO ()
index' (c :| cs) = do 
  putStrLn $ "0: " ++ (showc c)
  go 1 cs
  where
    go :: Int -> [Character] -> IO ()
    go _ [] = return ()
    go int (ec : ecs) = do 
      putStrLn $ show int ++ ": " ++ (showc ec)
      go (int + 1) ecs

index'' :: [Character] -> IO ()
index'' [] = return ()
index'' (ec : ecs) = do 
      putStrLn $ detailedview ec
      index'' ecs

indexlist :: Show a => [a] -> String
indexlist [] = ""
indexlist (c : cs) = "0: " ++ (show c) ++ "      " ++ (go cs)
  where
    go [] = []
    go (ec : ecs) = show (length (c :| cs) - (length (ec : ecs))) ++ ": " ++ (show ec) ++ "      " ++ (go ecs) 

-- ChooseAttackBasedonCharacter
--"Choose Attack: 1) F Weapon, 2) S Weapon, 3) Monster Attack, 4) S Monster Attack, 5) Combat Maneuver" 
-- "6) FW Sneak Attack, 7) SW Sneak Attack, 8) FW Distance Shot, 9) SW Distance Shot, 10) Special Attack, 12) Charge"
-- "13) Flank (full attack), 14) Monster Pounce, 15) Monster Charge, 16) Monster Flank"
--

chooseTarget :: Point -> World -> IO World
chooseTarget point world
  | target char == Nothing = chooseMapTarget point world
  | otherwise = do
    g1 <- newStdGen
    let attacks = go6 char g1
    case attacks of
      [] -> do
        putStrLn $ show attacks
        return $ over (#runGameText) ( ++ ["Out of Range"]) world
      (a:_) ->  if (5 * meleedistancechar (NE.head char) targetchar <= (view (#info . #increment) a)) then return newworld
          else return $ over (#runGameText) ( ++ ["Out of Range1"]) world
  where
    go6 :: NonEmpty Character -> StdGen -> [Attack Int Int]
    go6 as g = blah
        where
          (_, blah) = fmap (fmap (fmap runRoll)) $ fmap (fmap runF) . fst $ runState (getAttack True mouseLocation (getmenuchoice world) world g) as
    World char _ p zoom _ _ _ _ _ _ _ _ _ _ _ _ _ _= world
    (a1, a2) = point
    (b1, b2) = p
    newworld = set (#runTarget) ([target char]) . set (#runGameMode) Combat $ world
    mouseLocation = (round $ (a1 - b1) / zoom, round $ (a2 - b2) / zoom)   
    targetchar = case char !!? (target char) of
      Nothing ->  (NE.head char)
      Just y -> y
    target (a:|as) = if inhabits a mouseLocation then Just 0 else go 1 as
      where
        go _ [] = Nothing
        go int (a':[]) = if inhabits a' mouseLocation then Just int else Nothing
        go int (a':as') = if inhabits a' mouseLocation then Just int else go (int + 1) as'

chooseMapTarget :: Point -> World -> IO World
chooseMapTarget point world
  | isempty mint tile && isempty (fmap (\x -> mod (x + 2) 4) mint) locationchange = return world
  | otherwise = do
    g1 <- newStdGen
    let attacks = go6 char g1
    let attacks2 = go5 char g1
    let newworld = over (#runGameText) 
                      (++ [showmmb (MAP.lookup mouseLocation $ alterlocation2 attacks2 mouseLocation mint (runMaze world)), 
                       showmmb (MAP.lookup locationchange' $ alterlocation2 attacks2 mouseLocation mint (runMaze world))]) $ 
                    over (#runMaze) (alterlocation2 attacks2 mouseLocation mint) world
    let adjustvisionfield (x:|xs) = set (#immediateVision) (fst $ charactervisionfield (brighttime z14 z5) (view (#runMaze) newworld) (illuminatechar (view (#runMaze) newworld) x) (x:|xs)) x:|xs
    case attacks of
      [] -> do
        putStrLn $ show attacks
        return $ over (#runGameText) ( ++ ["Out of Range"]) world
      (a:_) ->  if (5 * eucliddistance (view (#runLocation) $ NE.head char) mouseLocation <= (view (#info . #increment) a)) 
          then ioEndTurn . over (#runCharacters) adjustvisionfield $ set (#runGameMode) Map newworld
          else return $ over (#runGameText) ( ++ ["Out of Range1"]) world
  where
    showmmb argument = case argument of
      Nothing -> []
      Just z -> foldr (\x y -> y ++ ", " ++ showmb x) [] z
    showmb argument
      | isDoorOpen argument || argument == Hall = []
      | otherwise = show $ getHP argument 
    go6 :: NonEmpty Character -> StdGen -> [Attack Int Int]
    go6 as g = blah
        where
          (_, blah) = fmap (fmap (fmap runRoll)) $ fmap (fmap runF) . fst $ runState (getAttack True mouseLocation (getmenuchoice world) world g) as
    go5 :: NonEmpty Character -> StdGen -> [Action (Attack  Int) Roll]
    go5 as g = blah
        where
          (_, blah) = fst $ runState (getAttack True mouseLocation (getmenuchoice world) world g) as
    World char _ p zoom _ _ z5 _ _ _ _ _ _ _ _ z14 _ _= world
    (a1, a2) = point
    (b1, b2) = p
    (c1, c2) = cellCenter $ locationToCoords p zoom mouseLocation
    deltax = abs $ a1 - c1
    deltay = abs $ a2 - c2
    mint :: Maybe Int
    mint
      | deltay >= deltax && a2 >= c2 = Just 1
      | deltay >= deltax = Just 3
      | a1 >= c1 = Just 2
      | otherwise = Just 4
    locationchange' = case mint of
      (Just 1) -> ((\(x,y) -> (x, y + 1)) mouseLocation)
      (Just 2) -> ((\(x,y) -> (x + 1, y)) mouseLocation)
      (Just 3) -> ((\(x,y) -> (x, y - 1)) mouseLocation)
      _ -> ((\(x,y) -> (x - 1, y)) mouseLocation)
    locationchange = MAP.lookup locationchange' (runMaze world)
    mouseLocation = (round $ (a1 - b1) / zoom, round $ (a2 - b2) / zoom)
    tile = MAP.lookup mouseLocation (runMaze world)
    isempty mintroute mblock = case mblock of
      Nothing -> True
      Just block -> (view (maybeInt2Lens mintroute) block == Hall || (isDoorOpen $ view (maybeInt2Lens mintroute) block))

chooseEquipmentTarget :: Point -> World -> IO World
chooseEquipmentTarget point world = do
    putStrLn . show . length $ textterminal1
    putStrLn . (\x -> x ++ " int1") . show $ (target char)
    return newworld
  where
    World char _ p zoom _ _ _ _ _ _ _ _ _ _ _ _ _ _= world
    (a1, a2) = point
    (b1, b2) = p
    newworld = go2
    mouseLocation = (round $ (a1 - b1) / zoom, round $ (a2 - b2) / zoom)    
    target (a:|as) = if inhabits a mouseLocation then Just 0 else go 1 as
      where
        go _ [] = Nothing
        go int (a':[]) = if inhabits a' mouseLocation then Just int else Nothing
        go int (a':as') = if inhabits a' mouseLocation then Just int else go (int + 1) as'
    go2
      | target char == Nothing = set (#runTarget) ([]) . set (#runGameMenu) (TakeEquipmentMenu textterminal1) . set (#runGameMode) (EquipmentTargeting . Just $ mouseLocation) $ world
      | otherwise = set (#runTarget) ([target char]) . set (#runGameMenu) (TakeEquipmentMenu textterminal1) . set (#runGameMode) (EquipmentTargeting Nothing) $ world
    textterminal1
      | target char == Nothing = go1 <$> (indexed . mObjectToEquip . MAP.lookup mouseLocation . runMaze $ world)
      | otherwise = go1 <$> (indexed . mcharToEquip $ char !!? (target char))
    go1 :: (Int, Either (Either (MagicItem Int) SpellTemplate) (Either (Armour Int) Weapon)) -> String
    go1 (a1, b) = show a1 ++ ": " ++ (either (either miname show) (either aname wname) b) ++ ", "
    wname weapon = view (#name) weapon
    aname armour =  view (#name) armour
    miname mitem = view (#name) mitem

chooseSpellTarget :: Point -> World -> IO World
chooseSpellTarget point world
  | getRange spelltarget < (5 * eucliddistance (view (#runLocation) (NE.head chars)) mouseLocation) = return $ over (#runGameText) (\x -> x ++ ["Out ofRange"]) world
  | target char == Nothing = return world
  | otherwise = return newworld
  where
    World char _ p zoom _ _ _ _ _ _ _ _ _ _ _ _ _ _= world
    (a1, a2) = point
    (b1, b2) = p
    newworld = set (#runTarget) ([target char]) . set (#runGameMode) (CastSpell False) $ world
    mouseLocation = (round $ (a1 - b1) / zoom, round $ (a2 - b2) / zoom)    
    target (a:|as) = if inhabits a mouseLocation then Just 0 else go 1 as
      where
        go _ [] = Nothing
        go int (a':[]) = if inhabits a' mouseLocation then Just int else Nothing
        go int (a':as') = if inhabits a' mouseLocation then Just int else go (int + 1) as'
    chars = view (#runCharacters) world
    (spelltype, spelllevel, chosenspell) = go1 world
    go1 world2
        | isSpellsMenu menu = (x, y, z)
        | otherwise = (Nothing, Nothing, Nothing)
        where
          menu = (view (#runGameMenu) world2)
          x = runClass menu
          y = runLevel menu
          z = runSpell menu
    spell :: Spell [Int]
    spell = case chosenspell !? (fst $ getspells spelltype spelllevel (view (#status) . NE.head $ chars)) of
      Nothing -> testspell
      Just y -> y
    spelltarget :: SpellTarget
    spelltarget = view (#info . #spelltarget) spell

chooseSpellTargets :: Point -> World -> IO World
chooseSpellTargets point world
  | getRange spelltarget < (5 * eucliddistance (view (#runLocation) (NE.head chars)) mouseLocation) = return $ over (#runGameText) (\x -> x ++ ["Out ofRange"]) world
  | view (#info . #name) spell == "Wall of Steel" = return $ newworld2
  | isMapArea . Just $ spelltarget = return . set (#runTarget) areatargets . set (#runGameMode) (CastSpell False) . over (#runMaze) putspellinmaze $ world
  | isAreaTarget . Just $ spelltarget = return . set (#runTarget) areatargets . set (#runGameMode) (CastSpell False) $ world
  | otherwise = return $ over (#runTarget) (deleteAll Nothing) newworld
  where
    go _ [] = Nothing
    go int (a':[]) = if inhabits a' mouseLocation && (not $ (Just int) `elem` oldtargets) then Just int else Nothing
    go int (a':as') = if inhabits a' mouseLocation && (not $ (Just int) `elem` oldtargets) then Just int else go (int + 1) as'
    World char _ p zoom _ oldtargets _ _ _ _ _ _ _ _ _ _ _ _= world
    (a:|as) = char
    (a1, a2) = point
    (b1, b2) = p
    newworld = over (#runTarget) (target :) world
    newworld2 = over (#runLocations) (mouseLocation :) world
    mouseLocation = (round $ (a1 - b1) / zoom, round $ (a2 - b2) / zoom)    
    target = if inhabits a mouseLocation && (not $ (Just 0) `elem` oldtargets) then Just 0 else go 1 as
    chars = view (#runCharacters) world
    (spelltype, spelllevel, chosenspell) = go1 world
    go1 world2
        | isSpellsMenu menu = (x, y, z)
        | otherwise = (Nothing, Nothing, Nothing)
        where
          menu = (view (#runGameMenu) world2)
          x = runClass menu
          y = runLevel menu
          z = runSpell menu
    putspellinmaze :: Maze -> Maze
    putspellinmaze maze = MAP.alter mmtmm mouseLocation maze
      where
        mmtmm :: Maybe MazeNode -> Maybe MazeNode
        mmtmm mmn = case mmn of
          Nothing -> Just $ set (#runspell) (Just ((prepareAreaSpell (view (#status) a) spell, spellduration))) (emptyblock Dark)
          Just y -> Just $ set (#runspell) (Just ((prepareAreaSpell (view (#status) a) spell, spellduration))) y
    spell :: Spell [Int]
    spell = case chosenspell !? (fst $ getspells spelltype spelllevel (view (#status) . NE.head $ chars)) of
      Nothing -> testspell
      Just y -> y
    spelltarget :: SpellTarget
    spelltarget = view (#info . #spelltarget) spell
    areatargets = indexfunction (\x -> view (#runLocation) x `elem` locationtargets) char
    locationtargets :: [Location]
    locationtargets
      | (isSphere . getArea $ spelltarget) || (isMapArea . Just $ spelltarget) = finalvisionfield (runMaze world) mouseLocation (getAreaRadius spelltarget)
      | (((Just Cone) == ). getArea $ spelltarget) = filter (\x -> x `elem` finalvisionfield (runMaze world) mouseLocation (getRange spelltarget)) $ 
        delete (view (#runLocation) a) $ 
        conealgo (view (#runLocation) a) mouseLocation
      | otherwise = filter (\x -> x `elem` finalvisionfield (runMaze world) mouseLocation (getRange spelltarget)) $ 
        delete (view (#runLocation) a) $ 
        linealgo (view (#runLocation) a) mouseLocation
    spellduration = case view (#meffect) spell of
      Nothing -> 1
      Just y -> case temporalduration $ view (#temporal) y of
        Nothing -> 1
        Just z -> z



chooseSpellAttackTarget :: StdGen -> Point -> World -> IO World
chooseSpellAttackTarget g point world
  | getRange spelltarget < (5 * eucliddistance (view (#runLocation) (NE.head chars)) mouseLocation) = return $ over (#runGameText) (\x -> x ++ ["Out ofRange"]) world
  | isMapArea . Just $ spelltarget = return . over (#runGameText) ( ++ potentialtext) . set (#runTarget) areatargets . set (#runGameMode) (CastSpell critcool)  . over (#runMaze) putspellinmaze $ world
  | isAreaTarget . Just $ spelltarget = return . over (#runGameText) ( ++ potentialtext) . set (#runTarget) areatargets . set (#runGameMode) (CastSpell critcool)  $ world
  | otherwise = return $ over (#runTarget) (deleteAll Nothing) newworld
  where
    isgrenadebool = if snd (fst $ runState (getAttack True mouseLocation (getmenuchoice world) world g) (runCharacters world)) == [] 
                  then False else snd . view (#runF . #info . #spellproxy) . head $ snd (fst $ runState (getAttack True mouseLocation (getmenuchoice world) world g) (runCharacters world))
    target = if inhabits a mouseLocation && (not $ (Just 0) `elem` oldtargets) then Just 0 else go 1 as
    go _ [] = Nothing
    go int (a':[]) = if inhabits a' mouseLocation && (not $ (Just int) `elem` oldtargets) then Just int else Nothing
    go int (a':as') = if inhabits a' mouseLocation && (not $ (Just int) `elem` oldtargets) then Just int else go (int + 1) as'
    World char _ p zoom _ oldtargets _ _ _ _ _ _ _ _ _ _ _ _= world
    (a:|as) = char
    (a1, a2) = point
    (b1, b2) = p
    newworld = over (#runTarget) (target :) world
    mouseLocation = (round $ (a1 - b1) / zoom, round $ (a2 - b2) / zoom) 
    potentialmouseLocation = if isgrenadebool then fst $ randomizelocation (runMaze world) a mouseLocation g else mouseLocation 
    potentialtext = if isgrenadebool then snd $ randomizelocation (runMaze world) a mouseLocation g else []
    critcool = take 1 potentialtext == ["Critical Hit"]
    chars = view (#runCharacters) world
    putspellinmaze :: Maze -> Maze
    putspellinmaze maze = MAP.alter mmtmm potentialmouseLocation maze
      where
        mmtmm :: Maybe MazeNode -> Maybe MazeNode
        mmtmm mmn = case mmn of
          Nothing -> Just $ set (#runspell) (Just ((prepareAreaSpell (view (#status) a) spell, spellduration))) (emptyblock Dark)
          Just y -> Just $ set (#runspell) (Just ((prepareAreaSpell (view (#status) a) spell, spellduration))) y
    spell :: Spell [Int]
    spell = w2sp Nothing Nothing $ view (#status) a
    spelltarget :: SpellTarget
    spelltarget = view (#info . #spelltarget) spell
    areatargets = if isgrenadebool then areatargetsdirecthit ++ indexfunction (\x -> view (#runLocation) x `elem` locationtargets) char else indexfunction (\x -> view (#runLocation) x `elem` locationtargets) char
    areatargetsdirecthit = indexfunction (\x -> view (#runLocation) x == potentialmouseLocation) char
    locationtargets :: [Location]
    locationtargets
      | (isSphere . getArea $ spelltarget) || (isMapArea . Just $ spelltarget) = finalvisionfield (runMaze world) potentialmouseLocation (getAreaRadius spelltarget)
      | (((Just Cone) == ). getArea $ spelltarget) = filter (\x -> x `elem` finalvisionfield (runMaze world) potentialmouseLocation (getRange spelltarget)) $ 
        delete (view (#runLocation) a) $ 
        conealgo (view (#runLocation) a) potentialmouseLocation
      | otherwise = filter (\x -> x `elem` finalvisionfield (runMaze world) potentialmouseLocation (getRange spelltarget)) $ 
        delete (view (#runLocation) a) $ 
        linealgo (view (#runLocation) a) potentialmouseLocation
    spellduration = case view (#meffect) spell of
      Nothing -> 1
      Just y -> case temporalduration $ view (#temporal) y of
        Nothing -> 1
        Just z -> z

randomizelocation :: Maze -> Character -> Location -> StdGen -> (Location, [String])
randomizelocation maze c loc g = (newloc, text)
  where
    packetinfo = "PLL " ++ (show $ length potenitallocations) ++ ",R " ++ (show $ runRoll direction) ++ ",C " ++ (show $ (Just $ runRoll direction) !? potenitallocations)
    locationifo = "PL " ++ (show $ potenitallocations)
    difficulty = if isActive . temporary $ view (#status . #effects . #deadlyaim) c then 25 else 15
    potenitallocations 
      | difficulty - arolltotal >= 5 = delete loc . filter (\x -> eucliddistance loc x >= 2) $ finalvisionfield maze loc 14
      | otherwise = delete loc $ finalvisionfield maze loc 7
    (aroll, _) = roll 20 g
    (direction, _) = roll (length potenitallocations) g
    locationdifference = (fst newlocation - fst loc, snd newlocation - snd loc)
    newlocation = case (Just $ (runRoll direction) - 1) !? potenitallocations of
      Nothing -> loc
      Just y -> y
    arolltotal = (view (#runRoll) aroll) + (temporary $ view (#status . #primclass . #bab) c) + (temporary . sum . view (#miscclass . #bab) . gatherbonuses . view (#status) $ c) + (view (#dexterity) . abilitybonuses . view (#status) $ c)
    (newloc, text)
      | (view (#runRoll) aroll) == 20 = (loc, ["Critical Hit"])
      | arolltotal >= difficulty = (loc, ["Hit"])
      | difficulty - arolltotal >= 5 = (newlocation, ["Bad Miss", show locationdifference])
      | otherwise = (newlocation, ["Miss", show locationdifference])

chooseCombatTargets :: Point -> World -> IO World
chooseCombatTargets point world = return $ over (#runTarget) (deleteAll Nothing) newworld
  where
    go _ [] = Nothing
    go int (a':[]) = if inhabits a' mouseLocation && (not $ (Just int) `elem` oldtargets) then Just int else Nothing
    go int (a':as') = if inhabits a' mouseLocation && (not $ (Just int) `elem` oldtargets) then Just int else go (int + 1) as'
    World char _ p zoom _ oldtargets _ _ _ _ _ _ _ _ _ _ _ _= world
    (a:|as) = char
    (a1, a2) = point
    (b1, b2) = p
    newworld = over (#runTarget) (target :) world
    mouseLocation = (round $ (a1 - b1) / zoom, round $ (a2 - b2) / zoom)    
    target = if inhabits a mouseLocation && (not $ (Just 0) `elem` oldtargets) then Just 0 else go 1 as

choosePolygonPoints :: Point -> World -> IO World
choosePolygonPoints point world = return newworld
  where
    World char _ p zoom _ oldtargets _ _ _ _ _ _ _ _ _ _ _ _= world
    (a:|as) = char
    (a1, a2) = point
    (b1, b2) = p
    newworld = over (#runLocations) (mouseLocation :) world
    mouseLocation = (round $ (a1 - b1) / zoom, round $ (a2 - b2) / zoom)

getAttack :: Bool -> Location -> Maybe Int -> World -> StdGen -> Game (Status, [Action (Attack Int) Roll])
getAttack multi loc mint world g = do
  gets go
  where
    go cs = (over (#ap) (\x -> x - roundeddistance) newstatus, fst $ runState (attacksRolls (longshots distance $ (updateattack (mi1 & mi2) Nothing) <$> actionvisibility)) g)
      where
        lineofattack = linealgo loc (runLocation . NE.head $ cs)
        interveningcharacters = filter (\x -> inhabitats x lineofattack && (not $ inhabits x loc) && (x /= (NE.head cs))) (NE.toList cs)
        fover :: (a->b) -> (a, c) -> (b, c)
        fover f (x, y) = (f x, y)
        ap :: Float
        ap = fromIntegral $ view (#ap) attstatus
        apdistancecost = distanceCheck world
        chargeBool = ((5 * (eucliddistance (view (#runStartLocation) (NE.head cs)) (view (#runLocation) (NE.head cs)))) >= (view (#status . #movement) (NE.head cs))) && ((Missile /=) . rangefinder . view (#status . #primaryhand . #weapontype) $ NE.head cs)
        roundeddistance
          | apavailable <= 2 && apavailable >= 1 && chargeBool = 1
          | otherwise = ceiling apdistancecost
        apavailable :: Float
        apavailable = ap - apdistancecost
        distance = if meleedistance (runLocation . NE.head $ cs) loc <= 1 
            then (Just $ meleedistance (runLocation . NE.head $ cs) loc) 
            else (Just $ eucliddistance (runLocation . NE.head $ cs) loc)
        mi1 = case (runTarget world) of
          [] -> Just 0
          (a: _) -> if fst $ flanked (runCharacters world) a then Just 3 else Just 0
        mi2 = case interveningcharacters of
          [] -> Nothing
          (a: _) -> Just (-4)
        attstatus = (status . NE.head) cs
        actionvisibility =  if loc `elem` (view (#immediateVision) (NE.head cs)) then action else updateattack (Just (-5)) Nothing <$> action
        stellarrush = view (#status . #classfeatures . #solarionfeatures . #stellarrush) (NE.head cs) && distanceCheck world >= 1 && distanceCheck world <= 2        
        improvedgetem = isActive . temporary $ view (#status . #effects . #improvedgetem) (NE.head cs)    
        (newstatus, action)
          | apavailable < 1 && inCombat world = (view (#status) (NE.head cs), [])
          | mint == (Just 1) && ((not . inCombat $ world) || (apavailable >= 2)) = fover (over (#ap) (\x -> x - 2)) $ fmap (\x -> [x]) (((cm2a (#cmb) (Just $ Effect Prone (Present (Just 1) [] [0]) Nothing Nothing)) . NE.head) cs)
          | mint == (Just 2) && ((not . inCombat $ world) || (apavailable >= 2)) = fover (over (#ap) (\x -> x - 2)) $ fmap (\x -> [x]) (((cm2a (#cmb) (Just $ Effect Grappled (Present (Just 2) [] [20]) Nothing Nothing)) . NE.head) cs)
          | mint == (Just 3) && ((not . inCombat $ world) || (apavailable >= 2)) = fover (over (#ap) (\x -> x - 2)) $ fmap (\x -> [x]) (((cm2a (#cmb) (Just $ Effect Disarmed (Present (Just 1) [] [0]) Nothing Nothing)) . NE.head) cs)
          | ((isMonster . NE.head) cs && (distance <= (Just 1))) && (apavailable <= 2) && chargeBool && inCombat world = 
             (over (#ap) (\x -> x - 2) $ t2ix Charged (Present (Just 2) [] Nothing) newstatus4, 
                 [updateattack (Just 2) Nothing action4] )
          | stellarrush && apavailable <= 2 && chargeBool && inCombat world = 
            (over (#ap) (\x -> x - 2) newstatus2, 
                  (chargeupdate (view (#status . #primaryhand . #weapontype) . NE.head $ cs) <$> action2))
          | apavailable <= 2 && chargeBool && inCombat world = 
            (over (#ap) (\x -> x - 2) $ t2ix Charged (Present (Just 2) [] Nothing) newstatus2, 
                  (chargeupdate (view (#status . #primaryhand . #weapontype) . NE.head $ cs) <$> action2))
          | apavailable < 2 && inCombat world = (view (#status) (NE.head cs), [])
          | ((isMonster . NE.head) cs && (distance <= (Just 1))) && ((apavailable >= 3 || not (inCombat world)) && multi) = fover (over (#ap) (\x -> x - 3)) $ ((sa2a (#monsterAttack)) . NE.head) cs
          | (apavailable >= 2 || not (inCombat world)) && distance <= (Just 6) && 
              (isActive . temporary . view (#status . #effects . #trickattack) . NE.head $ cs) = fover (over (#ap) (\x -> x - 3)) $ weapon21attacks attstatus
          | (apavailable >= 2 || not (inCombat world)) && improvedgetem = fover (over (#ap) (\x -> x - 2)) $ weapon21attacks attstatus
          | ((apavailable >= 3 || not (inCombat world)) && multi) = fover (over (#ap) (\x -> x - 3)) $ weapon2listattacks attstatus
          | ((isMonster . NE.head) cs && (distance <= (Just 1))) = fover (over (#ap) (\x -> x - 2)) $ fmap (\x -> [x]) (sa21a (NE.head cs) ((NE.head . (view (#status . #monsterAttack)) . NE.head) cs))
          | otherwise = fover (over (#ap) (\x -> x - 2)) $ weapon21attacks attstatus
          where
            (newstatus2, action2) = weapon21attacks attstatus
            (newstatus4, action4) = (sa21a (NE.head cs) ((NE.head . (view (#status . #monsterAttack)) . NE.head) cs))

distanceCheck :: World -> Float
distanceCheck world = distancecost
  where
    char = NE.head . runCharacters $ world
    distance = fromIntegral $ view (#status . #movementtotal) char
    speed = fromIntegral $ view (#status . #movement) char
    distancecost = distance / speed
  
makeAttack :: Maybe Int -> World -> IO World
makeAttack mint world = do
  g <- newStdGen
  _ <- simpleshowls . fmap runF $ newattacktext
  _ <- index' a
  g1 <- newStdGen
  let (oldresults, oldcharacters) = runState (go g target) a
  let newstatus = fst $ runState (go2 g target) a
  let newstatus1 = go4 oldresults newstatus
  (stib, gametextold1, stib2) <- sneakAttackAuto world
  (stellar, gametextold2) <- stellarRushAuto world
  (gem, gametextold3) <- improvedGetemAuto world
  (cleave, gametextold4, attackerfunc) <- cleaveAuto world oldresults
  (sola, gametextold5) <- solarAccelAuto world
  let gametextnew = if apavailable < 1 && inCombat world then ["No Ap Available"] 
      else if longshots distance (newattacktext) == [] then ["Out of Attack Range"] else delete [] $ gametext0 ++ [gametextold1] ++ [gametextold2] ++ [gametextold3] ++ [gametextold5] ++ gametextold4
  --let (results, characters) = (takeWhile (\x -> not (x == CriticalMiss)) oldresults, safemodifyat int1 (undoresults oldresults) oldcharacters )
  let damage = view (#status . #damage) (NE.head oldcharacters)
  let injury = view (#status . #injury) (NE.head oldcharacters)
  let ffield = view (#status . #effects . #forcefield) (NE.head oldcharacters)
  let resolve = view (#status . #resolve) (NE.head oldcharacters)
  let newstatus2 = set (#damage) damage . set (#resolve) resolve . set (#effects . #forcefield) ffield $ newstatus1
  let newstatus3 = healthupdate $ set (#injury) injury newstatus2
  let oldcharacters2 = cleave  oldcharacters
  let newcharacters = set (#status) newstatus3 (NE.head oldcharacters2) :| (NE.tail oldcharacters2)
  let (attacker :| defenders) = newcharacters
  let finalcharacters =  sola $ safemodifyat target (stib . stellar . gem) (go5 g1 target (stib2 . attackerfunc $ attacker) :| defenders)
  let finalcharactersworld = set (#runGameMode) Map . set (#runCharacters) finalcharacters $ world
  putStrLn "Test1"
  putStrLn $ show lineofattack
  putStrNLn $ fmap (view (#name)) interveningcharacters
  putStrLn "Test1"
  _ <- simpleshowls2 $ go6 a g
  putStrLn (show oldresults)
  putStrLn ""
  putStrLn (show $ detailedview (NE.head finalcharacters))
  putStrLn (show $ fmap detailedview (finalcharacters !!? target))
  putStrLn ""
  (ioEndTurn $ over (#runGameText) (++ [[]] ++ (deleteAll [] ((delete [] ((simpleshow2list $ go6 a g) ++ gametextnew)) ++ [showresults oldresults, go7 (finalcharacters !!? target)]))) finalcharactersworld)
    where
      lineofattack = linealgo enemylocation (runLocation . NE.head $ a)
      interveningcharacters = filter (\x -> inhabitats x lineofattack && (not $ inhabits x enemylocation) && (x /= (NE.head a))) (NE.toList a)
      interbool = length interveningcharacters >= 1
      gametext0 = if interbool then ["Blocked!"] else []
      range = view (#runF . #info . #increment) <$> newattacktext
      distance = case runTarget world of
                        [] -> Just 0
                        (ji : _) ->  meleedistancechar2 (NE.head a) <$> (a !!? ji)
      (newstatus2', action2) = weapon21attacks ((status . NE.head) a)
      (newstatus4, action4) = (sa21a (NE.head a) ((NE.head . (view (#status . #monsterAttack)) . NE.head) a))
      ap = fromIntegral $ view (#ap) ((status . NE.head) a)
      apdistancecost = distanceCheck world
      apavailable = ap - apdistancecost 
      improvedgetem = isActive . temporary $ view (#status . #effects . #improvedgetem) (NE.head a)
      stellarrush = view (#status . #classfeatures . #solarionfeatures . #stellarrush) (NE.head a) && distanceCheck world >= 1 && distanceCheck world <= 2
      newattacktext = if flankers then updateattack (Just 3) Nothing <$> newattacktext1 else newattacktext1
      newattacktext1 = if interbool then updateattack (Just (-4)) Nothing <$> (snd attacktext) else snd attacktext
      attacktext        
        | apavailable < 1 && inCombat world = (view (#status) (NE.head a), [])
        | mint == (Just 1) && ((not . inCombat $ world) || (apavailable >= 2)) = fmap (\x -> [x]) (((cm2a (#cmb) (Just $ Effect Prone (Present (Just 1) [] [0]) Nothing Nothing)) . NE.head) a)
        | mint == (Just 2) && ((not . inCombat $ world) || (apavailable >= 2)) = fmap (\x -> [x]) (((cm2a (#cmb) (Just $ Effect Grappled (Present (Just 2) [] [20]) Nothing Nothing)) . NE.head) a)
        | mint == (Just 3) && ((not . inCombat $ world) || (apavailable >= 2)) = fmap (\x -> [x]) (((cm2a (#cmb) (Just $ Effect Disarmed (Present (Just 1) [] [0]) Nothing Nothing)) . NE.head) a)
        | ((isMonster . NE.head) a && (distance <= (Just 5))) && (apavailable <= 2) && ((5 * (meleedistance (view (#runStartLocation) (NE.head a)) (view (#runLocation) (NE.head a)))) >= (view (#status . #movement) (NE.head a))) && inCombat world = 
             (t2ix Charged (Present (Just 2) [] Nothing) newstatus4, 
                 [updateattack (Just 2) Nothing action4] )
        | stellarrush && apavailable <= 2 && ((5 * (meleedistance (view (#runStartLocation) (NE.head a)) (view (#runLocation) (NE.head a)))) >= (view (#status . #movement) (NE.head a))) && inCombat world = 
            (newstatus2', (chargeupdate (view (#status . #primaryhand . #weapontype) . NE.head $ a) <$> action2))
        | apavailable <= 2 && ((5 * (meleedistance (view (#runStartLocation) (NE.head a)) (view (#runLocation) (NE.head a)))) >= (view (#status . #movement) (NE.head a))) && inCombat world = 
            (t2ix Charged (Present (Just 2) [] Nothing) newstatus2', 
                  (chargeupdate (view (#status . #primaryhand . #weapontype) . NE.head $ a) <$> action2))
        | apavailable < 2 && inCombat world = (view (#status) (NE.head a), [])
        
        | (apavailable >= 2 || not (inCombat world)) && distance <= (Just 30) && 
              (isActive . temporary . view (#status . #effects . #trickattack) . NE.head $ a) = weapon21attacks ((status . NE.head) a) 
        | (apavailable >= 2 || not (inCombat world)) && improvedgetem = weapon21attacks ((status . NE.head) a)
        | ((isMonster . NE.head) a && (distance <= (Just 5))) && (apavailable >= 3 || not (inCombat world)) = ((sa2a (#monsterAttack)) . NE.head) a
        | apavailable >= 3 || not (inCombat world) = weapon2listattacks ((status . NE.head) a)
        | ((isMonster . NE.head) a && (distance <= (Just 5))) = fmap (\x -> [x]) (sa21a (NE.head a) ((NE.head . (view (#status . #monsterAttack)) . NE.head) a))
        | otherwise = weapon21attacks ((status . NE.head) a)
      target = case view (#runTarget) world of
        [] -> Nothing
        (x: _) -> x
      charlocation = view (#runLocation) (NE.head a)
      enemylocation = case a !!? target of
        Nothing -> view (#runLocation) (NE.head a)
        Just y -> minimumBy (\x y -> compare (eucliddistance x charlocation) (eucliddistance y charlocation)) (inhabitssquare y)
      flankers = fst $ flanked a target
      a = runCharacters world
      go :: StdGen -> Maybe Int -> Game [Result (Maybe Int)]
      go g target' = do
        blah <- getAttack True enemylocation mint world g
        let attack = snd blah
        (action2gameresult attack target')
      go2 :: StdGen -> Maybe Int -> Game Status
      go2 g _ = do
        blah <- getAttack True enemylocation mint world g
        let attack = fst blah
        return attack
      go4 :: Eq a => [Result a] -> Status -> Status
      go4 oldresults newstatus
        | CriticalMiss `elem` oldresults = t2ix Prone (Present (Just 2) [] Nothing) newstatus
        | otherwise = newstatus
      go5 :: StdGen -> Maybe Int -> Character -> Character
      go5 g2 int1
        | rangefinder (view (#status . #primaryhand . #weapontype) (NE.head a)) /= Missile = fireshieldcharacter g2 (a !!? int1)
        | otherwise = id
      go6 :: NonEmpty Character -> StdGen -> [Attack Int Int]
      go6 as g = blah
        where
          (_, blah) = fmap (fmap (fmap runRoll)) $ fmap (fmap runF) . fst $ runState (getAttack True enemylocation mint world g) as
      go7 :: Maybe Character -> String
      go7 maybea = case maybea of
        Nothing -> ""
        Just y -> detailedview y

sneakAttack :: Character -> [Result (Maybe Int)] -> Maybe Int -> IO (Character -> Character)
sneakAttack _ _ _ = return id

cleaveAuto :: World -> [Result (Maybe Int)] -> IO ((NonEmpty Character -> NonEmpty Character), [String], Character -> Character)
cleaveAuto world result
  | not cleave = return (id, [], id)
  | otherwise = do
    g <- newStdGen
    let newcharfunc = foldr (\x y -> (\z -> snd $ runState (go g x) z) . y) id (fst newtargets)
    let cleaveresult = foldr (\x y -> (fst $ runState (go g x) chars) ++ y) [] (fst newtargets)
    return (newcharfunc, "Cleave" : fmap show cleaveresult, snd newtargets)
    where
      ap :: Float
      ap = fromIntegral $ view (#status . #ap) attacker
      apdistancecost = distanceCheck world
      apavailable :: Float
      apavailable = ap - apdistancecost
      newtargetsprime = indexfunction (\x -> meleedistancechar ogenemy x == 1 && 
        meleedistancechar attacker x == 1 && 
        (temporary . view (#status . #team) $ x) /= (temporary . view (#status . #team) $ attacker)) chars
      newtargets = if viewtempmodifyier cleavevalue > Nothing && (fst (resolvepoints . view (#status) $ attacker) >= 1) && (length newtargetsprime >= 1)
        then (newtargetsprime, over (#status) (damager' (#resolve) (Just 1)))
        else (take 1 newtargetsprime, id)
      go :: StdGen -> Maybe Int -> Game [Result (Maybe Int)]
      go g' mint = do
        blah <- getAttack False (go4 $ chars !!? mint) Nothing world g'
        let attack = snd blah
        (action2gameresult attack mint)
      go4 :: Maybe Character -> Location
      go4 mc = case mc of
        Nothing -> (0,0)
        Just cha -> runLocation cha
      chars = view (#runCharacters) world
      attacker = NE.head chars
      cleavevalue =  temporary $ view (#status . #effects . #cleave) attacker
      cleave = (isActive cleavevalue) && length (fst newtargets) >= 1 && resultbool && apavailable >= 3
      target = case view (#runTarget) world of
        [] -> Nothing
        (a: _) -> a
      ogenemy = case chars !!? target of
        Nothing -> defaultcharacter
        Just y -> y
      resultbool = case result of
        [] -> False
        (a:_) -> isHit a

improvedGetemAuto :: World -> IO ((Character -> Character), String)
improvedGetemAuto world
  | not improvedgetem = return (id, "")
  | otherwise = do
    return (over (#status) (t2ix Getem (Present (Just 2) [] Nothing)), "Improved Get'Em") 
    where
      chars = view (#runCharacters) world
      attacker = NE.head chars
      improvedgetem = isActive . temporary $ view (#status . #effects . #improvedgetem) attacker
      target = case view (#runTarget) world of
        [] -> Nothing
        (a: _) -> a

stellarRushAuto :: World -> IO ((Character -> Character), String)
stellarRushAuto world
  | not stellarrush = return (id, "")
  | otherwise = do
    g <- newStdGen
    g2 <- newStdGen
    let roll1 = runRoll . fst $ roll 20 g
    let damageamount = sum . fmap runRoll . fst $ runState (listdice levelamaount) g2
    let sdddamage = mddstatus 0 roll1 (Just $ D damageamount 0 (Just Fire) (Just $ Target Reflex 0) Half)
    return (over (#status) (healthupdate . sdddamage), "StellarRush DC " ++ (show $ roll1 + dc)) 
    where
      chars = view (#runCharacters) world
      attacker = NE.head chars
      levelamaount 
        | (getLevel $ view (#status . #primclass . #level) attacker) < 6 = [6,6] 
        | otherwise = take ((getLevel $ view (#status . #primclass . #level) attacker) `div` 2) $ repeat 6
      dc = (getLevel $ view (#status . #primclass . #level) attacker) `div` 2 + (view (#charisma) . abilitybonuses . view (#status) $ attacker)
      stellarrush = view (#status . #classfeatures . #solarionfeatures . #stellarrush) attacker && distanceCheck world >= 1 && distanceCheck world <= 2
      target = case view (#runTarget) world of
        [] -> Nothing
        (a: _) -> a

solarAccelAuto :: World -> IO ((NonEmpty Character -> NonEmpty Character), String)
solarAccelAuto world
  | not solaraccelerationbool = return (id, "")
  | otherwise =
    return (nemodifywhile 
      (over (#status) (t2ix Haste (Present (Just 2) [] Nothing)))
      (\x -> (temporary $ view (#status . #team) x) == (temporary $ view (#status . #team) attacker)) , "Solar Accelleration") 
      where
        ap :: Float
        ap = fromIntegral $ view (#status . #ap) attacker
        apdistancecost = distanceCheck world
        apavailable :: Float
        apavailable = ap - apdistancecost
        chars = view (#runCharacters) world
        attacker = NE.head chars
        solaraccelerationbool = view (#status . #classfeatures . #solarionfeatures . #solaracceleration) attacker && 
          apavailable >= 3 && ((\x -> x >= (Just 3)) . viewtempmodifyier . temporary $ view (#status . #effects . #photonmode) attacker)

sneakAttackAuto :: World -> IO ((Character -> Character), String, (Character -> Character))
sneakAttackAuto world
  | (not $ isActive trick) || (distance > 6) = return (id, "", id)
  | otherwise = do
    g <- newStdGen
    let roll1 = fst . fst $ skillcheck g (#skills . #stealth) (#dexterity) (#skills . #stealth) (NE.head $ runCharacters world)
    g2 <- newStdGen
    let damageamount = sum . fmap runRoll . fst $ runState (listdice levelamaount) g2
    if (Just roll1) >= defender 
      then return (over (#status) (t2ix FlatFooted (Present (viewtempmodifyier trick) [] Nothing) . healthupdate . damager (Just damageamount)), 
            "Success " ++ (show roll1) ++ " vs " ++ (show defender) ++ ", " ++ (show $ damageamount) ++ " Dam", fftargetfunction) 
      else return (id, "Failure " ++ (show roll1) ++ " vs " ++ (show defender), id)
    where
      chars = view (#runCharacters) world
      attacker = NE.head chars
      levelamaount
        | (getLevel $ view (#status . #primclass . #level) attacker) < 3 = [4] 
        | (getLevel $ view (#status . #primclass . #level) attacker) < 5 = [8] 
        | otherwise = take (((getLevel $ view (#status . #primclass . #level) attacker) + 1) `div` 2) $ repeat 8
      trick = temporary $ view (#status . #effects . #trickattack) attacker
      distance = meleedistancechar2 attacker defenderog
      defender = fmap ((\x -> 20 + x) . getLevel . view (#status . #primclass . #level)) $ chars !!? target
      defenderog = case chars !!? target of
        Nothing -> defaultcharacter
        Just y -> y
      target = case view (#runTarget) world of
        [] -> Nothing
        (a: _) -> a
      fftargetfunction = if (viewtempmodifyier trick) > (Just 1) then over (#status . #effects . #trickattack) (settemporary (set (#character) (view (#name) defenderog) trick)) else id

distanceshot :: Maybe Int -> IO (Maybe Int)
distanceshot i
    | (i == (Just 8)) || (i == (Just 9)) || (i == (Just 88)) || (i == (Just 99)) = do
      putStrLn "Enter Distance"
      int <- getInt
      return int
    | otherwise = return Nothing

bonuscheck :: Maybe Int -> IO (Maybe Int, Maybe Int)
bonuscheck i
    | (i == (Just 11)) || (i >= (Just 22)) = do
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
      weaponattack = [s2afunction $ w2a' True (view (#status) c) (view (#status . #primaryhand) c)]

simpleshow :: (Show a) => Attack a [Int] -> String
simpleshow a = names ++ ": " ++  (go2 . view (#info . #material) $ a) ++ "Attack Bonus: " ++ (go $ view (#info . #attackbonus) a) ++ 
               (go1 $ view (#ddice) a) ++
               (go32 $ view (#mddice) a) ++ (go3 $ view (#mddice) a) ++
               (go42 $ view (#mstatdamage) a) ++ (go4 $ view (#mstatdamage) a) ++
               (go52 $ view (#meffect) a) ++ (go5 $ view (#meffect) a) ++
               (go6 $ view (#msingddice) a) ++
               (go82 $ view (#msingeffect) a) ++ (go8 $ view (#msingeffect) a)
  where
    names = (view (#info . #attackname) a)
    mtargetshow :: Maybe Target -> String
    mtargetshow mtarget = case mtarget of
      Nothing -> ""
      Just target -> (show $ view (#defensetype) target) ++ " DC " ++ (show . (10 + ) $ view (#bonus) target) ++ ", "
    diceshow ldice = case ldice of
      [] -> ""
      as -> (show . length $ as) ++ "D" ++ (show . head $ as)
    diceshow2 :: [Int] -> Int -> String
    diceshow2 is i = case i of
      0 -> diceshow is
      y -> case is of
        [] -> show y
        [0] -> show y
        x -> diceshow x ++ " + " ++ show y
    go2 x
      | show x /= (takeWhile (/= ' ') names) = show x ++ ", "
      | otherwise = ""
    go ma = case ma of
      Nothing -> ""
      Just y -> show y
    go0 ma = case ma of
      Nothing -> ""
      Just y -> show y ++ ", "
    go01 ma = case ma of
      Nothing -> ""
      Just y -> " " ++ show y
    go02 ma = case ma of
      Nothing -> ""
      Just y -> ", " ++ show y
    go1 mdd = case mdd of
      Just (D x b c d _) -> ", Primary Damage: " ++ diceshow2 x b ++ " "  ++ go0 c ++ go d
      Nothing -> ""
    go3 ldd = case ldd of
      [] -> ""
      ((D x b c d _) : as) -> diceshow2 x b ++ go01 c ++ go02 d ++ ", " ++ go3 as
    go32 ldd = case ldd of
      [] -> ""
      _ -> ", Secondary Damage: "
    go4 :: Maybe (StatDamage [Int]) -> String
    go4 msd = case msd of
      Nothing -> ""
      Just (SD mtype mtarg mabil _ dice dicebonus duration durationbonus) -> go0 mtype ++ mtargetshow mtarg ++ diceshow2 dice dicebonus ++ " " ++ go mabil ++ " Damage" ++ go43 duration durationbonus
    go42 msd = case msd of
      Nothing -> ""
      _ -> ", Stat Damage: "
    go43 dur dbonus
      | (dur == [] || dur == [0]) && (dbonus == 0) = ""
      | otherwise = "for " ++ diceshow2 dur dbonus ++ " rounds, "
    go5 :: Maybe (Effect (Temporal [Int])) -> String
    go5 meff = case meff of
      Nothing -> ""
      Just (Effect seff temp _ mtarg) -> show seff ++ (go53 . temporalduration $ temp) ++ ", " ++ mtargetshow mtarg
    go52 :: Maybe (Effect (Temporal [Int])) -> String
    go52 meff = case meff of
      Nothing -> ""
      _ -> ", Effect: "
    go53 mint = case mint of
      Nothing -> ""
      Just 1 -> " for 1 round"
      Just y -> " for " ++ show y ++ " rounds"
    go6 mdd = case mdd of
      Just (D x b c d _) -> ", Single Damage: " ++ diceshow2 x b ++ " "  ++ go0 c ++ go d
      Nothing -> ""
    go8 :: [Effect (Temporal [Int])] -> String
    go8 leff = case leff of
      [] -> ""
      (Effect seff temp _ mtarg) : as -> show seff ++ " " ++ mtargetshow mtarg ++ (go . temporalduration $ temp) ++ go8 as
    go82 :: [Effect (Temporal [Int])] -> String
    go82 leff = case leff of
      [] -> ""
      _ -> ", Single Effect:"

simpleshow3 :: Spell [Int] -> String
simpleshow3 a = names ++ ": " ++ "    Spell Bonus: " ++ (show $ view (#info . #spellbonus) a) ++ 
               (go32 $ view (#mdamdice) a) ++ (go3 $ view (#mdamdice) a) ++
               (go42 $ view (#mstatdamage) a) ++ (go4 $ view (#mstatdamage) a) ++
               (go52 $ view (#meffect) a) ++ (go5 $ view (#meffect) a)
  where
    names = (view (#info . #name) a)
    mtargetshow :: Maybe Target -> String
    mtargetshow mtarget = case mtarget of
      Nothing -> ""
      Just target -> (show $ view (#defensetype) target) ++ " DC " ++ (show . (view (#info . #spellbonus) a + 10 + ) $ view (#bonus) target) ++ ", "
    diceshow ldice = case ldice of
      [] -> ""
      as -> (show . length $ as) ++ "D" ++ (show . head $ as)
    diceshow2 :: [Int] -> Int -> String
    diceshow2 is i = case i of
      0 -> diceshow is
      y -> case is of
        [] -> show y
        [0] -> show y
        x -> diceshow x ++ " + " ++ show y
    go ma = case ma of
      Nothing -> ""
      Just y -> show y
    go0 ma = case ma of
      Nothing -> ""
      Just y -> show y ++ ", "
    go01 ma = case ma of
      Nothing -> ""
      Just y -> " " ++ show y
    go3 ldd = case ldd of
      Nothing -> ""
      Just (D x b c d e) -> diceshow2 x b ++ go01 c ++ ", " ++ mtargetshow d ++ show e
    go32 ldd = case ldd of
      Nothing -> ""
      _ -> "    Damage: "
    go4 :: Maybe (StatDamage [Int]) -> String
    go4 msd = case msd of
      Nothing -> ""
      Just (SD mtype mtarg mabil _ dice dicebonus duration durationbonus) -> go0 mtype ++ mtargetshow mtarg ++ diceshow2 dice dicebonus ++ " " ++ go mabil ++ " Damage" ++ go43 duration durationbonus
    go42 msd = case msd of
      Nothing -> ""
      _ -> ",     Stat Damage: "
    go43 dur dbonus
      | (dur == [] || dur == [0]) && (dbonus == 0) = ""
      | otherwise = "for " ++ diceshow2 dur dbonus ++ " rounds, "
    go5 :: Maybe (Effect (Temporal [Int])) -> String
    go5 meff = case meff of
      Nothing -> ""
      Just (Effect seff temp _ mtarg) -> show seff ++ (go53 . temporalduration $ temp) ++ ", " ++ mtargetshow mtarg
    go52 :: Maybe (Effect (Temporal [Int])) -> String
    go52 meff = case meff of
      Nothing -> ""
      _ -> ",     Effect: "
    go53 mint = case mint of
      Nothing -> ""
      Just 1 -> " for 1 round"
      Just y -> " for " ++ show y ++ " rounds"

simpleshowls :: (Show a) => [Attack a [Int]] -> IO ()
simpleshowls [] = return ()
simpleshowls (l : ls) = do
  putStrLn $ simpleshow l
  simpleshowls ls

simpleshowls2 :: [Attack Int Int] -> IO ()
simpleshowls2 [] = return ()
simpleshowls2 (l : ls) = do
  putStrLn $ simpleshow2 l
  simpleshowls2 ls

simpleshowls3 :: [Spell [Int]] -> IO ()
simpleshowls3 [] = return ()
simpleshowls3 (l : ls) = do
  putStrLn $ simpleshow3 l
  simpleshowls3 ls

simpleshowls4 :: [Spell Int] -> IO ()
simpleshowls4 [] = return ()
simpleshowls4 (l : ls) = do
  putStrLn $ simpleshow4 l
  simpleshowls4 ls

simpleshow2list :: [Attack Int Int] -> [String]
simpleshow2list [] = [[]]
simpleshow2list (a : []) = delete [] $ delete [] (simpleshow2' a)
simpleshow2list (a : as) = (delete [] $ delete [] (simpleshow2' a)) ++ (simpleshow2list as)

simpleshow2' :: Attack Int Int -> [String]
simpleshow2' a =  [names ++ ": " ++ ", Attack: " ++ (go03 (view (#info . #attackbonus) a) attackroll) ++ 
               (go1 $ view (#ddice) a)] ++
               [(go42 $ view (#mstatdamage) a) ++ (go4 $ view (#mstatdamage) a)] ++
               [(go52 $ view (#meffect) a) ++ (go5 $ view (#meffect) a)]
  where
    mtargetshow :: Int -> Maybe Target -> String
    mtargetshow int mtarget = case mtarget of
      Nothing -> ""
      Just target -> (show $ view (#defensetype) target) ++ " DC " ++ (show . (int + ) $ view (#bonus) target) ++ ", "
    diceshow2 :: Int -> Int -> String
    diceshow2 i1 i2 = show ( i1 + i2)
    names = (view (#info . #attackname) a)
    threshold = (view (#info . #critical . #threshold) a)
    attackroll = view (#rolls . #tohitroll) a
    effectroll = view (#rolls . #effectroll) a
    statdamageroll = view (#rolls . #statdamageroll) a
    go ma = case ma of
      Nothing -> []
      Just y -> show y
    go0 ma = case ma of
      Nothing -> []
      Just y -> show y ++ ", "
    go1 mdd = case mdd of
      Just (D x b c _ _) -> ", Damage: " ++ diceshow2 x b ++ " "  ++ go0 c
      Nothing -> []
    go03 mint int = case mint of
      Nothing -> []
      Just y -> go04 y int
    go04 int1 int2 = show (int1 + int2)
    go4 :: Maybe (StatDamage Int) -> String
    go4 msd = case msd of
      Nothing -> []
      Just (SD mtype mtarg mabil _ dice dicebonus duration durationbonus) -> go0 mtype ++ mtargetshow statdamageroll mtarg ++ diceshow2 dice dicebonus ++ " " ++ go mabil ++ go43 duration durationbonus
    go42 msd = case msd of
      Nothing -> []
      _ -> "SD: "
    go43 dur dbonus
      | (dur == [0] || dur == []) && (dbonus == 0) = []
      | otherwise = diceshow2 (go44 dur) dbonus ++ "r, "
    go44 ds = case ds of
      [] -> 0
      y -> sum y
    go5 :: Maybe (Effect (Temporal Int)) -> String
    go5 meff = case meff of
      Nothing -> []
      Just (Effect seff temp _ mtarg) -> show seff ++ (go53 . temporalduration $ temp) ++ ", " ++ mtargetshow effectroll mtarg
    go52 :: Maybe (Effect (Temporal Int)) -> String
    go52 meff = case meff of
      Nothing -> []
      _ -> "E: "
    go53 mint = case mint of
      Nothing -> []
      Just y -> " " ++ show y ++ "r"

simpleshow2 :: Attack Int Int -> String
simpleshow2 a =  names ++ ": " ++  (go2 . view (#info . #material) $ a) ++ "Attack: " ++ (go03 (view (#info . #attackbonus) a) attackroll) ++ "(" ++ (show $ attackroll) ++ ")" ++
               (go1 $ view (#ddice) a) ++
               (go32 $ view (#mddice) a) ++ (go3 . function3 $ view (#mddice) a) ++
               (go42 $ view (#mstatdamage) a) ++ (go4 $ view (#mstatdamage) a) ++
               (go52 $ view (#meffect) a) ++ (go5 $ view (#meffect) a) ++
               (go6 $ view (#msingddice) a) ++
               (go82 $ view (#msingeffect) a) ++ (go8 . function3 $ view (#msingeffect) a)
  where
    names = (view (#info . #attackname) a)
    threshold = (view (#info . #critical . #threshold) a)
    attackroll = view (#rolls . #tohitroll) a
    effectroll = view (#rolls . #effectroll) a
    statdamageroll = view (#rolls . #statdamageroll) a
    msddroll = view (#rolls . #msddroll) a
    stealthroll = view (#rolls . #stealthroll) a
    mtargetshow :: Int -> Maybe Target -> String
    mtargetshow int mtarget = case mtarget of
      Nothing -> ""
      Just target -> (show $ view (#defensetype) target) ++ " DC " ++ (show . (int + ) $ view (#bonus) target) ++ ", "
    diceshow2 :: Int -> Int -> String
    diceshow2 i1 i2 = show ( i1 + i2)
    go2 x
      | show x /= (takeWhile (/= ' ') names) = show x ++ ", "
      | otherwise = ""
    go ma = case ma of
      Nothing -> ""
      Just y -> show y
    go0 ma = case ma of
      Nothing -> ""
      Just y -> show y ++ ", "
    go01 ma = case ma of
      Nothing -> ""
      Just y -> " " ++ show y
    go02 ma = case ma of
      Nothing -> ""
      Just y -> ", " ++ show y
    go03 mint int = case mint of
      Nothing -> ""
      Just y -> go04 y int
    go04 int1 int2 = show (int1 + int2)
    go1 mdd = case mdd of
      Just (D x b c _ _) -> ", Primary Damage: " ++ diceshow2 x b ++ " "  ++ go0 c
      Nothing -> ""
    go3 ldd = case ldd of
      [] -> ""
      ((D x b c d _) : as) -> diceshow2 x b ++ go01 c ++ go02 d ++ ", " ++ go3 as
    go32 ldd = case ldd of
      [] -> ""
      _ -> ", Secondary Damage: "
    go4 :: Maybe (StatDamage Int) -> String
    go4 msd = case msd of
      Nothing -> ""
      Just (SD mtype mtarg mabil _ dice dicebonus duration durationbonus) -> go0 mtype ++ mtargetshow statdamageroll mtarg ++ diceshow2 dice dicebonus ++ " " ++ go mabil ++ " Damage" ++ go43 duration durationbonus
    go42 msd = case msd of
      Nothing -> ""
      _ -> ", Stat Damage: "
    go43 dur dbonus
      | (dur == [0] || dur == []) && (dbonus == 0) = ""
      | otherwise = "for " ++ diceshow2 (go44 dur) dbonus ++ " rounds, "
    go44 ds = case ds of
      [] -> 0
      y -> sum y
    go5 :: Maybe (Effect (Temporal Int)) -> String
    go5 meff = case meff of
      Nothing -> ""
      Just (Effect seff temp _ mtarg) -> show seff ++ (go53 . temporalduration $ temp) ++ ", " ++ mtargetshow effectroll mtarg
    go52 :: Maybe (Effect (Temporal Int)) -> String
    go52 meff = case meff of
      Nothing -> ""
      _ -> ", Effect: "
    go53 mint = case mint of
      Nothing -> ""
      Just 1 -> " for 1 round"
      Just y -> " for " ++ show y ++ " rounds"
    go6 mdd = case mdd of
      Just (D x b c d _) -> ", Single Damage: " ++ diceshow2 x b ++ " "  ++ go0 c ++ mtargetshow msddroll d
      Nothing -> ""
    go8 :: [Effect (Temporal Int)] -> String
    go8 leff = case leff of
      [] -> ""
      (Effect seff temp _ mtarg) : as -> show seff ++ " " ++ mtargetshow effectroll mtarg ++ (go . temporalduration $ temp) ++ go8 as
    go82 :: [Effect (Temporal Int)] -> String
    go82 leff = case leff of
      [] -> ""
      _ -> ", Single Effect:"

simpleshow4 :: Spell Int -> String
simpleshow4 a = names ++ ": " ++ 
               (go32 $ view (#mdamdice) a) ++ (go3 $ view (#mdamdice) a) ++
               (go42 $ view (#mstatdamage) a) ++ (go4 $ view (#mstatdamage) a) ++
               (go52 $ view (#meffect) a) ++ (go5 $ view (#meffect) a)
  where
    names = (view (#info . #name) a)
    spellbonustotal = view (#info . #spellbonus) a
    effectroll = view (#rolls . #efroll) a
    statdamageroll = view (#rolls . #sdroll) a
    ddroll = view (#rolls . #ddroll) a
    mtargetshow :: Int -> Maybe Target -> String
    mtargetshow int mtarget = case mtarget of
      Nothing -> ""
      Just target -> (show $ view (#defensetype) target) ++ " DC " ++ (show . (int + ) $ view (#bonus) target) ++ ", "
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
      Just y -> ", " ++ show y
    go3 ldd = case ldd of
      Nothing -> ""
      Just (D x b c d _) -> diceshow2 x b ++ go02 c ++ mtargetshow (ddroll + spellbonustotal) d
    go32 ldd = case ldd of
      Nothing -> ""
      _ -> ", Damage: "
    go4 :: Maybe (StatDamage Int) -> String
    go4 msd = case msd of
      Nothing -> ""
      Just (SD mtype mtarg mabil _ dice dicebonus duration durationbonus) -> go0 mtype ++ mtargetshow (statdamageroll + spellbonustotal) mtarg ++ diceshow2 dice dicebonus ++ " " ++ go mabil ++ " Damage" ++ go43 duration durationbonus
    go42 msd = case msd of
      Nothing -> ""
      _ -> ", Stat Damage: "
    go43 dur dbonus
      | ((dur == [0]) || (dur == [])) && (dbonus == 0) = ""
      | otherwise = "for " ++ diceshow2 (go44 dur) dbonus ++ " rounds, "
    go44 ls = case ls of
      [] -> 0
      as -> sum as
    go5 :: Maybe (Effect (Temporal Int)) -> String
    go5 meff = case meff of
      Nothing -> ""
      Just (Effect seff temp _ mtarg) -> show seff ++ (go53 . temporalduration $ temp) ++ ", " ++ mtargetshow (effectroll + spellbonustotal) mtarg
    go52 :: Maybe (Effect (Temporal Int)) -> String
    go52 meff = case meff of
      Nothing -> ""
      _ -> ", Effect: "
    go53 mint = case mint of
      Nothing -> ""
      Just 1 -> " for 1 round"
      Just y -> " for " ++ show y ++ " rounds"

aoo :: World -> IO World
aoo olda = do
  _ <- index' a
  putStrLn "Choose Attackers"
  putStrLn ""
  ints1 <- getInts []
  putStrLn ""
  g <- newStdGen
  let attacksofoppurtunity = (updateattack mobilitybonus Nothing <$>) . concat $ fmap (snd . weapon21attacks . (view (#status))) (catMaybes $ fmap (\x -> a !!? x) ints1)
  let rolledattacks = fst $ runState (attacksRolls attacksofoppurtunity) g
  let (results, status) = runState (attackings rolledattacks) (view (#status) (NE.head a))
  putStrLn (show results)
  let (car :| chars) = a
  let newcharacters = set (#runCharacters) (set (#status) status car :| chars) olda
  ioEndTurn newcharacters
    where
      mobilitybonus = if isActive . temporary $ view (#status . #effects . #mobility) (NE.head a) then Just . negate $ 4 else Nothing
      a = runCharacters olda

aoomovement :: Character -> [Location] -> World -> IO World
aoomovement targetchar locs olda = if find (targetchar ==) chars == Nothing  && car /= targetchar then return olda else
  do
  g <- newStdGen
  let attacksofoppurtunity = (updateattack mobilitybonus Nothing <$>) . concat $ fmap (snd . weaponchoice) inrangeenemies
  let rolledattacks = fst $ runState (attacksRolls attacksofoppurtunity) g
  let (results, status) = runState (attackings rolledattacks) (view (#status) targetchar)
  putStrLn (show results)
  let newcharacters = set (#runCharacters) (nemodifywhile (set (#status) status) (targetchar ==) $ car :| chars) olda
  return $ over (#runGameText) (if length results == 0 then (++ []) else (++ ["Attack of Oppurtunity"] ++ (show <$> results))) newcharacters
    where
      mobilitybonus = if isActive . temporary $ view (#status . #effects . #mobility) targetchar then Just . negate $ 4 else Nothing
      (car :| chars) = runCharacters olda
      checkcharacter = (\x -> view (#status . #team) x /= view (#status . #team) targetchar && (characterThreatens locs x) && (not $ helplessornotincombat x))
      inrangeenemies = if checkcharacter car then car : filter checkcharacter chars else filter checkcharacter chars
      weaponchoice char'
        | isMonster char' = fmap (\x -> [x]) $ sa21a char' (NE.head $ view (#status . #monsterAttack) char')
        | otherwise = weapon21attacks . (view (#status)) $ char'

aoonew :: Bool -> World -> IO World
aoonew True olda = return olda
aoonew False olda = do
  putStrLn "AOO"
  g <- newStdGen
  let attacksofoppurtunity =  (updateattack mobilitybonus Nothing <$>) . concat $ fmap (snd . weaponchoice) inrangeenemies
  let rolledattacks = fst $ runState (attacksRolls attacksofoppurtunity) g
  let (results, status) = runState (attackings rolledattacks) (view (#status) c)
  putStrLn (show results)
  let newcharacters = set (#runCharacters) (set (#status) status c :| cs) olda
  return $ over (#runGameText) (if length results == 0 then (++ []) else (++ ["Attack of Oppurtunity"] ++ (show <$> results))) newcharacters
    where
      mobilitybonus = if isActive . temporary $ view (#status . #effects . #mobility) c then Just . negate $ 4 else Nothing
      c :| cs = runCharacters olda
      inrangeenemies = filter (\x -> view (#status . #team) x /= view (#status . #team) c && ((meleedistancechar c x) <= 1) && (not $ helplessornotincombat x)) cs
      weaponchoice char'
        | isMonster char' = fmap (\x -> [x]) $ sa21a char' (NE.head $ view (#status . #monsterAttack) char')
        | otherwise = weapon21attacks . (view (#status)) $ char'

maybeint2int :: Maybe Int -> Int
maybeint2int mi = case mi of
  Just x -> x
  Nothing -> 0

putStrNLn :: [String] -> IO [a]
putStrNLn [] = return []
putStrNLn (a : as) = do
  putStrLn a
  putStrNLn as

function :: [Maybe a] -> [Maybe a]
function a = fmap (Just) (catMaybes a)

punish :: World -> IO World
punish olda = do
  putStrLn "Choose Elemental"
  putStrLn "1) Fire, 2) Earth, 3) Ice, 4) Wind, 5) Acid, 6) Lightning, 7) Negative, 8) Positive"
  int5 <- getInt
  let elemental = i2me int5
  putStrLn "Choose Defense"
  putStrLn "1) Normal, 2) Touch, 3) Flatfooted, 4) Reflex, 5) Will, 6) Fortitude, 7) cmb, 8) Nothing"
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
  putStrLn (show $ fmap showc characters)
  putStrLn ""
  (ioEndTurn (set (#runCharacters) characters olda))
    where
      a = runCharacters olda

harm :: World -> IO World
harm olda = do
  _ <- index' a
  putStrLn "Choose Target"
  putStrLn ""
  int1 <- getInt
  putStrLn "Damage Amount"
  int2 <- getInt
  let characters = safemodifyat int1 (over (#status) healthupdate . over (#status) (damager int2)) a
  (ioEndTurn (set (#runCharacters) characters olda))
    where
      a = runCharacters olda

availableabilities :: Character -> [(StatusEffect, Temporal (Maybe Int))]
availableabilities c = ifoldMap go effects
    where
        gravimode = isActive . temporary $ view (#status . #effects . #gravitonmode) c
        effects = temporary <$> view (#status . #effects) c
        base = fst <$> view (#status . #equipment . #forcefield) c
        go :: StatusEffect -> Temporal (Maybe Int) -> [(StatusEffect, Temporal (Maybe Int))]
        go se t
            | se == Prone && (not $ isActive t) = [(se, (Present Nothing [] Nothing))]
            | se == Stealth && (not $ isActive t) && (charinvischeck c) = [(se, (Present Nothing [] Nothing))]
            | se == DefensiveStance && (not $ isActive t) = [(se, (Present (Just 2) [] Nothing))]
            | se == ForceField && (not $ isActive t) && (view (#status . #equipment . #forcefield) c /= Nothing) = [(se, (Present (fmap fst $ view (#status . #equipment . #forcefield) c) [] base))]
            | isOff t && se == DarkMatter && gravimode = [(se, on t)]
            | isOff t && se /= DarkMatter = [(se, on t)]
            | otherwise = []

disavailableabilities :: Character -> [(StatusEffect, Temporal (Maybe Int))]
disavailableabilities c = ifoldMap go effects
    where
        effects = temporary <$> view (#status . #effects) c
        go :: StatusEffect -> Temporal (Maybe Int) -> [(StatusEffect, Temporal (Maybe Int))]
        go se t
            | se == Prone && (isActive t) = [(se, (Absent Nothing [] Nothing))]
            | se == Stealth && (isActive t) = [(se, (Absent Nothing [] Nothing))]
            | (se == Grappled) && (isActive t) = [(se, (Absent Nothing [] Nothing))]
            | (se == DefensiveStance) && (isActive t) = [(se, (Absent Nothing [] Nothing))]
            | isOn t = [(se, off t)]
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

thosethatthegodswouldpunish :: World -> IO World
thosethatthegodswouldpunish olda = do
    _ <- putStrNLn alleffects
    int1 <- getInt
    let statuseffect =  safedoublexclam effectslist int1
    putStrLn "Choose Temporal: 0) NA, 1) Absent, 2) Off, 3) On, 4) Present, 5) Permanent"
    int2 <- getInt
    putStrLn "Choose Duration"
    int3 <- getInt
    putStrLn "Choose Character"
    string <- getLine
    putStrLn "Choose Modifyer"
    int4 <- getInt
    let madness = twtgwp statuseffect (selecttemporal2 int2 int3 string int4)
    _ <- index' a
    putStrLn "Choose Target"
    int5 <- getInt
    let newcharacters = safemodifyat int5 (over (#status) madness) a
    ioEndTurn (set (#runCharacters) newcharacters olda)
    where
      a = runCharacters olda

activateabilities :: World -> IO World
activateabilities olda = do
    let abilities1 = availableabilities a
    _ <- putStrNLn (go <$> (indexed $ fst <$> abilities1))
    int1 <- getInt
    let statuseffect = safedoublexclam abilities1 int1
    go1 statuseffect
        where
            (a :| as) = runCharacters olda
            go :: (Int, StatusEffect) -> String
            go (a1, Prone) = show a1 ++ ": " ++ "Get Down, "
            go (a1, b1) = show a1 ++ ": " ++ (show b1) ++ ", "
            go1 :: Maybe (StatusEffect, Temporal (Maybe Int)) -> IO World
            go1 Nothing = ioEndTurn olda
            go1 (Just tup)
              | (fst tup == ElementalBodyIII) || (fst tup == ElementalBodyIV) = do
                putStrLn "Choose Elemental: 1) Air, 2) Water, 3) Fire, 4) Earth"
                int <- getInt
                (ioEndTurn $ set (#runCharacters) (over (#status) (uncurry t2ix ((fst tup), ((\_ -> int) <$> (snd tup)))) a :| as) olda)
              | fst tup == Stigmata = do
                putStrLn "Choose Bonus: 1) Attacks, 2) Damage, 3) AC, 4) Caster Level, 5) Saves"
                int <- getInt
                (ioEndTurn $ set (#runCharacters) (over (#status) (uncurry t2ix ((fst tup), ((\_ -> int) <$> (snd tup)))) a :| as) olda)
              | fst tup == Ki = do
                putStrLn "Choose Bonus: 1) Attacks, 2) Defense"
                int <- getInt
                (ioEndTurn $ set (#runCharacters) (over (#status) (uncurry t2ix ((fst tup), ((\_ -> int) <$> (snd tup)))) a :| as) olda)
              | otherwise = ioEndTurn $ set (#runCharacters) (over (#status) (uncurry t2ix tup) a :| as) olda

disactivateabilities :: World -> IO World
disactivateabilities olda = do
    let abilities2 = disavailableabilities a
    _ <- putStrNLn (go <$> (indexed $ fst <$> abilities2))
    int1 <- getInt
    let statuseffect = safedoublexclam abilities2 int1
    go1 statuseffect
        where
            (a :| as) = runCharacters olda
            go :: (Int, StatusEffect) -> String
            go (a1, Prone) = show a1 ++ ": " ++ "Get Up, "
            go (a1, b1) = show a1 ++ ": " ++ (show b1) ++ ", "
            go1 :: Maybe (StatusEffect, Temporal (Maybe Int)) -> IO World
            go1 Nothing = ioEndTurn olda
            go1 (Just tup) = ioEndTurn $ set (#runCharacters) (over (#status) (uncurry t2ix tup) a :| as) olda

toggleabilities2 :: World -> IO World
toggleabilities2 olda = do
    let abilities2 = disavailableabilities a
    let abilities1 = availableabilities a
    _ <- putStrNLn (go (length abilities2) <$> (indexed $ fst <$> (abilities2 ++ abilities1)))
    int1 <- getInt
    let statuseffect = safedoublexclam (abilities2 ++ abilities1) int1
    go1 int1 (Just $ length abilities2) statuseffect
        where
            (a :| as) = runCharacters olda
            go :: Int -> (Int, StatusEffect) -> String
            go x (a1, Prone)
              | a1 > x = show a1 ++ ": " ++ "Get Down, "
              | otherwise = show a1 ++ ": " ++ "Get Up, "
            go _ (a1, b1) = show a1 ++ ": " ++ (show b1) ++ ", "
            go1 :: Maybe Int -> Maybe Int -> Maybe (StatusEffect, Temporal (Maybe Int)) -> IO World
            go1 _ _ Nothing = ioEndTurn olda
            go1 x y (Just tup)
              | x > y && ((fst tup == ElementalBodyIII) || (fst tup == ElementalBodyIV)) = do
                putStrLn "Choose Elemental: 1) Air, 2) Water, 3) Fire, 4) Earth"
                int <- getInt
                (ioEndTurn $ set (#runCharacters) (over (#status) (uncurry t2ix ((fst tup), ((\_ -> int) <$> (snd tup)))) a :| as) olda)
              | x > y && (fst tup == Stigmata) = do
                putStrLn "Choose Bonus: 1) Attacks, 2) Damage, 3) AC, 4) Caster Level, 5) Saves"
                int <- getInt
                (ioEndTurn $ set (#runCharacters) (over (#status) (uncurry t2ix ((fst tup), ((\_ -> int) <$> (snd tup)))) a :| as) olda)
              | x > y && (fst tup == Ki) = do
                putStrLn "Choose Bonus: 1) Attacks, 2) Defense"
                int <- getInt
                (ioEndTurn $ set (#runCharacters) (over (#status) (uncurry t2ix ((fst tup), ((\_ -> int) <$> (snd tup)))) a :| as) olda)
              | otherwise = ioEndTurn $ set (#runCharacters) (over (#status) (uncurry t2ix tup) a :| as) olda

toggleabilities :: Maybe Int -> World -> IO World
toggleabilities int1 olda = do
    let abilities2 = disavailableabilities a
    let abilities1 = availableabilities a
    _ <- putStrNLn (go (length abilities2) <$> (indexed $ fst <$> (abilities2 ++ abilities1)))
    let statuseffect = safedoublexclam (abilities2 ++ abilities1) int1
    go1 int1 (Just $ length abilities2) statuseffect
        where
            rangebonus = view (#strength) . abilitybonuses . view (#status) $ a
            babbonus = temporary $ view (#status . #primclass . #bab) a
            miscbonus = temporary . sum . view (#miscclass . #bab) . gatherbonuses . view (#status) $ a
            grappledifficulty = case viewtempmodifyier . temporary . view (#status . #effects . #grappled) $ a of
              Nothing -> 0
              Just int -> int
            (a :| as) = runCharacters olda
            go :: Int -> (Int, StatusEffect) -> String
            go x (a1, Prone)
              | a1 > x = show a1 ++ ": " ++ "Get Down, "
              | otherwise = show a1 ++ ": " ++ "Get Up, "
            go _ (a1, b1) = show a1 ++ ": " ++ (show b1) ++ ", "
            go1 :: Maybe Int -> Maybe Int -> Maybe (StatusEffect, Temporal (Maybe Int)) -> IO World
            go1 _ _ Nothing = ioEndTurn olda
            go1 x y (Just tup)
              | x <= y && (fst tup == Grappled) = do
                g <-newStdGen
                let roll20 = rangebonus + babbonus + miscbonus + (view (#runRoll) . fst $ roll 20 g)
                let newa = if roll20 >= grappledifficulty then over (#runGameText) (++ [show roll20 ++ "  Success vs " ++ show grappledifficulty]) olda else over (#runGameText) (++ [show roll20 ++ "  Failure vs " ++ show grappledifficulty]) olda
                if roll20 >= grappledifficulty then ioEndTurn $ set (#runCharacters) (over (#status) (uncurry t2ix tup) a :| as) newa else ioEndTurn newa
              | x <= y && (fst tup == Stealth) && (isActive . snd $ tup)= do
                g <-newStdGen
                let roll20 = fst . fst . skillcheck g (#skills . #stealth) (#dexterity) (#skills . #stealth) $ a
                g2 <-newStdGen
                let rolls20 = perceptionCheck g2 a (a:|as)
                let seechars = filter (\(x,y) -> x >= roll20) rolls20
                let newa = if all (\(x, y) -> roll20 > x) rolls20 then over (#runGameText) (++ [show roll20 ++ "  Success vs"] ++ (fmap (\(x,y) -> show x ++ " " ++ (view (#name) y)) rolls20)) olda else over (#runGameText) (++ [show roll20 ++ "  Failure vs"] ++ (fmap (\(x,y) -> show x ++ " " ++ (view (#name) y)) seechars)) olda
                if all (\(x, y) -> roll20 > x) rolls20 then ioEndTurn $ set (#runCharacters) (over (#status) (uncurry t2ix (fmap (setmodifyer (Just roll20)) tup)) a :| as) newa else ioEndTurn newa 
              | x > y && ((fst tup == ElementalBodyIII) || (fst tup == ElementalBodyIV)) && (isActive . snd $ tup) = do
                putStrLn "Choose Elemental: 1) Air, 2) Water, 3) Fire, 4) Earth"
                return $ set (#runGameMode) ToggleMode $ set (#runTarget) [int1] $ set (#runGameMenu . #runMenuText) ["Choose Elemental", "1) Air", "2) Water", "3) Fire", "4) Earth"] olda                
              | x > y && (fst tup == Stigmata) && (isActive . snd $ tup) = do
                putStrLn "Choose Bonus: 1) Attacks, 2) Damage, 3) AC, 4) Caster Level, 5) Saves"
                _ <- putStrLn $ show int1
                return $ set (#runGameMode) ToggleMode $ set (#runTarget) [int1] $ set (#runGameMenu . #runMenuText) ["Choose Bonus", "1) Attacks", "2) Damage", "3) AC", "4) Caster Level", "5) Saves"] olda
              | x > y && (fst tup == Ki) && (isActive . snd $ tup) = do
                putStrLn "Choose Bonus: 1) Attacks, 2) Defense"
                return $ set (#runGameMode) ToggleMode $ set (#runTarget) [int1] $ set (#runGameMenu . #runMenuText) ["Choose Bonus", "1) Attacks", "2) Defense"] olda                
              | otherwise = ioEndTurn $ set (#runCharacters) (over (#status) (uncurry t2ix tup) a :| as) olda

charinvischeck :: Character -> Bool
charinvischeck char = (not $ view (#inCombat) char)  || 
  (isActive . temporary . view (#status . #effects . #invisible) $ char)  || 
  (isActive . temporary . view (#status . #effects . #hips) $ char)

perceptionCheck :: StdGen -> Character -> NonEmpty Character -> [(Int, Character)]
perceptionCheck g char chars@(a:|as) = fst $ runState (perceptioncheck ~> charlist) g
  where
    perceptioncheck = skilldice (#skills . #stealth) (#dexterity) (#skills . #stealth)
    charlist = if percievebool a then a : (filter percievebool as) else (filter percievebool as)
    percievebool char' = ((temporary $ view (#status . #team) char') /= (temporary $ view (#status . #team) char)) && 
      ((view (#runLocation) char) `elem` (view (#immediateVision) char')) && (not . ishelpless $ char')

changeequip :: World -> IO World
changeequip olda = do
    let equipment' = view (#status . #otherweapons) a
    putStrLn $ join (go1 <$> (indexed equipment'))
    int1 <- getInt
    let selection = safedoublexclam equipment' int1
    let newcharacter = maybe a (either (\x -> changeitem x a) (\x -> changeweapons (Just x) a)) selection
    ioEndTurn $ set (#runCharacters) (go selection newcharacter :| as) olda
        where
            (a :| as) = runCharacters olda
            go :: Maybe (Either (MagicItem Int) (Either (Armour Int) Weapon)) -> Character -> Character
            go selection c = case selection of
                Nothing -> c
                Just x -> if (view (#inCombat) c && (either (const True) (either (const True) (const False)) x)) || 
                  (isActive . temporary . view (#status . #effects . #disarmed) $ c) then c else over (#status . #otherweapons) (delete x) c
            go1 :: (Int, Either (MagicItem Int) (Either (Armour Int) Weapon)) -> String
            go1 (a1, b) = show a1 ++ ": " ++ (either miname (either aname wname) b) ++ ", "
            wname weapon = view (#name) weapon
            aname armour =  view (#name) armour
            miname mitem = view (#name) mitem

changeequip' :: Maybe Int -> World -> IO World
changeequip' int1 olda = do
    let equipment' = view (#status . #otherweapons) a
    putStrLn $ join (go1 <$> (indexed equipment'))
    let selection = safedoublexclam equipment' int1
    let newcharacter = maybe (removeequip a) (either (\x -> changeitem x a) (\x -> changeweapons (Just x) a)) selection
    return . set (#runGameMenu) (EquipmentMenu (go1 <$> (indexed . view (#status . #otherweapons) $ (go selection newcharacter)))) . set (#runTarget) [] . set (#runCharacters) ((go selection newcharacter) :| as) $ olda
        where
            (a :| as) = runCharacters olda
            go :: Maybe (Either (MagicItem Int) (Either (Armour Int) Weapon)) -> Character -> Character
            go selection c = case selection of
                Nothing -> c                
                Just x -> if (view (#inCombat) c && (either (const True) (either (const True) (const False)) x)) || 
                  (isActive . temporary . view (#status . #effects . #disarmed) $ c) then c else over (#status . #otherweapons) (delete x) c
            go1 :: (Int, Either (MagicItem Int) (Either (Armour Int) Weapon)) -> String
            go1 (a1, b) = show a1 ++ ": " ++ (either miname (either aname wname) b) ++ ", "
            wname weapon = view (#name) weapon
            aname armour =  view (#name) armour
            miname mitem = view (#name) mitem

mObjectToEquip :: Maybe MazeNode -> [Either (Either (MagicItem Int) SpellTemplate) (Either (Armour Int) Weapon)]
mObjectToEquip mmb = case mmb of
  Nothing -> []
  Just y -> case view (#runObject) y of
    Nothing -> []
    Just (Object _ xs) -> xs
    _ -> []

removefromobject :: Maybe (Either (Either (MagicItem Int) SpellTemplate) (Either (Armour Int) Weapon)) -> Location -> Maze -> Maze
removefromobject mitem loc m = case mitem of
    Nothing -> m
    Just y -> MAP.adjust (overcontents (delete $ y)) loc m

takeequip' :: Maybe Int -> World -> IO World
takeequip' int2 olda = do
  putStrLn . join $ textterminal1
  return $ set (#runGameMenu . #runMenuText) textterminal1 newworld
    where
      newworld = over (#runMaze) (removefromobject mitem getLocation) 
        $ set (#runCharacters) (safemodifyat int1 (removefromequipment mitem) 
        $ addtoequipment mitem a :| as) olda
      newmchar = runCharacters newworld !!? int1
      mchar = (a :| as) !!? int1
      getLocation = case getEquipmentTarget . runGameMode $ olda of
        Nothing -> (0, 0)
        Just y -> y
      mitem
        | int1 == Nothing = (mObjectToEquip . MAP.lookup getLocation $ runMaze olda) `safedoublexclam` int2
        | otherwise = mcharToEquip mchar `safedoublexclam` int2
      textterminal1
        | int1 == Nothing = go1 <$> (indexed . mObjectToEquip . MAP.lookup getLocation $ runMaze newworld)
        | otherwise = go1 <$> (indexed $ mcharToEquip newmchar)
      int1 = case runTarget olda of
          [] -> Nothing
          (a : _) -> a
      (a :| as) = runCharacters olda
      go1 :: (Int, Either (Either (MagicItem Int) SpellTemplate) (Either (Armour Int) Weapon)) -> String
      go1 (a1, b) = show a1 ++ ": " ++ (either (either miname show) (either aname wname) b) ++ ", "
      wname weapon = view (#name) weapon
      aname armour =  view (#name) armour
      miname mitem = view (#name) mitem

abilities :: Maybe Int -> (World -> IO World)
abilities (Just 1) = harm
abilities (Just 2) = thosethatthegodswouldpunish
abilities (Just 3) = punish
abilities (Just 4) = aoo
abilities (Just 5) = toggleabilities2
abilities (Just 6) = changerp
abilities _ = toggleabilities2

changerp :: World -> IO World
changerp olda = do
  _ <- index' a
  putStrLn "Choose Target"
  putStrLn ""
  int1 <- getInt
  putStrLn "Damage Amount"
  int2 <- getInt
  let characters = safemodifyat int1 (over (#status) healthupdate . over (#status) (damager' (#resolve) int2)) a
  (ioEndTurn (set (#runCharacters) characters olda))
    where
      a = runCharacters olda

chooseAbilities :: World -> IO World
chooseAbilities a = do
  putStrLn "Choose Action: 1) Harm, 2) Inflict, 3) Punish, 4) AOO, 5) Toggle, 6) Chang RP"
  putStrLn ""
  int1 <- getInt
  (abilities int1 a)

mcharToEquip :: Maybe Character -> [Either (Either (MagicItem Int) SpellTemplate) (Either (Armour Int) Weapon)]
mcharToEquip mchar = case mchar of
  Nothing -> []
  Just y -> if view (#status . #health) y == Dead then
    fmap go (view (#status . #otherweapons) y) ++ fmap (Left . Right) (view (#status . #itemspells) y) ++ 
    [Right . Right $ view (#status . #primaryhand) y] ++ go1 y ++ go3 y
    else fmap go (view (#status . #otherweapons) y) ++ fmap (Left . Right) (view (#status . #itemspells) y)
  where
    go :: Either (MagicItem Int) (Either (Armour Int) Weapon) -> Either (Either (MagicItem Int) SpellTemplate) (Either (Armour Int) Weapon)
    go emeaw = case emeaw of
      Left z -> Left $ Left z
      Right z -> Right z
    go1 x = [Right . Left $ view (#status . #equipedarmour) x]
    go3 m = fmap (Left . Left) . catMaybes . foldr (\x y -> x : y) [] $ view (#status . #equipment) m

ioEndTurn :: World -> IO World
ioEndTurn olda
  | not incombat = return $ set (#runGameMenu) (MainMenu  mainmenutext) olda
  | otherwise = return $ set (#runGameMenu) (EndTurnMenu ["End Turn", "1) Yes", "2) No", "0) EndCombat"]) olda
    where
      a = view (#runCharacters) olda
      incombat = inCombat olda

cancelTarget :: World -> IO World
cancelTarget olda
  | not incombat = return . set (#runGameMode) Map . set (#runGameMenu) (MainMenu  mainmenutext) $ olda
  | otherwise = return . set (#runGameMode) Map . set (#runGameMenu) (ActionMenu actionmenutext) $ olda
    where
      a = view (#runCharacters) olda
      incombat = inCombat olda

windowDisplay :: Display
windowDisplay = InWindow "Window" (200, 200) (10, 10)

skillmenutext :: [String]
skillmenutext = ["Choose Skill", "1) Acrobatics", "2) Athletics", "3) Bluff ", "4) Computers", "5) Culture", "6) Diplomacy", "7) Disguise", "8) Engineering", "9) Intimidate", "10) Life Science"]

skillmenutext2 :: [String]
skillmenutext2 = ["Choose Skill", "11) Medicine", "12) Mysticism", "13) Perception", "14) Physical Sciences", "15) Piloting", "16) Profession", "17) Sense Motive", "18) Sleight of Hand", "19) Stealth", "20) Survival"]

mainmenutext :: [String]
mainmenutext = ["Choose Activity", "1) Player Action", "2) Regain Stamina", "3) Rest ", "4) Pass Time (Terminal)", "5) Save", "6) Add Ally (Terminal)", "7) Start Combat", "8) Change Map", "9) Toggle DMMode"]

actionmenutext :: [String]
actionmenutext = ["Choose Action", "1) Attack", "2) Cast Spell", "3) Toggle Ability", "4) Equipment", "5) Get Equipment", "6) Combat maneuver", "7) Take Cover", "8) End Turn", "9) GM Abilities", "10) Skills"]

setalliesinCombat :: NonEmpty Character -> NonEmpty Character
setalliesinCombat chars = nemodifywhile (set (#inCombat) True) (\x -> Ally == (temporary $ view (#status . #team) x)) chars

startCombat :: World -> IO World
startCombat world = do
  putStrLn $ show targets
  startgame $ set (#runCharacters) newchars world
  where
    targets = runTarget world
    chars = runCharacters world
    newchars = go targets chars
    go :: [Maybe Int] -> NonEmpty Character -> NonEmpty Character
    go [] nechar = nechar
    go (mi : mis) nechar = go mis $ safemodifyat mi (set (#inCombat) True) nechar

startgame :: World -> IO World
startgame olda = do
  g <- newStdGen
  let (message,newa) = runState (startturn g (view (#runMaze) olda)) (initiativefinal g a)
  putStrLn $ show (view (#name) $ NE.head newa) ++ "'s Turn"
  putStrLn $ (detailedview . NE.head) newa
  return $ (over (#runGameText) (++ message) . set (#runGameMenu) (ActionMenu actionmenutext) . set (#runGameMode) Map . set (#inCombat) True . set (#runCharacters) newa) olda
    where
      a = runCharacters olda

settime :: IO (Time Int)
settime =  do
  putStrLn "Enter Time: Rounds, Minutes, Hours, Days"
  int1 <- getInt
  int2 <- getInt
  int3 <- getInt
  int4 <- getInt
  return $ Time (maybeint2int int1) (maybeint2int int2) (maybeint2int int3) (maybeint2int int4)

resettime :: World -> IO World
resettime a = do
  time <- settime
  return $ set (#runTime) time a

mcharToChar :: Character -> [Character] -> NonEmpty Character
mcharToChar car nmc = case nmc of
  [] -> (car :| [])
  (a : as) -> a :| as

defaultBMP :: BMP
defaultBMP = packRGBA32ToBMP32 0 0 empty

chooseTeam :: IO World
chooseTeam = do
  b <- loadBMP2 "BackgroundFinall.bmp"
  c <- loadBMP2 "BackgroundFinall2.bmp"
  putStrLn "1) Choose Team Manually, 2) Load Characters"
  int <- getInt
  case int of
    Just 2 -> do
      loadedteam <- (loadcharacters $ World testgame ((0.0,0.0),(0.0,0.0),False) (0,0) 0 Map [Nothing] time (MainMenu mainmenutext) [] False (bitmapOfBMP  $ either (const defaultBMP) id b) (bitmapOfBMP  $ either (const defaultBMP) id c) [] maze [] (Just Dark) Nothing Nothing)
      return . over (#runGameText) (++ ["Test"]) $ setallyimmediatevsion loadedteam
    _ -> chooseTeam' $ World testgame ((0.0,0.0),(0.0,0.0),False) (0,0) 0 Map [Nothing] time (MainMenu mainmenutext) [] False (bitmapOfBMP  $ either (const defaultBMP) id b) (bitmapOfBMP  $ either (const defaultBMP) id c) [] maze [] (Just Dark) Nothing Nothing
    where
      time = (Time 0 0 0 0)

chooseTeam' :: World -> IO World
chooseTeam' olda = do
  index' (a :| as)
  putStrLn "Choose Team"
  int1 <- getInts []
  let characters = catMaybes $ fmap (\x -> (a :| as) !!? x) int1
  return $ over (#runCharacters) (\xs -> NE.map (\x -> if temporary (view (#status . #team) x) == Ally then set (#immediateVision) (fst $ charactervisionfield (brighttime (Just Dark) (Time 0 0 0 0)) maze x xs) x else id x) xs) . set (#runCharacters) (mcharToChar a characters) $ olda
  where
    (a :| as) = runCharacters olda

addAlly :: World -> IO World
addAlly olda = do
  index' testgame2
  pic <- loadBMP "DefaultToken.bmp"
  putStrLn "Choose Team"
  int1 <- getInts []
  let characters = randomizeMonsterLocation pic olda . catMaybes . fmap (\x -> testgame2 !!? x) $ int1
  return $ set (#runCharacters) (a :| (as ++ characters)) olda
  where
    (a :| as) = runCharacters olda

timepasser :: World -> IO World
timepasser olda = do
  putStrLn "Enter Time in Minutes"
  int1 <- getInt
  g <- newStdGen
  g1 <- newStdGen
  g2 <- newStdGen
  g3 <- newStdGen
  let newa = timepasser' a int1 g g1 g2 g3
  return $ (set (#runCharacters) newa . addrounds2 (maybeint2int int1 * 10) . over (#runMaze) (fmap $ alterduration (maybeint2int . fmap (10*) $ int1))) olda
  where
    time = view (#runTime) olda
    a = runCharacters olda

timepasserprime :: Maybe Int -> World -> IO World
timepasserprime int1 olda = do
  g <- newStdGen
  g1 <- newStdGen
  g2 <- newStdGen
  g3 <- newStdGen
  let newa = timepasser' a int1 g g1 g2 g3
  return $ (set (#runCharacters) newa . addrounds2 (maybeint2int int1 * 10) . over (#runMaze) (fmap $ alterduration (maybeint2int . fmap (10*) $ int1))) olda
  where
    time = view (#runTime) olda
    a = runCharacters olda

timepassernew :: Int -> StdGen -> StdGen -> StdGen -> StdGen -> World -> World
timepassernew int g1 g2 g3 g4 olda = over (#runCharacters) newcharsfun . addrounds2 (int * 10) . over (#runMaze) altermazefun $ olda
  where
    newcharsfun = (\x -> timepasser' x (Just int) g1 g2 g3 g4)
    altermazefun = fmap $ alterduration (10 * int)

timepasser' :: NonEmpty Character -> Maybe Int -> StdGen -> StdGen -> StdGen -> StdGen -> NonEmpty Character
timepasser' a int1 g g1 g2 g3 = newa3
  where
    newa = snd $ runState (doStuffWithCharacter' g) a
    newa1 = snd $ runState (doStuffWithCharacter' g1) newa
    newa2 = snd $ runState (doStuffWithCharacter' g2) newa1
    newa3 = snd $ runState (passtime3 g3 int1) newa2

restnew :: World -> IO World
restnew a = do
  case int1 of
    Nothing -> rest' a
    _ -> monsterestnew a
    where
      int1 = view (#runMonsterRest) a

rest' :: World -> IO World
rest' olda = do
  putStrLn "You awake feeling refressed!"
  g <- newStdGen
  g1 <- newStdGen
  g2 <- newStdGen
  g3 <- newStdGen
  let newa = timepasser' (newresteveryone a) (Just 480) g g1 g2 g3
  return $ over (#runGameText) (++ ["You awake feeling refreshed!"]) . set (#runCharacters) newa . addrounds2 4800 . over (#runMaze) (fmap $ alterduration 4800) $ olda
  where
    time = view (#runTime) olda
    a = runCharacters olda

monsterestnew :: World -> IO World
monsterestnew olda = do
  g <- newStdGen
  pic1 <- loadBMP "DefaultToken.bmp"
  let (roll1, g2) = roll 35 g
  let (roll2, g3) = roll 100 g2
  g4 <- newStdGen
  g5 <- newStdGen
  g6 <- newStdGen
  g7 <- newStdGen
  go g3 g4 g5 g6 g7 int1 roll1 (runRoll roll2) pic1
  where
    int1 = view (#runMonsterRest) olda
    time = view (#runTime) olda
    night = (view (#hours) time) >= 21 || (view (#hours) time) < 3
    daynight :: Int -> Int -> Int -> Bool -> Bool
    daynight inta intb intc bool
      | bool = intb >= intc
      | otherwise = inta >= intc
    go gen gen2 gen3 gen4 gen5 mint roll' int pic
        | mint == (Just 0) && daynight 25 40 int night = do
          survivalcheck3 (Just 17) (timepassernew 60 gen2 gen3 gen4 gen5 olda) (randomizeMonsterLocation pic olda $ forrestencounters35 (runRoll roll') gen)
        | mint == (Just 1) && daynight 40 40 int night = do
          survivalcheck3 (Just 17) (timepassernew 60 gen2 gen3 gen4 gen5 olda) (randomizeMonsterLocation pic olda $ underground35 (runRoll roll') gen)
        | mint == (Just 2) && daynight 25 40 int night = do
          survivalcheck3 (Just 21) (timepassernew 60 gen2 gen3 gen4 gen5 olda) (randomizeMonsterLocation pic olda $ isleofterror35 (runRoll roll') gen)
        | mint == (Just 3) && daynight 40 60 int night = do
          monsters <- selectmonsters'
          survivalcheck3 (Just 21) (timepassernew 60 gen2 gen3 gen4 gen5 olda) (randomizeMonsterLocation pic olda . fst $ runState (randomizehealthandscores2 ~> monsters) gen)
        | otherwise = rest' $ over (#runGameText) (++ ["No Monsters Tonight"]) olda

randomizeMonsterLocation :: Picture -> World -> [Character] -> [Character]
randomizeMonsterLocation pic world monsters = fmap (set (#runLocation) (meanx, meany) . set (#runPicture) pic) monsters
  where
    (a :| as) = view (#runCharacters) world
    otherallies = filter (\x -> ((temporary . view (#status . #team)) $ x) == Ally) as
    allies = if ((temporary . view (#status . #team)) $ a) == Ally then a : otherallies else otherallies
    allylocations = fmap runLocation allies
    allyxs = fmap fst allylocations
    allyys = fmap snd allylocations
    meanx = case allyxs of
      [] -> 0
      xs -> (sum xs) `div` (length xs)
    meany = case allyys of
      [] -> 0
      xs -> (sum xs) `div` (length xs)

selectmonsters :: World -> IO World
selectmonsters olda = do
  putStrLn "Select Monster"
  pic <- loadBMP "DefaultToken.bmp"
  _ <- index' enemies
  int1 <- getInts []
  let monsters = catMaybes $ fmap (\x -> enemies !!? x) int1
  newmonsters <- (multiplymonsters .  fmap (set (#inCombat) True) $ randomizeMonsterLocation pic olda monsters)
  startgame $ set (#runCharacters) (setalliesinCombat $ a :| (as ++ newmonsters)) olda
  where
    (a :| as) = view (#runCharacters) olda

selectmonsters' :: IO [Character]
selectmonsters' = do
  putStrLn "Select Monster"
  _ <- index' enemies
  int1 <- getInts []
  let monsters = catMaybes $ fmap (\x -> enemies !!? x) int1
  newmonsters <- multiplymonsters monsters
  return newmonsters

multiplymonsters :: [Character] -> IO [Character]
multiplymonsters [] = return []
multiplymonsters (a : as) = do
  putStrLn (showc a)
  putStrLn "Choose Number"
  int1 <- getInt
  let newas = gamemaker a (maybeint2int int1)
  putStrLn "Finished: 1) Yes, 2) No"
  int2 <- getInt
  g <- newStdGen
  case int2 of
    (Just 1) -> return . fst $ runState (randomizehealthandscores2 ~> (newas ++ as)) g
    _ -> multiplymonsters (as ++ newas)

randomenemies2new :: Maybe Int -> World -> IO World
randomenemies2new int1 olda = do
  g <- newStdGen
  pic1 <- loadBMP "DefaultToken.bmp"
  let (roll1, g2) = roll 35 g
  let (roll2, g3) = roll 100 g2
  go g3 int1 roll1 (runRoll roll2) pic1
  where
    time = view (#runTime) olda
    night = (view (#hours) time) >= 21 || (view (#hours) time) < 3
    daynight :: Int -> Int -> Int -> Bool -> Bool
    daynight inta intb intc bool
      | bool = intb >= intc
      | otherwise = inta >= intc
    go gen mint roll' int pic
        | mint == (Just 0) && daynight 10 40 int night = do
          survivalcheck3 (Just 17) olda (randomizeMonsterLocation pic olda $ forrestencounters35 (runRoll roll') gen)
        | mint == (Just 1) && daynight 40 40 int night = do
          survivalcheck3 (Just 17) olda (randomizeMonsterLocation pic olda $ underground35 (runRoll roll') gen)
        | mint == (Just 2) && daynight 25 40 int night = do
          survivalcheck3 (Just 21) olda (randomizeMonsterLocation pic olda $ isleofterror35 (runRoll roll') gen)
        | mint == Nothing = return $ over (#runGameText) (++ ["No Monsters in this Area"]) olda
        | otherwise = return $ addrounds2 600 . over (#runGameText) (++ ["No Monsters Arround"]) $ olda

randomenemiesnew :: Maybe Int -> World -> IO World
randomenemiesnew int1 olda = do
  g <- newStdGen
  pic1 <- loadBMP "DefaultToken.bmp"
  let (roll1, g2) = roll 35 g
  go g2 int1 roll1 pic1
  where
    (a :| as) = view (#runCharacters) olda
    go gen mint roll' pic = case mint of
      Just 0 -> do
        _ <- index' (a :| (as ++ (forrestencounters35 (runRoll roll') gen)))
        startgame $ set (#runCharacters) (setalliesinCombat $ a :| (as ++ (fmap (set (#inCombat) True) $ randomizeMonsterLocation pic olda $ forrestencounters35 (runRoll roll') gen))) olda
      Just 2 -> do
        _ <- index' (a :| (as ++ (isleofterror35 (runRoll roll') gen)))
        startgame $ set (#runCharacters) (setalliesinCombat $ a :| (as ++ (fmap (set (#inCombat) True) $ randomizeMonsterLocation pic olda $ isleofterror35 (runRoll roll') gen))) olda
      Just 1 -> do
        _ <- index' (a :| (as ++ (underground35 (runRoll roll') gen)))
        startgame $ set (#runCharacters) (setalliesinCombat $ a :| (as ++ (fmap (set (#inCombat) True) $ randomizeMonsterLocation pic olda $ underground35 (runRoll roll') gen))) olda
      _ -> return $ over (#runGameText) (++ ["No Monsters in this Area"]) olda

survivalcheck3 :: Maybe Int -> World -> [Character] -> IO World
survivalcheck3 _ as [] = do
  return as
survivalcheck3 int1 as bs = do
  g <- newStdGen
  let int2 = Just . fst . fst $ skillcheck g (#skills . #survival) (#wisdom) (#skills . #survival) (NE.head . runCharacters $ as)
  go as bs int1 int2
    where
      go :: World -> [Character] -> Maybe Int -> Maybe Int -> IO World
      go world bs' int1' int2'
        | int2' > int1' = rest' $ over (#runGameText) (++ ["Success! " ++ show int2' ++ " vs DC " ++ show int1']) world
        | otherwise = do
          startgame . over (#runGameText) (++ ["Ambushed! " ++ show int2' ++ " vs DC " ++ show int1']) . set (#runCharacters) (setalliesinCombat $ a' :| ((fmap (set (#inCombat) True) bs') ++ as')) $ world
          where
            (a' :| as') = view (#runCharacters) world
            time' = view (#runTime) world

maintest :: Show a => a -> IO ()
maintest a = do 
       outh <- openFile "character.txt" WriteMode
       mainloop a outh
       hClose outh

mapfilepath :: String -> String -> IO ()
mapfilepath name savefile = do 
       outh <- openFile name WriteMode
       hPrint outh savefile
       hClose outh

mainloop :: Show a => a -> Handle -> IO ()
mainloop a outh = hPrint outh (show a)

loadloop :: Handle -> IO (String)
loadloop inh = hShow inh

loadcharacters :: World -> IO World
loadcharacters olda  = do
        inh <- openFile "character.txt" ReadMode
        string1 <- hGetContents inh
        newa <- parseT load string1
        let finala = (go $ fst <$> newa)
        hClose inh
        return $ set (#runCharacters) finala olda
        where
          a = view (#runCharacters) olda
          go ma = case ma of
            Nothing -> a
            Just y -> y

loadmap :: String -> World -> IO World
loadmap string olda  = do
        inh <- openFile string ReadMode
        string1 <- hGetContents inh
        newa <- parseT mazeparse string1
        let finala = (go $ fst <$> newa)
        hClose inh
        return $ set (#runMaze) finala olda
        where
          a = view (#runMaze) olda
          go ma = case ma of
            Nothing -> a
            Just y -> y

loadbackground :: String -> IO [Location]
loadbackground string  = do
        inh <- openFile string ReadMode
        string1 <- hGetContents inh
        newa <- parseT backgroundparse string1
        let finala = (go $ fst <$> newa)
        hClose inh
        return $ finala
        where
          go ma = case ma of
            Nothing -> []
            Just y -> y

loadenemies :: String -> IO [Character]
loadenemies string  = do
        inh <- openFile string ReadMode
        string1 <- hGetContents inh
        newa <- parseT characterlist string1
        let finala = (go $ fst <$> newa)
        hClose inh
        return $ finala
        where
          go ma = case ma of
            Nothing -> []
            Just y -> y

savecharacters :: World -> IO World
savecharacters olda = do
  maintest a
  putStrLn "Characters Saved!"
  ioEndTurn olda
  where
    a = view (#runCharacters) $ olda

savemap :: String -> World -> IO World
savemap name olda = do
  mapfilepath name (show . MAP.toList $ a)
  putStrLn "Map Saved!"
  ioEndTurn olda
  where
    a = view (#runMaze) olda

takecover :: World -> IO World
takecover olda = do
  putStrLn "Take Cover : 1) Partial (20%), 2) Full (50%), 3) Complete (100%), 4) Leave Cover (0%), 5) Cancel"
  int1 <- getInt
  ioEndTurn $ set (#runCharacters) (covercharacter int1 a :| as) olda
  where
    (a :| as) = view (#runCharacters) olda

covercharacter :: Maybe Int -> Character -> Character
covercharacter mi c = case mi of
  Just 1 -> over (#status) (t2ix Cover (Present (Just 2) [] (Just 20))) c
  Just 2 -> over (#status) (t2ix Cover (Present (Just 2) [] (Just 50))) c
  Just 3 -> over (#status) (t2ix Cover (Present (Just 2) [] (Just 100))) c
  Just 4 -> over (#status) (t2ix Cover (Absent Nothing [] Nothing)) c
  _ -> c

presetencounter2 :: StdGen -> Maybe Int -> [Character]
presetencounter2 g mi = case mi of
  (Just 0) -> (fst $ runState (randomizehealthandscores2 ~> 
    ([Character "Rogi" (0,0) (0,0) defaultPicture  False False False False (0,0) skulk [] []] ++ 
    [Character "Zahkar" (0,0) (0,0) defaultPicture  False False False False (0,0) skulk [] []])) g)
  (Just 1) -> (fst $ runState (randomizehealthandscores2 ~> 
    ((gamemaker (Character "Town Folk" (0,0) (0,0) defaultPicture  False False False False (0,0) townfolk [] []) 4) ++ 
    (gamemaker (Character "Pochteca Guard" (0,0) (0,0) defaultPicture  False False False False (0,0) tabaxianguard [] []) 2))) g)
  (Just 2) -> (fst $ runState (randomizehealthandscores2 ~> 
    ((gamemaker (Character "Town Folk" (0,0) (0,0) defaultPicture  False False False False (0,0) townfolk [] []) 5) ++ 
    [set (#status . #effects . #bleeding) (pure $ Present (Just 1) [] Nothing) . set (#status . #damage) (pure 11 :: Temporary Int)
    $ Character "Pazoh" (0,0) (0,0) defaultPicture  False False False False (0,0) tabaxianguard [] []])) g)
  (Just 3) -> (fst $ runState (randomizehealthandscores2 ~> 
    ((gamemaker (Character "Guild Thug" (0,0) (0,0) defaultPicture  False False False False (0,0) guildthug [] []) 4) ++ 
    (gamemaker guildrogue 2))) g)

presetencountermenutext :: [String]
presetencountermenutext = ["0) Skulk", "1) Rough Times", "2) Poor Kid", "3) Guild"]

changeMapDM :: World -> IO World
changeMapDM world = do
  linea <- getLine
  lineb <- getLine
  b <- loadBMP linea
  c <- loadBMP lineb
  return . set (#runBrightness) Nothing . set (#explore) [] . over (#runCharacters) (go <$>) . 
    set (#backGround) b . set (#backGroundEx) c . set (#runMaze) (MAP.fromList [])  $ world
  where
  go :: Character -> Character
  go character = set (#runLocation) (0, 0) character

changeMap :: Maybe Int -> World -> IO World
changeMap mint world = do
  b <- loadBMP linea
  c <- loadBMP lineb
  d <- loadmap (linec mint) world
  _ <- savemap (linec (view (#runWorldNumber) world)) world
  _ <- save (linei (view (#runWorldNumber) world)) (show . explore $ world)
  newexplore <- loadbackground (linei mint)
  enemies <- loadenemies (lineh mint)
  pic <- loadBMP "DefaultToken.bmp"
  _ <- save (lineh (view (#runWorldNumber) world)) (show . getenemies . runCharacters $ world) 
  return . setallyimmediateviison2 . set (#runWorldNumber) mint . set (#runBrightness) lined . 
    over (#runCharacters) (go1 (fmap (set (#runPicture) pic) enemies) . fmap go) . set (#backGround) b . set (#backGroundEx) c . 
    set (#runGameMenu) (MainMenu mainmenutext) . over (#runMaze) (illuminate lineg . fmap (set (#runIllumination) lineg)) . 
    set (#runMonsterRest) linef . set (#explore) newexplore . set (#runCharacters) (enemiesremoved . runCharacters $ world) .
    set (#runGlobalOffSet) (tupleFunctionnew (int2Float . (\x -> (-70) * x)) linee)
    $ d
  where
  tupleFunctionnew :: (a -> b) -> (a,a) -> (b,b)
  tupleFunctionnew f (x,y) = (f x, f y)
  getenemies :: NonEmpty Character -> [Character]
  getenemies (a:|as) = if not (view (#pC) a) then a : filter (\x -> not $ view (#pC) x) as 
    else filter (\x -> not $ view (#pC) x) as
  enemiesremoved :: NonEmpty Character -> NonEmpty Character
  enemiesremoved (a:|as) = case filter (\x -> view (#pC) x) as of
    [] -> a:|[]
    (x:xs) -> if  view (#pC) x then a :| (x:xs) else x :| xs
  go :: Character -> Character
  go character = set (#runLocation) linee character
  go1 :: [Character] -> NonEmpty Character -> NonEmpty Character
  go1 ens (headchar:|chars) = headchar :| (chars ++ ens)
  linea
    | mint == (Just 1) = "Untitled.bmp"
    | mint == (Just 2) = "Traveler'sInn.bmp"
    | mint == (Just 0) = "Untitled.bmp"
    | mint == (Just 3) = "ForrestPath.bmp"
    | mint == (Just 4) = "WillowInn.bmp"
    | mint == (Just 5) = "Tents.bmp"
    | mint == (Just 6) = "ShopMurder.bmp"
    | mint == (Just 7) = "Manor.bmp"
    | mint == (Just 8) = "MagatiorFacient.bmp"
    | otherwise = "BackgroundFinall.bmp"
  lineb
    | mint == (Just 1) = "Untitled2.bmp"
    | mint == (Just 2) = "Traveler'sInn2.bmp"
    | mint == (Just 0) = "Untitled2.bmp"
    | mint == (Just 3) = "ForrestPath2.bmp"
    | mint == (Just 4) = "WillowInn2.bmp"
    | mint == (Just 5) = "Tents2.bmp"
    | mint == (Just 6) = "ShopMurder2.bmp"
    | mint == (Just 7) = "Manor2.bmp"
    | mint == (Just 8) = "MagatiorFacient2.bmp"
    | otherwise = "BackgroundFinall2.bmp"
  linec mint2
    | mint2 == (Just 1) = "ForrectGlenn.txt"
    | mint2 == (Just 2) = "Innmap.txt"
    | mint2 == (Just 0) = "TonatzinGlenn.txt"
    | mint2 == (Just 3) = "forrestpathmap.txt"
    | mint2 == (Just 4) = "WillowInnMap.txt"
    | mint2 == (Just 5) = "Tentsmap.txt"
    | mint2 == (Just 6) = "MurderShopMap.txt"
    | mint2 == (Just 7) = "manormap.txt"
    | mint2 == (Just 8) = "MagatarFacientMap.txt"
    | otherwise = "maptest.txt"
  lined
    | mint == (Just 1) = Nothing
    | mint == (Just 2) = Just NormalVision
    | otherwise = Nothing
  linee
    | mint == (Just 1) = (14, 23)
    | mint == (Just 2) = (0,8)
    | mint == (Just 0) = (0, 0)
    | mint == (Just 3) = (15, 1)
    | mint == (Just 4) = (0,8)
    | mint == (Just 6) = (2,(-1))
    | mint == (Just 7) = (8,0)
    | mint == (Just 8) = (33,41)
    | otherwise = (1,1)
  linef
    | mint == (Just 1) = Just 0
    | mint == (Just 2) = Nothing
    | mint == (Just 5) = Nothing
    | mint == (Just 6) = Nothing
    | mint == (Just 7) = Nothing
    | mint == (Just 8) = Nothing
    | otherwise = Just 0
  lineg
    | mint == (Just 0) = brighttime Nothing (runTime world)
    | mint == (Just 1) = brighttime Nothing (runTime world)
    | mint == (Just 2) = NormalVision
    | mint == (Just 3) = brighttime Nothing (runTime world)
    | mint == (Just 5) = NormalVision
    | mint == (Just 6) = NormalVision
    | mint == (Just 7) = NormalVision
    | otherwise = Dark
  lineh mint2
    | mint2 == (Just 1) = "ForrectGlennChar.txt"
    | mint2 == (Just 2) = "InnmapChar.txt"
    | mint2 == (Just 0) = "TonatzinGlennChar.txt"
    | mint2 == (Just 3) = "forrestpathmapChar.txt"
    | mint2 == (Just 4) = "WillowInnChar.txt"
    | mint2 == (Just 5) = "TentsChar.txt"
    | mint2 == (Just 6) = "MurderShopChar.txt"
    | mint2 == (Just 7) = "manorChar.txt"
    | mint2 == (Just 8) = "MagatarFacientCharacter.txt"
    | otherwise = "maptestChar.txt"
  linei mint2
    | mint2 == (Just 1) = "ForrectGlennExplore.txt"
    | mint2 == (Just 2) = "InnmapExplore.txt"
    | mint2 == (Just 0) = "TonatzinGlennExplore.txt"
    | mint2 == (Just 3) = "forrestpathmapExplore.txt"
    | mint2 == (Just 4) = "WillowInnExplore.txt"
    | mint2 == (Just 5) = "TentsExplore.txt"
    | mint2 == (Just 6) = "MurderShopExplore.txt"
    | mint2 == (Just 7) = "manorExplore.txt"
    | mint2 == (Just 8) = "MagatarFacientExplore.txt"
    | otherwise = "maptestExplore.txt"
  setallyimmediateviison2 :: World -> World
  setallyimmediateviison2 y = over (#runCharacters) (\xs -> fmap (\x -> if temporary (view (#status . #team) x) == Ally then set (#immediateVision) (fst $ charactervisionfield lineg (runMaze y) x xs) x else id x) xs) y

save :: String -> String -> IO ()
save name a = do
  mapfilepath name a
  putStrLn $ name ++ " Saved!"
  return ()

setallyimmediatevsion :: World -> World
setallyimmediatevsion y = over (#runCharacters) (\xs -> fmap (\x -> if temporary (view (#status . #team) x) == Ally then set (#immediateVision) (fst $ charactervisionfield (brighttime (Just Dark) (Time 0 0 0 0)) maze x xs) x else id x) xs) y

setallyimmediatevsion3 :: World -> World
setallyimmediatevsion3 y = over (#runCharacters) (\xs -> fmap (\x -> if temporary (view (#status . #team) x) == Ally then set (#immediateVision) (fst $ charactervisionfield (brighttime (runBrightness y) (runTime y)) (runMaze y) x xs) x else id x) xs) y