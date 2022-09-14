{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module DND.Maps
where

import Control.Lens (view, set, over, Lens')
import Control.Monad.Trans.State.Strict
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Generics.Labels ()
import GHC.Generics (Generic)
import Data.List.Index (indexed, deleteAt)
import System.Random
import Control.Monad (join)
import Data.Char (isDigit)
import Data.Maybe (catMaybes)
import Control.Lens.Indexed (ifoldMap, imap)
import Data.List
import System.IO (Handle, IOMode (WriteMode, ReadMode), hPrint, hClose, openFile, hShow, hGetContents)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Vector
import Codec.BMP
import Graphics.UI.GLUT.Fonts
import GHC.Float (int2Float)
import qualified Data.Map as MAP

import DND.CHARACTER.Character (Location, Character)
import DND.CHARACTER.Status (SpellTemplate, MagicItem, VisionCategory (Dark, Low, NormalVision),magicitemtest2, Vision (Vision), emptystatus)
import DND.ATTACKS.Weapon (Weapon)
import DND.DEFENSE.Armour (Armour)
import DND.SPELLS.Spell (Spell, testspell)
import DND.SPELLS.SpellFunctions (fireball, fireballwand)
import DND.Roll (Roll (Roll, runRoll))
import DND.Action (Action (Action))
import DND.ATTACKS.Attack (Attack (Attack), tohitroll, attackbonus)
import DND.ATTACKS.AttackFunctions (function3, action2)
import DND.DAMAGEDICE.DamageDiceFunctions (dDEP2MI)
import DND.DAMAGEDICE.Elemental (normalelementalresistance)
import DND.STATUSEFFECTS.Effects ((&))

data Portal = Wall {hardness :: Maybe Int, hp :: Maybe Int} | Hall | 
              DoorOpen {hardness :: Maybe Int, hp :: Maybe Int} | 
              DoorClosed {hardness :: Maybe Int, hp :: Maybe Int, runLockDC :: Maybe Int} deriving (Eq, Show, Generic)

isDoorClosed :: Portal -> Bool
isDoorClosed port = case port of
  DoorClosed _ _ _ -> True
  _ -> False

isDoorOpen :: Portal -> Bool
isDoorOpen port = case port of
  DoorOpen _ _ -> True
  _ -> False

isWall :: Portal -> Bool
isWall port = case port of
  Wall _ _ -> True
  _ -> False

getHP :: Portal -> Maybe Int
getHP p = case p of
  Wall x y -> y
  DoorClosed x y z -> y
  _ -> Nothing

getHardness :: Portal -> Maybe Int
getHardness p = case p of
  Wall x y -> x
  DoorClosed x y z -> x
  _ -> Nothing

overHP :: (Maybe Int -> Maybe Int) -> Portal -> Portal
overHP f p = case p of
  Wall x y -> Wall x (f y)
  DoorClosed x y z -> DoorClosed x (f y) z
  x -> x

attackMap' :: Action (Attack  Int) Roll -> Portal -> Portal
attackMap' a p
    | dd == Nothing = p
    | tohittotal > finaltarget = newp
    | otherwise = p
    where
        (Action (Attack info ar dd mdd mstd meff _ _)) = a
        finaltarget = Just 10
        tohit = runRoll $ tohitroll ar
        tohittotal = (+) <$> (attackbonus info) <*> (pure tohit)
        damage1 = (({-(set (#damagebonus) 0) .-} (runRoll <$>)) <$> dd) >>= (\x -> dDEP2MI x (normalelementalresistance))
        damage2 = foldr (&) Nothing ((\x -> dDEP2MI (runRoll <$> x) $ normalelementalresistance) <$> (function3 mdd))
        damagefinal = damage1 & damage2 {- & (fmap (view (#damagebonus)) dd)-}
        reductiontotal = getHardness p
        damagetotal
            | damagefinal < (Just 1) = damagefinal
            | damagefinal <= reductiontotal = (Just 0)
            | otherwise = damagefinal & (fmap negate reductiontotal)
        newp 
          | getHP p == Nothing = p
          | getHP p <= damagetotal = Hall 
          | otherwise = overHP (\x -> x & (fmap negate damagetotal)) p

attackMap :: [Action (Attack  Int) Roll] -> Portal -> Portal
attackMap actions p = case actions of
  [] -> p
  (a:as) -> attackMap as (attackMap' a p)

data Object = HiddenObject {name :: String, runPerceptionDC :: Int, runContents :: [Either (Either (MagicItem Int) SpellTemplate) (Either (Armour Int) Weapon)]}
  | Object {name :: String, runContents :: [Either (Either (MagicItem Int) SpellTemplate) (Either (Armour Int) Weapon)]} 
  | HiddenTrap {name :: String, runPerceptionDC :: Int, runDisableDC :: Int, runTriggerLocations :: [Location], runspelltrap :: Spell [Int]} 
  | Trap {name :: String, runDisableDC :: Int, runTriggerLocations :: [Location], runspelltrap :: Spell [Int]} deriving (Eq, Show, Generic)

changecontents :: ([Either (Either (MagicItem Int) SpellTemplate) (Either (Armour Int) Weapon)] -> [Either (Either (MagicItem Int) SpellTemplate) (Either (Armour Int) Weapon)]) -> Object -> Object
changecontents f obj = case obj of
  HiddenObject a b c -> HiddenObject a b (f c)
  Object a b -> Object a (f b)
  _ -> obj

overcontents :: ([Either (Either (MagicItem Int) SpellTemplate) (Either (Armour Int) Weapon)] -> [Either (Either (MagicItem Int) SpellTemplate) (Either (Armour Int) Weapon)]) -> MazeNode -> MazeNode
overcontents f as = case runObject as of
  Nothing -> as
  (Just y) -> set (#runObject) (Just $ changecontents f y) as

searchHiddenObject :: Int -> Object -> Object
searchHiddenObject i (HiddenTrap name i1 i2 targets trap) = if i >= i2 then (Trap name i1 targets trap) else (HiddenTrap name i1 i2 targets trap)
searchHiddenObject i (HiddenObject name i2 items) = if i >= i2 then (Object name items) else (HiddenObject name i2 items)
searchHiddenObject _  a = a

hasObject :: Maybe MazeNode -> Bool
hasObject x = case x of
  Nothing -> False
  Just y -> case runObject y of
    Nothing -> False
    Just (Object _ _) -> True
    Just (Trap _ _ _ _) -> True
    _ -> False

hasObject2 :: Maybe MazeNode -> Bool
hasObject2 x = case x of
  Nothing -> False
  Just y -> case runObject y of
    Nothing -> False
    _ -> True

hasTrap :: MazeNode -> Bool
hasTrap y = case runObject y of
    Nothing -> False
    Just (HiddenTrap _ _ _ _ _) -> True
    Just (Trap _ _ _ _) -> True
    _ -> False

hasVisTrap :: Maybe MazeNode -> Bool
hasVisTrap mnode = case mnode of
  Nothing -> False
  Just y -> case runObject y of
    Nothing -> False
    Just (Trap _ _ _ _) -> True
    _ -> False

getTrap :: MazeNode -> Maybe (Spell [Int])
getTrap y = case runObject y of
    Nothing -> Nothing
    Just (HiddenTrap _ _ _ _ t) -> Just t
    Just (Trap _ _ _ t) -> Just t
    _ -> Nothing

getTrapLoc :: MazeNode -> [Location]
getTrapLoc y = case runObject y of
    Nothing -> []
    Just (HiddenTrap _ _ _ l _) -> l
    Just (Trap _ _ l _) -> l
    _ -> []

getTrapLoc2 :: MazeNode -> [Location]
getTrapLoc2 y = case runObject y of
    Nothing -> []
    Just (Trap _ _ l _) -> l
    _ -> []

getVisTrapDC :: Maybe MazeNode -> Int
getVisTrapDC mnode = case mnode of
  Nothing -> 0
  Just y -> case runObject y of
    Nothing -> 0
    Just (Trap _ d _ _) -> d
    _ -> 0

getObjectName :: Maybe MazeNode -> String
getObjectName x = case x of
  Nothing -> ""
  Just y -> case runObject y of
    Nothing -> ""
    Just (Object a _) -> a
    Just (Trap a _ _ _) -> a
    _ -> ""

illuminate :: VisionCategory -> Maze -> Maze
illuminate vcat mazeold = MAP.foldrWithKey go mazeold lights
  where
    lights :: MAP.Map Location Vision
    lights = MAP.mapMaybe runLight $ mazeold
    go :: Location -> Vision -> Maze -> Maze
    go loc vis maz = foldr (MAP.alter go3) (foldr (MAP.alter go2) maz fullyilluminated) partiallyilluminated
      where
        fullyilluminated :: [Location]
        fullyilluminated
          | view (#runCategory) vis == NormalVision = finalvisionfield mazeold loc (view (#runDistance) vis)
          | otherwise = []
        partiallyilluminated :: [Location]
        partiallyilluminated
          | view (#runCategory) vis == NormalVision = finalvisionfield mazeold loc (view (#runDistance) vis * 2) \\ finalvisionfield mazeold loc (view (#runDistance) vis)
          | view (#runCategory) vis == Low = finalvisionfield mazeold loc (view (#runDistance) vis)
          | otherwise = []
        go2 :: Maybe MazeNode -> Maybe MazeNode
        go2 mmn = case mmn of
          Nothing -> Just $ set (#runIllumination) NormalVision (emptyblock Dark)
          _ -> fmap (set (#runIllumination) NormalVision) mmn
        go3 :: Maybe MazeNode -> Maybe MazeNode
        go3 mmn = case mmn of
          Nothing -> Just $ over (#runIllumination) (\x -> if (min x vcat) > Low then Low else (min x vcat)) (emptyblock Dark)
          Just y -> Just $ over (#runIllumination) (\x -> if (min x vcat) > Low then Low else (min x vcat)) y 
{-
illuminatechar :: Character -> VisionCategory -> Maze -> Maze
illuminatechar char vcat mazeold = case view (#status . #illumination) char of
  Nothing -> mazeold
  Just y -> go y mazeold
  where
    go :: Vision -> Maze -> Maze
    go charillum maz = foldr (MAP.alter go3) (foldr (MAP.alter go2) maz fullyilluminated) partiallyilluminated
      where
        fullyilluminated :: [Location]
        fullyilluminated
          | view (#runCategory) charillum == NormalVision = finalvisionfield mazeold (view (#runLocation) char) (view (#runDistance) charillum)
          | otherwise = []
        partiallyilluminated :: [Location]
        partiallyilluminated
          | view (#runCategory) charillum == NormalVision = finalvisionfield mazeold (view (#runLocation) char) (view (#runDistance) charillum * 2) \\ finalvisionfield mazeold (view (#runLocation) char) (view (#runDistance) charillum)
          | view (#runCategory) charillum == Low = finalvisionfield mazeold (view (#runLocation) char) (view (#runDistance) charillum)
          | otherwise = []
        go2 :: Maybe MazeNode -> Maybe MazeNode
        go2 mmn = case mmn of
          Nothing -> Just $ set (#runIllumination) NormalVision (emptyblock Dark)
          _ -> fmap (set (#runIllumination) NormalVision) mmn
        go3 :: Maybe MazeNode -> Maybe MazeNode
        go3 mmn = case mmn of
          Nothing -> Just $ over (#runIllumination) (\x -> if (min x vcat) > Low then Low else (min x vcat)) (emptyblock Dark)
          Just y -> Just $ over (#runIllumination) (\x -> if (min x vcat) > Low then Low else (min x vcat)) y 
-}
illuminatechar :: Maze -> Character -> Character
illuminatechar mazeold char = case view (#status . #illumination) char of
  Nothing -> char
  Just y -> set (#illuminatedSquares) (fullyilluminated y) char
  where
        fullyilluminated :: Vision -> [Location]
        fullyilluminated charillum
          | view (#runCategory) charillum == NormalVision = finalvisionfield mazeold (view (#runLocation) char) (view (#runDistance) charillum)
          | otherwise = []
        partiallyilluminated :: Vision -> [Location]
        partiallyilluminated charillum
          | view (#runCategory) charillum == NormalVision = finalvisionfield mazeold (view (#runLocation) char) (view (#runDistance) charillum * 2) \\ finalvisionfield mazeold (view (#runLocation) char) (view (#runDistance) charillum)
          | view (#runCategory) charillum == Low = finalvisionfield mazeold (view (#runLocation) char) (view (#runDistance) charillum)
          | otherwise = []

illuminatecharmap :: [Character] -> VisionCategory -> Maze -> (Maze, [Location])
illuminatecharmap chars vcat mazeold = go mazeold
  where
    go :: Maze -> (Maze, [Location])
    go maz = ((foldr (MAP.alter go2) maz fullyilluminated),fullyilluminated)
      where
        fullyilluminated :: [Location]
        fullyilluminated = nub . join $ fmap (view (#illuminatedSquares)) chars
        go2 :: Maybe MazeNode -> Maybe MazeNode
        go2 mmn = case mmn of
          Nothing -> Just $ set (#runIllumination) NormalVision (emptyblock Dark)
          _ -> fmap (set (#runIllumination) NormalVision) mmn
        go3 :: Maybe MazeNode -> Maybe MazeNode
        go3 mmn = case mmn of
          Nothing -> Just $ over (#runIllumination) (\x -> if (min x vcat) > Low then Low else (min x vcat)) (emptyblock Dark)
          Just y -> Just $ over (#runIllumination) (\x -> if (min x vcat) > Low then Low else (min x vcat)) y

data MazeBlock a = MazeBlock {runNorth :: a, runEast :: a, runSouth :: a, runWest :: a, runObject :: Maybe Object, runIllumination :: VisionCategory, runLight :: Maybe Vision
                             , runspell :: Maybe (Spell [Int], Int)} deriving (Eq, Show, Generic)

instance Functor MazeBlock where
  fmap f (MazeBlock a b c d e g h i) = MazeBlock (f a) (f b) (f c) (f d) e g h i

instance Applicative MazeBlock where
  pure a = MazeBlock a a a a Nothing Dark Nothing Nothing
  (<*>) (MazeBlock fa fb fc fd fe fg fh fi) (MazeBlock a b c d _ _ _ _) = MazeBlock (fa a) (fb b) (fc c) (fd d) fe fg fh fi

instance Foldable MazeBlock where
  foldMap f (MazeBlock a b c d _ _ _ _) = (f a) <> (f b) <> (f c) <> (f d) 

type MazeNode = MazeBlock Portal

data CellCoordinates = CellCoordinates
  { cellCenter :: Point
  , cellTopLeft :: Point
  , cellTopRight :: Point
  , cellBottomLeft :: Point
  , cellBottomRight :: Point
  }

isEmptyBlock :: Maybe MazeNode -> Bool
isEmptyBlock mmn = case mmn of
  Nothing -> False
  Just mn -> case mn of
    MazeBlock Hall Hall Hall Hall _ _ _ _ -> True
    _ -> False

emptyblock :: VisionCategory -> MazeNode
emptyblock vis = MazeBlock Hall Hall Hall Hall Nothing (min vis Dark) Nothing Nothing

type Maze = MAP.Map Location MazeNode

removeTrap :: MazeNode -> MazeNode
removeTrap maze1 = if hasTrap maze1 then set (#runObject) Nothing maze1 else maze1

maze :: Maze
maze = MAP.fromList $ fmap (\((x,y),z) -> (((x - 4), (y - 1)), z)) $ [((5, 5), MazeBlock Hall Hall Hall Hall (Just (HiddenTrap "Trap" 15 15 [(0, 5),(2, 5),(1, 5),(0, 4),(2, 4),(1, 4),(0, 3),(2, 3),(1, 3)] (fireballwand Nothing Nothing emptystatus))) Dark (Just $ Vision NormalVision 20) Nothing)] ++ room2 (4,1) (6,10) ++ room (4,12) (6,2) ++ room (8,15) (3,5) ++ walll (11,3) 9 (MazeBlock Hall Hall (Wall Nothing Nothing) Hall Nothing Dark Nothing Nothing) ++ room (21, 1) (6,10) 
  ++ room (21, 12) (6,2) ++ [((19, 19), MazeBlock Hall (Wall Nothing Nothing) (Wall Nothing Nothing) (Wall Nothing Nothing) Nothing Dark Nothing Nothing),((19, 20), MazeBlock (Wall Nothing Nothing) (Wall Nothing Nothing) Hall (DoorClosed Nothing Nothing (Just 15)) Nothing Dark Nothing Nothing), ((20, 19), MazeBlock (Wall Nothing Nothing) (Wall Nothing Nothing) (DoorClosed Nothing Nothing Nothing) (Wall Nothing Nothing) Nothing Dark Nothing Nothing)] ++ room (24, 19) (4,3)
  ++ room (27, 23) (2,1) ++ walll (12,23) 5 (MazeBlock (Wall Nothing Nothing) Hall Hall Hall Nothing Dark Nothing Nothing) ++ [((18, 23), MazeBlock (Wall Nothing Nothing) (Wall Nothing Nothing) Hall Hall Nothing Dark Nothing Nothing),((11, 23), MazeBlock (Wall Nothing Nothing) Hall Hall (Wall Nothing Nothing) Nothing Dark Nothing Nothing)]
  ++ wallh (11,21) 1 (MazeBlock Hall Hall Hall (Wall Nothing Nothing) Nothing Dark Nothing Nothing) ++ wallh (18,21) 1 (MazeBlock Hall (Wall Nothing Nothing) Hall Hall Nothing Dark Nothing Nothing) ++ walll (21,19) 2 (MazeBlock Hall Hall (Wall Nothing Nothing) Hall Nothing Dark Nothing Nothing) ++ walll (22,17) 10 (MazeBlock Hall Hall (Wall Nothing Nothing) Hall Nothing Dark Nothing Nothing)
  ++ wallh (22,15) 1 (MazeBlock Hall Hall Hall (Wall Nothing Nothing) Nothing Dark Nothing Nothing) ++ wallh (30,14) 2 (MazeBlock Hall (Wall Nothing Nothing) Hall Hall Nothing Dark Nothing Nothing) 
  ++ [((33,17), MazeBlock Hall (DoorClosed Nothing Nothing Nothing) (Wall Nothing Nothing) Hall Nothing Dark Nothing Nothing),((33,18), MazeBlock Hall (Wall Nothing Nothing) Hall Hall Nothing Dark Nothing Nothing),((34,19), MazeBlock Hall Hall (Wall Nothing Nothing) Hall Nothing Dark Nothing Nothing),((35,19), MazeBlock Hall Hall (Wall Nothing Nothing) Hall Nothing Dark Nothing Nothing),((36,19), MazeBlock Hall Hall (Wall Nothing Nothing) (Wall Nothing Nothing) Nothing Dark Nothing Nothing)]
  ++ walll (30,13) 6 (MazeBlock (Wall Nothing Nothing) Hall Hall Hall Nothing Dark Nothing Nothing) ++ wallh (37,14) 4 (MazeBlock Hall Hall Hall (Wall Nothing Nothing) Nothing Dark Nothing Nothing) ++ wallh (36,20) 2 (MazeBlock Hall Hall Hall (Wall Nothing Nothing) Nothing Dark Nothing Nothing) ++ walll (30,23) 5 (MazeBlock Hall Hall (Wall Nothing Nothing) Hall Nothing Dark Nothing Nothing)
  ++ [((10,7), (MazeBlock Hall (DoorClosed Nothing Nothing Nothing) Hall Hall Nothing Dark Nothing Nothing)), ((10,8), (MazeBlock Hall (DoorClosed Nothing Nothing Nothing) Hall Hall Nothing Dark Nothing Nothing)), ((21,7), (MazeBlock Hall Hall Hall (DoorClosed Nothing Nothing Nothing) Nothing Dark Nothing Nothing)), ((21,8), (MazeBlock Hall Hall Hall (DoorClosed Nothing Nothing Nothing) Nothing Dark Nothing Nothing)), ((6,11), (MazeBlock (DoorClosed Nothing Nothing Nothing) Hall Hall Hall Nothing Dark Nothing Nothing)), ((6,12), (MazeBlock Hall Hall Hall Hall Nothing Dark Nothing Nothing)), ((10,13), (MazeBlock Hall (DoorClosed Nothing Nothing Nothing) Hall Hall Nothing Dark Nothing Nothing)),
  ((11,17), (MazeBlock Hall (DoorClosed Nothing Nothing Nothing) Hall Hall Nothing Dark Nothing Nothing)), ((21,17), (MazeBlock Hall (DoorClosed Nothing Nothing Nothing) Hall Hall Nothing Dark Nothing Nothing)), ((21,18), (MazeBlock Hall (DoorClosed Nothing Nothing Nothing) Hall Hall Nothing Dark Nothing Nothing)), ((21,13), (MazeBlock Hall Hall Hall (DoorClosed Nothing Nothing Nothing) Nothing Dark Nothing Nothing)), ((23,12), (MazeBlock Hall Hall Hall Hall Nothing Dark Nothing Nothing)), ((23,11), (MazeBlock (DoorClosed Nothing Nothing Nothing) Hall Hall Hall Nothing Dark Nothing Nothing)), ((22, 19), MazeBlock Hall Hall (DoorClosed Nothing Nothing Nothing) Hall Nothing Dark Nothing Nothing), 
  ((26, 19), MazeBlock Hall Hall (DoorClosed Nothing Nothing Nothing) Hall Nothing Dark Nothing Nothing), ((29, 23), MazeBlock Hall (Wall Nothing Nothing) (DoorClosed Nothing Nothing Nothing) Hall Nothing Dark Nothing Nothing)]

mazea :: Maze
mazea = MAP.fromList $ room (0,0) (30,24)

room :: Location -> Location -> [(Location, MazeNode)]
room (x, y) (l, h) = wall1 ++ wall2 ++ wall3 ++ wall4 ++ 
  [((x,y), MazeBlock Hall Hall (Wall Nothing Nothing) (Wall Nothing Nothing) Nothing Dark Nothing Nothing), ((x,y + h), MazeBlock (Wall Nothing Nothing) Hall Hall (Wall Nothing Nothing) Nothing Dark Nothing Nothing), ((x + l,y + h), MazeBlock (Wall Nothing Nothing) (Wall Nothing Nothing) Hall Hall Nothing Dark Nothing Nothing), ((x + l,y), MazeBlock Hall (Wall Nothing Nothing) (Wall Nothing Nothing) Hall Nothing Dark Nothing Nothing)]
  where
    wall1 = fmap (\x' -> (((x', (y + h )) :: Location), MazeBlock (Wall Nothing Nothing) Hall Hall Hall Nothing Dark Nothing Nothing)) [(x + 1) .. (x + l - 1)]
    wall2 = fmap (\y' -> ((((x + l ), y') :: Location), MazeBlock Hall (Wall Nothing Nothing) Hall Hall Nothing Dark Nothing Nothing)) [(y + 1) .. (y + h - 1)]
    wall3 = fmap (\x' -> (((x', y ) :: Location), MazeBlock Hall Hall (Wall Nothing Nothing) Hall Nothing Dark Nothing Nothing)) [(x + 1) .. (x + l - 1)]
    wall4 = fmap (\y' -> (((x, y') :: Location), MazeBlock Hall Hall Hall (Wall Nothing Nothing) Nothing Dark Nothing Nothing)) [(y + 1) .. (y + h - 1)]
room2 :: Location -> Location -> [(Location, MazeNode)]
room2 (x, y) (l, h) = wall1 ++ wall2 ++ wall3 ++ wall4 ++ 
  [((x,y), MazeBlock Hall Hall (Wall (Just 0) (Just 10)) (Wall (Just 0) (Just 10)) Nothing Dark Nothing Nothing), ((x,y + h), MazeBlock (Wall (Just 0) (Just 10)) Hall Hall (Wall (Just 0) (Just 10)) Nothing Dark Nothing Nothing), ((x + l,y + h), MazeBlock (Wall (Just 0) (Just 10)) (Wall (Just 0) (Just 10)) Hall Hall Nothing Dark Nothing Nothing), ((x + l,y), MazeBlock Hall (Wall (Just 0) (Just 10)) (Wall (Just 0) (Just 10)) Hall Nothing Dark Nothing Nothing)]
  where
    wall1 = fmap (\x' -> (((x', (y + h )) :: Location), MazeBlock (Wall (Just 0) (Just 10)) Hall Hall Hall Nothing Dark Nothing Nothing)) [(x + 1) .. (x + l - 1)]
    wall2 = fmap (\y' -> ((((x + l ), y') :: Location), MazeBlock Hall (Wall (Just 0) (Just 10)) Hall Hall Nothing Dark Nothing Nothing)) [(y + 1) .. (y + h - 1)]
    wall3 = fmap (\x' -> (((x', y ) :: Location), MazeBlock Hall Hall (Wall (Just 0) (Just 10)) Hall Nothing Dark Nothing Nothing)) [(x + 1) .. (x + l - 1)]
    wall4 = fmap (\y' -> (((x, y') :: Location), MazeBlock Hall Hall Hall (Wall (Just 0) (Just 10)) Nothing Dark Nothing Nothing)) [(y + 1) .. (y + h - 1)]

walll :: Location -> Int -> MazeNode -> [(Location, MazeNode)]
walll (x, y) l mazenode = fmap (\x' -> (((x', y ) :: Location), mazenode)) [x .. (x + l)]

wallh :: Location -> Int -> MazeNode -> [(Location, MazeNode)]
wallh (x, y) h mazenode = fmap (\y' -> (((x, y') :: Location), mazenode)) [y .. (y + h)]

maze2 :: Maze
maze2 = MAP.fromList $ wall1 ++ wall2 ++ wall3 ++ wall4
  where
    wall1 = fmap (\x -> (((x, 12) :: Location), MazeBlock (Wall Nothing Nothing) Hall Hall Hall Nothing Dark Nothing Nothing)) [8 .. 12]
    wall2 = fmap (\x -> (((12, x) :: Location), MazeBlock Hall (Wall Nothing Nothing) Hall Hall Nothing Dark Nothing Nothing)) [8 .. 12]
    wall3 = fmap (\x -> (((x, 8) :: Location), MazeBlock Hall Hall (Wall Nothing Nothing) Hall Nothing Dark Nothing Nothing)) [8 .. 12]
    wall4 = fmap (\x -> (((8, x) :: Location), MazeBlock Hall Hall Hall (Wall Nothing Nothing) Nothing Dark Nothing Nothing)) [8 .. 12]

meleedistance :: Location -> Location -> Int
meleedistance (a1, b1) (a2, b2) = distance
  where
    deltaa = abs $ a1 - a2
    deltab = abs $ b1 - b2
    distance = max deltaa deltab
    
eucliddistance :: Location -> Location -> Int
eucliddistance (a1, b1) (a2, b2) = round . sqrt . fromIntegral $ hypotenuse 
  where
    deltaa = abs $ a1 - a2
    deltab = abs $ b1 - b2
    hypotenuse = deltaa * deltaa + deltab * deltab  
eucliddistancefloat :: Location -> Location -> Float
eucliddistancefloat (a1, b1) (a2, b2) = sqrt . fromIntegral $ hypotenuse 
  where
    deltaa = abs $ a1 - a2
    deltab = abs $ b1 - b2
    hypotenuse = deltaa * deltaa + deltab * deltab

conealgo :: Location -> Location -> [Location]
conealgo pa@(xa,ya) pb@(xb,yb) = pb : (fmap (\(x,y) -> (x + xa, y + ya)) $ nub (foldl (\x y -> x ++ (linealgo (0,0) y)) [] destinations))
  where
    dest = cellCenter $ vanillaLocationtoCoords (xb - xa, yb - ya)
    (x1,y1) = rotateV (pi / 4) dest
    (x2,y2) = rotateV (pi / (-4)) dest
    cellone = (round $ x1 / 70, round $ y1 / 70)
    celltwo = (round $ x2 / 70, round $ y2 / 70)
    destinations = linealgo cellone celltwo

linealgo :: Location -> Location -> [Location]
linealgo pa@(xa,ya) pb@(xb,yb) = map maySwitch . unfoldr go $ (x1,y1,0)
  where
    steep = abs (yb - ya) > abs (xb - xa)
    maySwitch = if steep then (\(x,y) -> (y,x)) else id
    [(x1,y1),(x2,y2)] = sort [maySwitch pa, maySwitch pb]
    deltax = x2 - x1
    deltay = abs (y2 - y1)
    ystep = if y1 < y2 then 1 else -1
    go (xTemp, yTemp, error)
        | xTemp > x2 = Nothing
        | otherwise  = Just ((xTemp, yTemp), (xTemp + 1, newY, newError))
        where
          tempError = error + deltay
          (newY, newError) = if (2*tempError) >= deltax
                            then (yTemp+ystep,tempError-deltax)
                            else (yTemp,tempError)

linealgomap :: Location -> Location -> Maybe Int -> Maybe Int -> [(Location, MazeNode)]
linealgomap pa@(xa',ya') pb@(xb',yb') hardness hp
  | xb' == xa' && ya' < yb' = fmap (\y -> ((xb', y), MazeBlock Hall Hall Hall (Wall hardness hp) Nothing Dark Nothing Nothing)) [ya' .. yb']
  | yb' == ya' && xa' < xb' = fmap (\x -> ((x, yb'), MazeBlock (Wall hardness hp) Hall Hall Hall Nothing Dark Nothing Nothing)) [xa' .. xb']
  | otherwise = map maySwitch2 . unfoldr go $ (x1,y1,0,False)
  where
    direction = cellCenter $ vanillaLocationtoCoords (xb' - xa', yb' - ya')
    [(xa,ya),(xb,yb)] = maySwitch3
    steep = abs (yb - ya) > abs (xb - xa)
    maySwitch = if steep then (\(x,y) -> (y,x)) else id
    maySwitch2 = if steep then (\((x,y),z) -> ((y,x),{-maySwitch3-} z)) else id
    maySwitch3 = if (argV direction > pi / 4) && (argV direction < (5 * pi) / 4) then [pb, pa] else [pa, pb]
    {-maySwitch3 (MazeBlock a b c d e f g h) = (MazeBlock c d a b e f g h)-}
    [(x1,y1),(x2,y2)] = sort [maySwitch pa, maySwitch pb]
    deltax = x2 - x1
    deltay = abs (y2 - y1)
    blocka
      {-| steep && xb /= xa = MazeBlock Hall Hall Hall (Wall hardness hp) Nothing Dark Nothing Nothing -}
      | steep = MazeBlock Hall (if (xb - xa) < 0 then Hall else (Wall hardness hp)) Hall (if (xb - xa) >= 0 then Hall else (Wall hardness hp)) Nothing Dark Nothing Nothing 
      {-| yb /= ya = MazeBlock (Wall hardness hp) Hall Hall Hall Nothing Dark Nothing Nothing-}
      | otherwise =  MazeBlock (if (yb - ya) < 0 then (Wall hardness hp) else Hall) Hall (if (yb - ya) >= 0 then (Wall hardness hp) else Hall) Hall Nothing Dark Nothing Nothing
    blockb = MazeBlock (if (yb - ya) < 0 then (Wall hardness hp) else Hall) (if (xb - xa) < 0 then Hall else (Wall hardness hp)) (if (yb - ya) >= 0 then (Wall hardness hp) else Hall) (if (xb - xa) >= 0 then Hall else (Wall hardness hp)) Nothing Dark Nothing Nothing
    ystep = if y1 < y2 then 1 else -1
    go (xTemp, yTemp, error, step)
        | xTemp > x2 = Nothing
        | otherwise  = Just (((xTemp, yTemp), if newstep then blockb else blocka), (xTemp + 1, newY, newError, newstep))
        where
          tempError = error + deltay
          (newY, newError, newstep) = if (2*tempError) > deltax
                            then (yTemp+ystep,tempError-deltax,True)
                            else (yTemp,tempError,False)
{-  
    blocka = if steep then MazeBlock Hall (if deltax * deltay >= 0 then (Wall Nothing Nothing) else Hall) Hall (if deltax * deltay < 0 then (Wall Nothing Nothing) else Hall) Nothing Dark Nothing Nothing else MazeBlock (if deltax * deltay >= 0 then (Wall Nothing Nothing) else Hall) Hall (if deltax * deltay < 0 then (Wall Nothing Nothing) else Hall) Hall Nothing Dark Nothing Nothing
    blockb = MazeBlock (if deltax * deltay >= 0 then (Wall Nothing Nothing) else Hall) (if deltax * deltay >= 0 then (Wall Nothing Nothing) else Hall) (if deltax * deltay < 0 then (Wall Nothing Nothing) else Hall) (if deltax * deltay < 0 then (Wall Nothing Nothing) else Hall) Nothing Dark Nothing Nothing
    -}
roommaker :: [Location] -> Maybe Int -> Maybe Int -> [(Location, MazeNode)]
roommaker [] _ _ = []
roommaker (a:[]) _ _ = []
roommaker (a1:a2:as) hardness hp = linealgomap a1 a2 hardness hp ++ roommaker (a2:as) hardness hp

finddirections :: [Location] -> [Float]
finddirections locs = case locs of
  [] -> []
  (a: []) -> []
  (a1: a2: as) -> argV direction : finddirections (a2 : as)
    where
      (xa,ya) = a1
      (xb,yb) = a2
      direction = cellCenter $ vanillaLocationtoCoords (xb - xa, yb - ya)

addtomaze :: [Location] -> Maybe Int -> Maybe Int -> Maze -> Maze
addtomaze locs hardness hp maze1 = MAP.unionWith go (MAP.fromListWith go $ roommaker locs hardness hp) maze1
  where
    go =  (\x y -> take <$> x <*> y)
    take :: Portal -> Portal -> Portal
    take (DoorClosed x y z) _ = (DoorClosed x y z)
    take _ (DoorClosed x y z) = (DoorClosed x y z)
    take (DoorOpen x y) _ = (DoorOpen x y)
    take _ (DoorOpen x y) = (DoorOpen x y)
    take (Wall x y) _ = (Wall x y)
    take _ (Wall x y) = (Wall x y)
    take _ _ = Hall

maptestlines :: Maze
maptestlines = MAP.fromListWith (\x y -> take <$> x <*> y) $ roommaker [(-1,24),(-1,-1),(30,-1),(30,24),(-1,24)] Nothing Nothing ++ roommaker [(2,3),(10,2)] Nothing Nothing ++ [((4,20),MazeBlock Hall Hall Hall Hall Nothing NormalVision (Just $ Vision NormalVision 20) Nothing)]
  where
    take :: Portal -> Portal -> Portal
    take (DoorClosed x y z) _ = (DoorClosed x y z)
    take _ (DoorClosed x y z) = (DoorClosed x y z)
    take (DoorOpen x y) _ = (DoorOpen x y)
    take _ (DoorOpen x y) = (DoorOpen x y)
    take (Wall x y) _ = (Wall x y)
    take _ (Wall x y) = (Wall x y)
    take _ _ = Hall

maplines :: [Location] -> Maze
maplines locs = MAP.fromListWith (\x y -> take <$> x <*> y) $ roommaker locs Nothing Nothing
  where
    take :: Portal -> Portal -> Portal
    take (DoorClosed x y z) _ = (DoorClosed x y z)
    take _ (DoorClosed x y z) = (DoorClosed x y z)
    take (DoorOpen x y) _ = (DoorOpen x y)
    take _ (DoorOpen x y) = (DoorOpen x y)
    take (Wall x y) _ = (Wall x y)
    take _ (Wall x y) = (Wall x y)
    take _ _ = Hall
{-$ linealgomap (5,5) (1,7) ++ linealgomap (0,8) (2, 12) ++ linealgomap (3,13) (7,11) ++ linealgomap (8,10) (6,6)-}

visionfield' :: Location -> Int -> [Location]
visionfield' (a,b) i = do
  avariant <- [(+ a)] <*> [(i `div` (-5)) .. (i `div` 5)]
  bvariant <- [(+ b)] <*> [(i `div` (-5)) .. (i `div` 5)]
  return (avariant, bvariant)

visionfield'' :: Location -> Int -> [Location]
visionfield'' (a,b) i = filter (\x -> eucliddistance (a,b) x <= (i `div` 5)) (visionfield' (a,b) i)

visionfieldNorth :: Location -> [Location]
visionfieldNorth (a,b) = do
  avariant <- [(+ a)] <*> [(-12) .. 12]
  bvariant <- [(+ b)] <*> [ (0) .. 12]
  return (avariant, bvariant)

visionfieldNorth2 :: Location -> [Location]
visionfieldNorth2 (a,b) = do
  avariant <- [(+ a)] <*> [(-12) .. 12]
  bvariant <- [(+ b)] <*> [ (1) .. 12]
  return (avariant, bvariant)

visionfieldSouth :: Location -> [Location]
visionfieldSouth (a,b) = do
  avariant <- [(+ a)] <*> [(-12) .. 12]
  bvariant <- [(+ b)] <*> [ (-12) .. (-1)]
  return (avariant, bvariant)

visionfieldSouth2 :: Location -> [Location]
visionfieldSouth2 (a,b) = do
  avariant <- [(+ a)] <*> [(-12) .. 12]
  bvariant <- [(+ b)] <*> [ (-12) .. 0]
  return (avariant, bvariant)

visionfieldEast :: Location -> [Location]
visionfieldEast (a,b) = do
  avariant <- [(+ a)] <*> [0 .. 12]
  bvariant <- [(+ b)] <*> [ (-12) .. 12]
  return (avariant, bvariant)

visionfieldWest :: Location -> [Location]
visionfieldWest (a,b) = do
  avariant <- [(+ a)] <*> [(-12) .. (-1)]
  bvariant <- [(+ b)] <*> [ (-12) .. 12]
  return (avariant, bvariant)

visionfieldEast2 :: Location -> [Location]
visionfieldEast2 (a,b) = do
  avariant <- [(+ a)] <*> [1 .. 12]
  bvariant <- [(+ b)] <*> [ (-12) .. 12]
  return (avariant, bvariant)

visionfieldWest2 :: Location -> [Location]
visionfieldWest2 (a,b) = do
  avariant <- [(+ a)] <*> [(-12) .. 0]
  bvariant <- [(+ b)] <*> [ (-12) .. 12]
  return (avariant, bvariant)

visionfield :: Location -> Int -> [Location]
visionfield loc i = sortOn (meleedistance loc) $ visionfield'' loc i

finalvisionfield :: Maze -> Location -> Int -> [Location]
finalvisionfield maze1 loc i = (visionfield loc i) \\ (concat $ fmap (blockvisionvector maze1 loc i ) (visionfield loc i))

finalvisionfield' :: Maze -> Location -> Int -> [Location]
finalvisionfield' maze1 loc i = (visionfield loc i) \\ ((concat $ fmap (blocksvision maze1 loc i ) (visionfieldNorth loc)) ++ (concat $ fmap (blocksvision2 maze1 loc i) (visionfieldSouth loc)) ++
  (concat $ fmap (blocksvision3 maze1 loc i) (visionfieldNorth2 loc)) ++ (concat $ fmap (blocksvision4 maze1 loc i) (visionfieldSouth2 loc)) ++ 
  (concat $ fmap (blocksvision5 maze1 loc i) (visionfieldEast loc)) ++ (concat $ fmap (blocksvision6 maze1 loc i) (visionfieldWest loc)) ++ 
  (concat $ fmap (blocksvision7 maze1 loc i) (visionfieldEast2 loc)) ++ (concat $ fmap (blocksvision8 maze1 loc i) (visionfieldWest2 loc)))

searchmaze :: Int -> [Location] -> Maze -> Maze
searchmaze i locs maze = foldl (\x y -> MAP.adjust (over (#runObject) (searchHiddenObject i <$>)) y x) maze locs

blockvisionvector :: Maze -> Location -> Int -> Location -> [Location]
blockvisionvector maze char@(xa,ya) i obstacle@(xb,yb)
  | MAP.lookup obstacle maze == Nothing = []
  | isEmptyBlock $ MAP.lookup obstacle maze = []
  | otherwise = blockedsquare
    where
      charcenter = cellCenter $ vanillaLocationtoCoords (xb - xa, yb - ya)
      mazeblock = case MAP.lookup obstacle maze of
        Nothing -> (emptyblock Dark)
        Just y -> y
      go2 :: Point -> Location
      go2 (x, y) = ((round $ x / 70) + xa, (round $ y / 70) + ya)
      potentialcentres = fmap (cellCenter . vanillaLocationtoCoords . (\(x,y) -> (x - xa, y - ya))) $ visionfield char i
      walls = wallCoordinates (xb - xa, yb - ya) mazeblock
      pointcorrection trailingpoint1 leadingpoint1 = if abs (argV leadingpoint1 - argV trailingpoint1) > pi
          then if argV trailingpoint1 > argV leadingpoint1 then (argV leadingpoint1, (argV trailingpoint1 - (2 * pi))) else ((argV leadingpoint1 - (2 * pi)), argV trailingpoint1)
          else (argV leadingpoint1, argV trailingpoint1)
      checkpointcorrection checkpoint trailingpoint leadingpoint = 
        if ((snd $ pointcorrection trailingpoint leadingpoint) >= argV checkpoint && argV checkpoint >= (fst $ pointcorrection trailingpoint leadingpoint)) ||
          ((fst $ pointcorrection trailingpoint leadingpoint) >= argV checkpoint && argV checkpoint >= (snd $ pointcorrection trailingpoint leadingpoint))
        then argV checkpoint else argV checkpoint - (2 * pi)
      distancecheck checkpoint leadingpoint trailingpoint = magV checkpoint >= scalarprojection
        where
          theta =  abs $ ((fst $ pointcorrection trailingpoint leadingpoint) - (snd $ pointcorrection trailingpoint leadingpoint)) / 2
          scalarprojection = if (fst $ pointcorrection trailingpoint leadingpoint) > (snd $ pointcorrection trailingpoint leadingpoint)
            then abs $ ((magV leadingpoint * cos theta) + (magV trailingpoint * cos (negate theta))) / 2
            else abs $ ((magV trailingpoint * cos theta) + (magV leadingpoint * cos (negate theta))) / 2
      angle checkpoint leadingpoint trailingpoint = if (fst $ pointcorrection trailingpoint leadingpoint) > (snd $ pointcorrection trailingpoint leadingpoint)
          then ((fst $ pointcorrection trailingpoint leadingpoint) >= (checkpointcorrection checkpoint trailingpoint leadingpoint) && 
                (checkpointcorrection checkpoint trailingpoint leadingpoint) >= (snd $ pointcorrection trailingpoint leadingpoint))
          else ((snd $ pointcorrection trailingpoint leadingpoint) >= (checkpointcorrection checkpoint trailingpoint leadingpoint) && 
                (checkpointcorrection checkpoint trailingpoint leadingpoint) >= (fst $ pointcorrection trailingpoint leadingpoint))
      go :: [(Point, Point)] -> Point -> Bool
      go points p@(x, y) = case points of
        [] -> False
        ((p1,p2): as) -> (angle p p1 p2 && distancecheck p p1 p2) || go as p
      blockedsquare = fmap go2 $ filter (go walls) potentialcentres

blocksvision :: Maze -> Location -> Int -> Location -> [Location]
blocksvision maze char i obstacle
  | MAP.lookup obstacle maze == Nothing = []
  | isEmptyBlock $ MAP.lookup obstacle maze = []
  | otherwise = blockedsquare
    where
      charobstaclecompare (x1,y1) (x2,y2) = x2 - x1
      charcenter = cellCenter $ vanillaLocationtoCoords char
      mazeblock = case MAP.lookup obstacle maze of
        Nothing -> (emptyblock Dark)
        Just y -> y
      northwall = wallcoordinatesNorth obstacle mazeblock
      lines = case northwall of
        Nothing -> Nothing 
        Just (a, b) -> Just (linefunction charcenter a, linefunction charcenter b, linefunction a b)
      potentialcentres = fmap (cellCenter . vanillaLocationtoCoords) $ visionfield char i
      blockedsquare = fmap go2 $ filter go potentialcentres
      go :: Point -> Bool
      go (x, y) = case lines of
        Nothing -> False
        Just (a, b , c) ->  (if charobstaclecompare char obstacle <= 0 then ((a x) <= y) else ((a x) >= y)) && 
                            (if charobstaclecompare char obstacle >= 0 then ((b x) <= y) else ((b x) >= y)) && (c x < y)
      go2 :: Point -> Location
      go2 (x, y) = (round $ x / 70, round $ y / 70)

blocksvision2 :: Maze -> Location -> Int -> Location -> [Location]
blocksvision2 maze char i obstacle
  | MAP.lookup obstacle maze == Nothing = []
  | isEmptyBlock $ MAP.lookup obstacle maze = []
  | otherwise = blockedsquare
    where
      charobstaclecompare (x1,y1) (x2,y2) = x2 - x1
      charcenter = cellCenter $ vanillaLocationtoCoords char
      mazeblock = case MAP.lookup obstacle maze of
        Nothing -> (emptyblock Dark)
        Just y -> y
      northwall = wallcoordinatesNorth obstacle mazeblock
      lines = case northwall of
        Nothing -> Nothing 
        Just (a, b) -> Just (linefunction charcenter a, linefunction charcenter b, linefunction a b)
      potentialcentres = fmap (cellCenter . vanillaLocationtoCoords) $ visionfield char i
      blockedsquare = fmap go2 $ filter go potentialcentres
      go :: Point -> Bool
      go (x, y) = case lines of
        Nothing -> False
        Just (a, b, c) -> comp1 a && comp2 b && (c x > y)
        where
          comp1 f = if charobstaclecompare char obstacle <= 0 then (f x >= y) else (f x <= y)
          comp2 f = if charobstaclecompare char obstacle >= 0 then (f x >= y) else (f x <= y)
      go2 :: Point -> Location
      go2 (x, y) = (round $ x / 70, round $ y / 70)

blocksvision3 :: Maze -> Location -> Int -> Location -> [Location]
blocksvision3 maze char i obstacle
  | MAP.lookup obstacle maze == Nothing = []
  | isEmptyBlock $ MAP.lookup obstacle maze = []
  | otherwise = blockedsquare
    where
      charobstaclecompare (x1,y1) (x2,y2) = x1 - x2
      charcenter = cellCenter $ vanillaLocationtoCoords char
      mazeblock = case MAP.lookup obstacle maze of
        Nothing -> (emptyblock Dark)
        Just y -> y
      northwall = wallcoordinatesSouth obstacle mazeblock
      lines = case northwall of
        Nothing -> Nothing 
        Just (a, b) -> Just (linefunction charcenter a, linefunction charcenter b, linefunction a b)
      potentialcentres = fmap (cellCenter . vanillaLocationtoCoords) $ visionfield char i
      blockedsquare = fmap go2 $ filter go potentialcentres
      go :: Point -> Bool
      go (x, y) = case lines of
        Nothing -> False
        Just (a, b, c) -> comp1 a && comp2 b && (c x < y)
        where
          comp1 f = if charobstaclecompare char obstacle >= 0 then (f x <= y) else (f x >= y)
          comp2 f = if charobstaclecompare char obstacle <= 0 then (f x <= y) else (f x >= y)
      go2 :: Point -> Location
      go2 (x, y) = (round $ x / 70, round $ y / 70)

blocksvision4 :: Maze -> Location -> Int -> Location -> [Location]
blocksvision4 maze char i obstacle
  | MAP.lookup obstacle maze == Nothing = []
  | isEmptyBlock $ MAP.lookup obstacle maze = []
  | otherwise = blockedsquare
    where
      charobstaclecompare (x1,y1) (x2,y2) = x1 -x2
      charcenter = cellCenter $ vanillaLocationtoCoords char
      mazeblock = case MAP.lookup obstacle maze of
        Nothing -> (emptyblock Dark)
        Just y -> y
      northwall = wallcoordinatesSouth obstacle mazeblock
      lines = case northwall of
        Nothing -> Nothing 
        Just (a, b) -> Just (linefunction charcenter a, linefunction charcenter b, linefunction a b)
      potentialcentres = fmap (cellCenter . vanillaLocationtoCoords) $ visionfield char i
      blockedsquare = fmap go2 $ filter go potentialcentres
      go :: Point -> Bool
      go (x, y) = case lines of
        Nothing -> False
        Just (a, b, c) -> comp1 a && comp2 b && (c x > y)
        where
          comp1 f = if charobstaclecompare char obstacle >= 0 then (f x >= y) else (f x <= y)
          comp2 f = if charobstaclecompare char obstacle <= 0 then (f x >= y) else (f x <= y)
      go2 :: Point -> Location
      go2 (x, y) = (round $ x / 70, round $ y / 70)

blocksvision5 :: Maze -> Location -> Int -> Location -> [Location]
blocksvision5 maze char i obstacle
  | MAP.lookup obstacle maze == Nothing = []
  | isEmptyBlock $ MAP.lookup obstacle maze = []
  | otherwise = blockedsquare
    where
      charobstaclecompare (x1,y1) (x2,y2) = y2 - y1
      charcenter = cellCenter $ vanillaLocationtoCoords char
      mazeblock = case MAP.lookup obstacle maze of
        Nothing -> (emptyblock Dark)
        Just y -> y
      northwall = wallcoordinatesEast obstacle mazeblock
      lines = case northwall of
        Nothing -> Nothing 
        Just (a, b) -> Just (linefunction2 charcenter a, linefunction2 charcenter b, linefunction2 a b)
      potentialcentres = fmap (cellCenter . vanillaLocationtoCoords) $ visionfield char i
      blockedsquare = fmap go2 $ filter go potentialcentres
      go :: Point -> Bool
      go (x, y) = case lines of
        Nothing -> False
        Just (a, b, c) -> comp1 a && comp2 b && (c y < x)
        where
          comp1 f = if charobstaclecompare char obstacle >= 0 then (f y <= x) else (f y >= x)
          comp2 f = if charobstaclecompare char obstacle <= 0 then (f y <= x) else (f y >= x)
      go2 :: Point -> Location
      go2 (x, y) = (round $ x / 70, round $ y / 70)

blocksvision6 :: Maze -> Location -> Int -> Location -> [Location]
blocksvision6 maze char i obstacle
  | MAP.lookup obstacle maze == Nothing = []
  | isEmptyBlock $ MAP.lookup obstacle maze = []
  | otherwise = blockedsquare
    where
      charobstaclecompare (x1,y1) (x2,y2) = y2 - y1
      charcenter = cellCenter $ vanillaLocationtoCoords char
      mazeblock = case MAP.lookup obstacle maze of
        Nothing -> (emptyblock Dark)
        Just y -> y
      northwall = wallcoordinatesEast obstacle mazeblock
      lines = case northwall of
        Nothing -> Nothing 
        Just (a, b) -> Just (linefunction2 charcenter a, linefunction2 charcenter b, linefunction2 a b)
      potentialcentres = fmap (cellCenter . vanillaLocationtoCoords) $ visionfield char i
      blockedsquare = fmap go2 $ filter go potentialcentres
      go :: Point -> Bool
      go (x, y) = case lines of
        Nothing -> False
        Just (a, b, c) -> comp1 a && comp2 b && (c y > x)
        where
          comp1 f = if charobstaclecompare char obstacle >= 0 then (f y >= x) else (f y <= x)
          comp2 f = if charobstaclecompare char obstacle <= 0 then (f y >= x) else (f y <= x)
      go2 :: Point -> Location
      go2 (x, y) = (round $ x / 70, round $ y / 70)

blocksvision7 :: Maze -> Location -> Int -> Location -> [Location]
blocksvision7 maze char i obstacle
  | MAP.lookup obstacle maze == Nothing = []
  | isEmptyBlock $ MAP.lookup obstacle maze = []
  | otherwise = blockedsquare
    where
      charobstaclecompare (x1,y1) (x2,y2) = y2 - y1
      charcenter = cellCenter $ vanillaLocationtoCoords char
      mazeblock = case MAP.lookup obstacle maze of
        Nothing -> (emptyblock Dark)
        Just y -> y
      northwall = wallcoordinatesWest obstacle mazeblock
      lines = case northwall of
        Nothing -> Nothing 
        Just (a, b) -> Just (linefunction2 charcenter a, linefunction2 charcenter b, linefunction2 a b)
      potentialcentres = fmap (cellCenter . vanillaLocationtoCoords) $ visionfield char i
      blockedsquare = fmap go2 $ filter go potentialcentres
      go :: Point -> Bool
      go (x, y) = case lines of
        Nothing -> False
        Just (a, b, c) -> comp1 a && comp2 b && (c y < x)
        where
          comp1 f = if charobstaclecompare char obstacle <= 0 then (f y <= x) else (f y >= x)
          comp2 f = if charobstaclecompare char obstacle >= 0 then (f y <= x) else (f y >= x)
      go2 :: Point -> Location
      go2 (x, y) = (round $ x / 70, round $ y / 70)

blocksvision8 :: Maze -> Location -> Int -> Location -> [Location]
blocksvision8 maze char i obstacle
  | MAP.lookup obstacle maze == Nothing = []
  | isEmptyBlock $ MAP.lookup obstacle maze = []
  | otherwise = blockedsquare
    where
      charobstaclecompare (x1,y1) (x2,y2) = y2 - y1
      charcenter = cellCenter $ vanillaLocationtoCoords char
      mazeblock = case MAP.lookup obstacle maze of
        Nothing -> (emptyblock Dark)
        Just y -> y
      northwall = wallcoordinatesWest obstacle mazeblock
      lines = case northwall of
        Nothing -> Nothing 
        Just (a, b) -> Just (linefunction2 charcenter a, linefunction2 charcenter b, linefunction2 a b)
      potentialcentres = fmap (cellCenter . vanillaLocationtoCoords) $ visionfield char i
      blockedsquare = fmap go2 $ filter go potentialcentres
      go :: Point -> Bool
      go (x, y) = case lines of
        Nothing -> False
        Just (a, b, c) -> comp1 a && comp2 b && (c y > x)
        where
          comp1 f = if charobstaclecompare char obstacle <= 0 then (f y >= x) else (f y <= x)
          comp2 f = if charobstaclecompare char obstacle >= 0 then (f y >= x) else (f y <= x)
      go2 :: Point -> Location
      go2 (x, y) = (round $ x / 70, round $ y / 70)

linefunction :: Point -> Point -> (Float -> Float)
linefunction (x1, y1) (x2, y2)
        | x2 == x1 = (\x -> x * (y2 - y1) / (0.00000000000000000000000000000000001) - ((y2 - y1) / (0.00000000000000000000000000000000001) * x1) + y1)
        | otherwise = (\x -> x * (y2 - y1) / (x2 - x1) - ((y2 - y1) / (x2 - x1) * x1) + y1)

linefunctioninverse :: Point -> Point -> (Float -> Float)
linefunctioninverse (x1, y1) (x2, y2)
        | y2 == y1 = (\x -> x * (x2 - x1) / (0.00000000000000000000000000000000001) + ((x2 - x1) / (0.00000000000000000000000000000000001) * x2) + y2)
        | otherwise = (\x -> x * (x2 - x1) / (y2 - y1) + (x2 * (x2 - x1) / (y2 - y1)) + y2 - y1)

linefunction2 :: Point -> Point -> (Float -> Float)
linefunction2 (x1, y1) (x2, y2)
        | y2 == y1 = (\x -> x * (x2 - x1) / (0.00000000000000000000000000000000001) - ((x2 - x1) / (0.00000000000000000000000000000000001) * y1) + x1)
        | otherwise = (\x -> x * (x2 - x1) / (y2 - y1) - ((x2 - x1) / (y2 - y1) * y1) + x1)

wallCoordinates :: Location -> MazeNode -> [(Point,Point)]
wallCoordinates loc (MazeBlock north east south west _ _ _ _) = northnew ++ eastnew ++ southnew ++ westnew
  where
    coords = vanillaLocationtoCoords loc
    northnew = case north of 
      Hall -> []
      (DoorOpen _ _) -> []
      (Wall _ _) -> [(cellTopLeft coords, cellTopRight coords)]
      (DoorClosed _ _ _) -> [(cellTopLeft coords, cellTopRight coords)]
      _ -> []
    eastnew = case east of 
      Hall -> []
      (Wall _ _) -> [(cellTopRight coords, cellBottomRight coords)]
      (DoorOpen _ _) -> []
      (DoorClosed _ _ _) -> [(cellTopRight coords, cellBottomRight coords)]
      _ -> []
    southnew = case south of 
      Hall -> []
      (Wall _ _) -> [(cellBottomRight coords, cellBottomLeft coords)]
      (DoorOpen _ _) -> []
      (DoorClosed _ _ _) -> [(cellBottomRight coords, cellBottomLeft coords)]
      _ -> []
    westnew = case west of 
      Hall -> []
      (Wall _ _) -> [(cellBottomLeft coords, cellTopLeft coords)]
      (DoorOpen _ _) -> []
      (DoorClosed _ _ _) -> [(cellBottomLeft coords, cellTopLeft coords)]
      _ -> []

wallcoordinatesNorth :: Location -> MazeNode -> (Maybe (Point,Point))
wallcoordinatesNorth loc (MazeBlock north _ _ _ _ _ _ _) = northnew
  where
    coords = vanillaLocationtoCoords loc
    northnew = case north of 
      Hall -> Nothing
      (DoorOpen _ _) -> Nothing
      (Wall _ _) -> Just $ (cellTopLeft coords, cellTopRight coords)
      (DoorClosed _ _ _) -> Just $ (cellTopLeft coords, cellTopRight coords)
      _ -> Nothing

wallcoordinatesEast :: Location -> MazeNode -> (Maybe (Point,Point))
wallcoordinatesEast loc (MazeBlock _ east _ _ _ _ _ _) = eastnew
  where
    coords = vanillaLocationtoCoords loc
    eastnew = case east of 
      Hall -> Nothing
      (Wall _ _) -> Just $ (cellTopRight coords, cellBottomRight coords)
      (DoorOpen _ _) -> Nothing
      (DoorClosed _ _ _) -> Just $ (cellTopRight coords, cellBottomRight coords)
      _ -> Nothing

wallcoordinatesSouth :: Location -> MazeNode -> (Maybe (Point,Point))
wallcoordinatesSouth loc (MazeBlock _ _ south _ _ _ _ _) = southnew
  where
    coords = vanillaLocationtoCoords loc
    southnew = case south of 
      Hall -> Nothing
      (Wall _ _) -> Just $ (cellBottomLeft coords, cellBottomRight coords)
      (DoorOpen _ _) -> Nothing
      (DoorClosed _ _ _) -> Just $ (cellBottomLeft coords, cellBottomRight coords)
      _ -> Nothing

wallcoordinatesWest :: Location -> MazeNode -> (Maybe (Point,Point))
wallcoordinatesWest loc (MazeBlock _ _ _ west _ _ _ _) = weatnew
  where
    coords = vanillaLocationtoCoords loc
    weatnew = case west of 
      Hall -> Nothing
      (Wall _ _) -> Just $ (cellBottomLeft coords, cellTopLeft coords)
      (DoorOpen _ _) -> Nothing
      (DoorClosed _ _ _) -> Just $ (cellBottomLeft coords, cellTopLeft coords)
      _ -> Nothing

toggledoor :: Maybe Int -> Portal -> (Portal, String)
toggledoor _ (DoorOpen x y) = ((DoorClosed x y Nothing) , "Door Closed")
toggledoor _ (DoorClosed x y Nothing) = ((DoorOpen x y) , "Door Open")
toggledoor mint (DoorClosed x y mint2)
  | mint >= mint2  = ((DoorOpen x y) , "You successfully pick the lock, " ++ (show mint) ++ " vs DC " ++ (show mint2))
  | otherwise = ((DoorClosed x y mint2) , "You fail to pick the lock, " ++ (show mint))
toggledoor _ x = (x,"")

openDoors :: Maybe Int -> MazeNode -> (MazeNode, String)
openDoors mint maze@(MazeBlock a b c d e f g i) = 
  (MazeBlock (fst $ toggledoor mint a) (fst $ toggledoor mint b) (fst $ toggledoor mint c) (fst $ toggledoor mint d) e f g i,
  foldl (\x y -> (snd $ toggledoor mint y) ++ x) [] maze)

maybeInt2Lens :: Maybe Int -> (Lens' (MazeBlock Portal) Portal)
maybeInt2Lens mint = case mint of
  (Just 1) -> (#runNorth)
  (Just 2) -> (#runEast)
  (Just 3) -> (#runSouth)
  (Just 4) -> (#runWest)
  (Just 0) -> (#runWest)
  _ -> (#runNorth)

alterlocation :: VisionCategory -> Location -> Maybe Int -> Portal -> Maze -> Maze
alterlocation vis (x,y) mint port maze = go tuples maze
  where
    tuples = (\x -> (x, set (maybeInt2Lens mint) port $ MAP.findWithDefault (emptyblock vis) x maze)) (x,y) : tpllist
    tpllist
      | mint == (Just 2) = [(\x -> (x, set (#runWest) port2 $ MAP.findWithDefault (emptyblock vis) x maze)) (x+1,y)]
      | mint == (Just 3) = [(\x -> (x, set (#runNorth) port2 $ MAP.findWithDefault (emptyblock vis) x maze)) (x,y-1)]
      | mint == (Just 4) = [(\x -> (x, set (#runEast) port2 $ MAP.findWithDefault (emptyblock vis) x maze)) (x-1,y)]
      | otherwise = [(\x -> (x, set (#runSouth) port2 $ MAP.findWithDefault (emptyblock vis) x maze)) (x,y+1)]
    go [] mazep = mazep
    go (a:as) mazep = go as (uncurry MAP.insert a mazep)
    port2 = if isDoorClosed port then Hall else port

alterlocation2 :: [Action (Attack  Int) Roll] -> Location -> Maybe Int -> Maze -> Maze
alterlocation2 attack (x,y) mint maze = go tuples maze
  where
    tuples = (\x -> (over (maybeInt2Lens mint) (attackMap attack), x)) (x,y) : tpllist
    tpllist
      | mint == (Just 2) = [(\x -> (over (#runWest) (attackMap attack), x)) (x+1,y)]
      | mint == (Just 3) = [(\x -> (over (#runNorth) (attackMap attack), x)) (x,y-1)]
      | mint == (Just 4) = [(\x -> (over (#runEast) (attackMap attack), x)) (x-1,y)]
      | otherwise = [(\x -> (over (#runSouth) (attackMap attack), x)) (x,y+1)]
    go [] mazep = mazep
    go (a:as) mazep = go as (uncurry MAP.adjust a mazep)

addlight :: VisionCategory -> Location -> Vision -> Maze -> Maze
addlight vis loc visi maze = MAP.alter go loc maze
  where
    go :: Maybe MazeNode -> Maybe MazeNode
    go mmb = case mmb of
      Nothing -> Just $ set (#runLight) (Just visi) (emptyblock vis)
      Just x -> Just $ set (#runLight) (Just visi) x

openlocaldoors :: VisionCategory -> Maybe Int -> Location -> Maze -> (Maze, [String])
openlocaldoors vis mint (x,y) maze = (go tuples maze,strings)
  where
    locations = [(x,y), (x+1,y), (x - 1,y), (x, y+1), (x, y - 1)]
    tuples = (\x -> (x, fst . openDoors mint $ MAP.findWithDefault (emptyblock vis) x maze)) (x,y) : [(\x -> (x, over (#runWest) (\x -> fst $ toggledoor mint x) $ MAP.findWithDefault (emptyblock vis) x maze)) (x+1,y),
      (\x -> (x, over (#runEast) (\x -> fst $ toggledoor mint x) $ MAP.findWithDefault (emptyblock vis) x maze)) (x-1,y), (\x -> (x, over (#runSouth) (\x -> fst $ toggledoor mint x) $ MAP.findWithDefault (emptyblock vis) x maze)) (x,y+1),
      (\x -> (x, over (#runNorth) (\x -> fst $ toggledoor mint x) $ MAP.findWithDefault (emptyblock vis) x maze)) (x,y-1)]
    go [] mazep = mazep
    go (a:as) mazep = go as (uncurry MAP.insert a mazep)
    strings = (\x -> snd . openDoors mint $ MAP.findWithDefault (emptyblock vis) x maze) (x,y) : [(\x -> snd $ toggledoor mint . view (#runWest) $ MAP.findWithDefault (emptyblock vis) x maze) (x+1,y),
      (\x -> snd . toggledoor mint . view (#runEast) $ MAP.findWithDefault (emptyblock vis) x maze) (x-1,y), (\x -> snd . toggledoor mint . view (#runSouth) $ MAP.findWithDefault (emptyblock vis) x maze) (x,y+1),
      (\x -> snd . toggledoor mint . view (#runNorth) $ MAP.findWithDefault (emptyblock vis) x maze) (x,y-1)]

vanillaLocationtoCoords :: Location -> CellCoordinates
vanillaLocationtoCoords (x, y) = CellCoordinates
  (centerX, centerY) -- Center
  (centerX - halfCell, centerY + halfCell) -- Top Left
  (centerX + halfCell, centerY + halfCell) -- Top Right
  (centerX - halfCell, centerY - halfCell) -- Bottom Left
  (centerX + halfCell, centerY - halfCell) -- Bottom Right
  where
    (centerX, centerY) =
      ((fromIntegral x) * 70
      ,(fromIntegral y) * 70)
    halfCell = 35
  
removelights :: Maze -> Maze
removelights maze = fmap (set (#runLight) Nothing) maze