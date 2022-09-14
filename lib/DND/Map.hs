{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module DND.Maps
where

import Control.Lens (view, set, over)
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
import Data.List (delete)
import System.IO (Handle, IOMode (WriteMode, ReadMode), hPrint, hClose, openFile, hShow, hGetContents)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Codec.BMP
import Graphics.UI.GLUT.Fonts

data Portal = Wall | Hall | DoorOpen | DoorClosed deriving (Eq, Show, Read)

data MazeBlock a = MazeBlock {runNorth :: a, runEast :: a, runSouth :: a, runWest :: a} deriving (Eq, Show, Read, Generic)

type MazeNode = MazeBlock Portal

emptyblock :: MazeNode
emptyblock = MazeBlock Hall Hall Hall Hall

type Maze = MAP.Map Location MazeNode

maze :: Maze
maze = MAP.fromList $ room (4,1) (6,10) ++ room (4,12) (6,2) ++ room (8,15) (3,5) ++ walll (11,3) 9 (MazeBlock Hall Hall Wall Hall) ++ room (21, 1) (6,10) 
  ++ room (21, 12) (6,2) ++ [((19, 19), MazeBlock Hall Wall Wall Wall),((19, 20), MazeBlock Wall Wall Hall DoorClosed), ((20, 19), MazeBlock Wall Wall DoorClosed Wall)] ++ room (24, 19) (4,3)
  ++ room (27, 23) (2,1) ++ walll (12,23) 5 (MazeBlock Wall Hall Hall Hall) ++ [((18, 23), MazeBlock Wall Wall Hall Hall),((11, 23), MazeBlock Wall Hall Hall Wall)]
  ++ wallh (11,21) 1 (MazeBlock Hall Hall Hall Wall) ++ wallh (18,21) 1 (MazeBlock Hall Wall Hall Hall) ++ walll (21,19) 2 (MazeBlock Hall Hall Wall Hall) ++ walll (22,17) 10 (MazeBlock Hall Hall Wall Hall)
  ++ wallh (22,15) 1 (MazeBlock Hall Hall Hall Wall) ++ wallh (30,14) 2 (MazeBlock Hall Wall Hall Hall) ++ [((33,17), MazeBlock Hall DoorClosed Wall Hall),((33,18), MazeBlock Hall Wall Hall Hall),((34,19), MazeBlock Hall Hall Wall Hall),((35,19), MazeBlock Hall Hall Wall Hall),((36,19), MazeBlock Hall Hall Wall Wall)]
  ++ walll (30,13) 6 (MazeBlock Wall Hall Hall Hall) ++ wallh (37,14) 4 (MazeBlock Hall Hall Hall Wall) ++ wallh (36,20) 2 (MazeBlock Hall Hall Hall Wall) ++ walll (30,23) 5 (MazeBlock Hall Hall Wall Hall)
  ++ [((10,7), (MazeBlock Hall DoorClosed Hall Hall)), ((10,8), (MazeBlock Hall DoorClosed Hall Hall)), ((21,7), (MazeBlock Hall Hall Hall DoorClosed)), ((21,8), (MazeBlock Hall Hall Hall DoorClosed)), ((6,11), (MazeBlock DoorClosed Hall Hall Hall)), ((6,12), (MazeBlock Hall Hall Hall Hall)), ((10,13), (MazeBlock Hall DoorClosed Hall Hall)),
  ((11,17), (MazeBlock Hall DoorClosed Hall Hall)), ((21,17), (MazeBlock Hall DoorClosed Hall Hall)), ((21,18), (MazeBlock Hall DoorClosed Hall Hall)), ((21,13), (MazeBlock Hall Hall Hall DoorClosed)), ((23,12), (MazeBlock Hall Hall Hall Hall)), ((23,11), (MazeBlock DoorClosed Hall Hall Hall)), ((22, 19), MazeBlock Hall Hall DoorClosed Hall), 
  ((26, 19), MazeBlock Hall Hall DoorClosed Hall), ((29, 23), MazeBlock Hall Wall DoorClosed Hall)]

mazea :: Maze
mazea = MAP.fromList $ wall1 ++ wall2 ++ wall3 ++ wall4
  where
    wall1 = fmap (\x -> (((x, 12) :: Location), MazeBlock Wall Hall Hall Hall)) [4 .. 12]
    wall2 = fmap (\x -> (((12, x) :: Location), MazeBlock Hall Wall Hall Hall)) [7 .. 11]
    wall3 = fmap (\x -> (((x, 8) :: Location), MazeBlock Hall Hall Wall Hall)) [3 .. 11]
    wall4 = fmap (\x -> (((3, x) :: Location), MazeBlock Hall Hall Hall Wall)) [9 .. 12]

room :: Location -> Location -> [(Location, MazeNode)]
room (x, y) (l, h) = wall1 ++ wall2 ++ wall3 ++ wall4 ++ 
  [((x,y), MazeBlock Hall Hall Wall Wall), ((x,y + h), MazeBlock Wall Hall Hall Wall), ((x + l,y + h), MazeBlock Wall Wall Hall Hall), ((x + l,y), MazeBlock Hall Wall Wall Hall)]
  where
    wall1 = fmap (\x' -> (((x', (y + h )) :: Location), MazeBlock Wall Hall Hall Hall)) [(x + 1) .. (x + l - 1)]
    wall2 = fmap (\y' -> ((((x + l ), y') :: Location), MazeBlock Hall Wall Hall Hall)) [(y + 1) .. (y + h - 1)]
    wall3 = fmap (\x' -> (((x', y ) :: Location), MazeBlock Hall Hall Wall Hall)) [(x + 1) .. (x + l - 1)]
    wall4 = fmap (\y' -> (((x, y') :: Location), MazeBlock Hall Hall Hall Wall)) [(y + 1) .. (y + h - 1)]

walll :: Location -> Int -> MazeNode -> [(Location, MazeNode)]
walll (x, y) l mazenode = fmap (\x' -> (((x', y ) :: Location), mazenode)) [x .. (x + l)]

wallh :: Location -> Int -> MazeNode -> [(Location, MazeNode)]
wallh (x, y) h mazenode = fmap (\y' -> (((x, y') :: Location), mazenode)) [y .. (y + h)]

maze2 :: Maze
maze2 = MAP.fromList $ wall1 ++ wall2 ++ wall3 ++ wall4
  where
    wall1 = fmap (\x -> (((x, 12) :: Location), MazeBlock Wall Hall Hall Hall)) [8 .. 12]
    wall2 = fmap (\x -> (((12, x) :: Location), MazeBlock Hall Wall Hall Hall)) [8 .. 12]
    wall3 = fmap (\x -> (((x, 8) :: Location), MazeBlock Hall Hall Wall Hall)) [8 .. 12]
    wall4 = fmap (\x -> (((8, x) :: Location), MazeBlock Hall Hall Hall Wall)) [8 .. 12]

meleedistance :: Location -> Location -> Int
meleedistance (a1, b1) (a2, b2) = distance
  where
    deltaa = abs $ a1 - a2
    deltab = abs $ b1 - b2
    distance = max deltaa deltab

visionfield' :: Location -> [Location]
visionfield' (a,b) = do
  avariant <- [(+ a)] <*> [(-12) .. 12]
  bvariant <- [(+ b)] <*> [(-12) .. 12]
  return (avariant, bvariant)

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

visionfield :: Location -> [Location]
visionfield loc = sortOn (meleedistance loc) $ visionfield' loc

finalvisionfield :: Maze -> Location -> [Location]
finalvisionfield maze1 loc = (visionfield loc) \\ ((concat $ fmap (blocksvision maze1 loc) (visionfieldNorth loc)) ++ (concat $ fmap (blocksvision2 maze1 loc) (visionfieldSouth loc)) ++
  (concat $ fmap (blocksvision3 maze1 loc) (visionfieldNorth2 loc)) ++ (concat $ fmap (blocksvision4 maze1 loc) (visionfieldSouth2 loc)) ++ 
  (concat $ fmap (blocksvision5 maze1 loc) (visionfieldEast loc)) ++ (concat $ fmap (blocksvision6 maze1 loc) (visionfieldWest loc)) ++ 
  (concat $ fmap (blocksvision7 maze1 loc) (visionfieldEast2 loc)) ++ (concat $ fmap (blocksvision8 maze1 loc) (visionfieldWest2 loc)))

blocksvision :: Maze -> Location -> Location -> [Location]
blocksvision maze char obstacle
  | MAP.lookup obstacle maze == Nothing = []
  | MAP.lookup obstacle maze == (Just $ emptyblock) = []
  | otherwise = blockedsquare
    where
      charobstaclecompare (x1,y1) (x2,y2) = x2 - x1
      charcenter = cellCenter $ vanillaLocationtoCoords char
      mazeblock = case MAP.lookup obstacle maze of
        Nothing -> emptyblock
        Just y -> y
      northwall = wallcoordinatesNorth obstacle mazeblock
      lines = case northwall of
        Nothing -> Nothing 
        Just (a, b) -> Just (linefunction charcenter a, linefunction charcenter b, linefunction a b)
      potentialcentres = fmap (cellCenter . vanillaLocationtoCoords) $ visionfield char
      blockedsquare = fmap go2 $ filter go potentialcentres
      go :: Point -> Bool
      go (x, y) = case lines of
        Nothing -> False
        Just (a, b , c) ->  (if charobstaclecompare char obstacle <= 0 then ((a x) <= y) else ((a x) >= y)) && 
                            (if charobstaclecompare char obstacle >= 0 then ((b x) <= y) else ((b x) >= y)) && (c x < y)
      go2 :: Point -> Location
      go2 (x, y) = (round $ x / 70, round $ y / 70)

blocksvision2 :: Maze -> Location -> Location -> [Location]
blocksvision2 maze char obstacle
  | MAP.lookup obstacle maze == Nothing = []
  | MAP.lookup obstacle maze == (Just $ emptyblock) = []
  | otherwise = blockedsquare
    where
      charobstaclecompare (x1,y1) (x2,y2) = x2 - x1
      charcenter = cellCenter $ vanillaLocationtoCoords char
      mazeblock = case MAP.lookup obstacle maze of
        Nothing -> emptyblock
        Just y -> y
      northwall = wallcoordinatesNorth obstacle mazeblock
      lines = case northwall of
        Nothing -> Nothing 
        Just (a, b) -> Just (linefunction charcenter a, linefunction charcenter b, linefunction a b)
      potentialcentres = fmap (cellCenter . vanillaLocationtoCoords) $ visionfield char
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

blocksvision3 :: Maze -> Location -> Location -> [Location]
blocksvision3 maze char obstacle
  | MAP.lookup obstacle maze == Nothing = []
  | MAP.lookup obstacle maze == (Just $ emptyblock) = []
  | otherwise = blockedsquare
    where
      charobstaclecompare (x1,y1) (x2,y2) = x1 - x2
      charcenter = cellCenter $ vanillaLocationtoCoords char
      mazeblock = case MAP.lookup obstacle maze of
        Nothing -> emptyblock
        Just y -> y
      northwall = wallcoordinatesSouth obstacle mazeblock
      lines = case northwall of
        Nothing -> Nothing 
        Just (a, b) -> Just (linefunction charcenter a, linefunction charcenter b, linefunction a b)
      potentialcentres = fmap (cellCenter . vanillaLocationtoCoords) $ visionfield char
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

blocksvision4 :: Maze -> Location -> Location -> [Location]
blocksvision4 maze char obstacle
  | MAP.lookup obstacle maze == Nothing = []
  | MAP.lookup obstacle maze == (Just $ emptyblock) = []
  | otherwise = blockedsquare
    where
      charobstaclecompare (x1,y1) (x2,y2) = x1 -x2
      charcenter = cellCenter $ vanillaLocationtoCoords char
      mazeblock = case MAP.lookup obstacle maze of
        Nothing -> emptyblock
        Just y -> y
      northwall = wallcoordinatesSouth obstacle mazeblock
      lines = case northwall of
        Nothing -> Nothing 
        Just (a, b) -> Just (linefunction charcenter a, linefunction charcenter b, linefunction a b)
      potentialcentres = fmap (cellCenter . vanillaLocationtoCoords) $ visionfield char
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

blocksvision5 :: Maze -> Location -> Location -> [Location]
blocksvision5 maze char obstacle
  | MAP.lookup obstacle maze == Nothing = []
  | MAP.lookup obstacle maze == (Just $ emptyblock) = []
  | otherwise = blockedsquare
    where
      charobstaclecompare (x1,y1) (x2,y2) = y2 - y1
      charcenter = cellCenter $ vanillaLocationtoCoords char
      mazeblock = case MAP.lookup obstacle maze of
        Nothing -> emptyblock
        Just y -> y
      northwall = wallcoordinatesEast obstacle mazeblock
      lines = case northwall of
        Nothing -> Nothing 
        Just (a, b) -> Just (linefunction2 charcenter a, linefunction2 charcenter b, linefunction2 a b)
      potentialcentres = fmap (cellCenter . vanillaLocationtoCoords) $ visionfield char
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

blocksvision6 :: Maze -> Location -> Location -> [Location]
blocksvision6 maze char obstacle
  | MAP.lookup obstacle maze == Nothing = []
  | MAP.lookup obstacle maze == (Just $ emptyblock) = []
  | otherwise = blockedsquare
    where
      charobstaclecompare (x1,y1) (x2,y2) = y2 - y1
      charcenter = cellCenter $ vanillaLocationtoCoords char
      mazeblock = case MAP.lookup obstacle maze of
        Nothing -> emptyblock
        Just y -> y
      northwall = wallcoordinatesEast obstacle mazeblock
      lines = case northwall of
        Nothing -> Nothing 
        Just (a, b) -> Just (linefunction2 charcenter a, linefunction2 charcenter b, linefunction2 a b)
      potentialcentres = fmap (cellCenter . vanillaLocationtoCoords) $ visionfield char
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

blocksvision7 :: Maze -> Location -> Location -> [Location]
blocksvision7 maze char obstacle
  | MAP.lookup obstacle maze == Nothing = []
  | MAP.lookup obstacle maze == (Just $ emptyblock) = []
  | otherwise = blockedsquare
    where
      charobstaclecompare (x1,y1) (x2,y2) = y2 - y1
      charcenter = cellCenter $ vanillaLocationtoCoords char
      mazeblock = case MAP.lookup obstacle maze of
        Nothing -> emptyblock
        Just y -> y
      northwall = wallcoordinatesWest obstacle mazeblock
      lines = case northwall of
        Nothing -> Nothing 
        Just (a, b) -> Just (linefunction2 charcenter a, linefunction2 charcenter b, linefunction2 a b)
      potentialcentres = fmap (cellCenter . vanillaLocationtoCoords) $ visionfield char
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

blocksvision8 :: Maze -> Location -> Location -> [Location]
blocksvision8 maze char obstacle
  | MAP.lookup obstacle maze == Nothing = []
  | MAP.lookup obstacle maze == (Just $ emptyblock) = []
  | otherwise = blockedsquare
    where
      charobstaclecompare (x1,y1) (x2,y2) = y2 - y1
      charcenter = cellCenter $ vanillaLocationtoCoords char
      mazeblock = case MAP.lookup obstacle maze of
        Nothing -> emptyblock
        Just y -> y
      northwall = wallcoordinatesWest obstacle mazeblock
      lines = case northwall of
        Nothing -> Nothing 
        Just (a, b) -> Just (linefunction2 charcenter a, linefunction2 charcenter b, linefunction2 a b)
      potentialcentres = fmap (cellCenter . vanillaLocationtoCoords) $ visionfield char
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

linefunction2 :: Point -> Point -> (Float -> Float)
linefunction2 (x1, y1) (x2, y2)
        | y2 == y1 = (\x -> x * (x2 - x1) / (0.00000000000000000000000000000000001) - ((x2 - x1) / (0.00000000000000000000000000000000001) * y1) + x1)
        | otherwise = (\x -> x * (x2 - x1) / (y2 - y1) - ((x2 - x1) / (y2 - y1) * y1) + x1)

wallcoordinatesNorth :: Location -> MazeNode -> (Maybe (Point,Point))
wallcoordinatesNorth loc (MazeBlock north _ _ _) = northnew
  where
    coords = vanillaLocationtoCoords loc
    northnew = case north of 
      Hall -> Nothing
      DoorOpen -> Nothing
      Wall -> Just $ (cellTopLeft coords, cellTopRight coords)
      DoorClosed -> Just $ (cellTopLeft coords, cellTopRight coords)

wallcoordinatesEast :: Location -> MazeNode -> (Maybe (Point,Point))
wallcoordinatesEast loc (MazeBlock _ east _ _) = eastnew
  where
    coords = vanillaLocationtoCoords loc
    eastnew = case east of 
      Hall -> Nothing
      Wall -> Just $ (cellTopRight coords, cellBottomRight coords)
      DoorOpen -> Nothing
      DoorClosed -> Just $ (cellTopRight coords, cellBottomRight coords)

wallcoordinatesSouth :: Location -> MazeNode -> (Maybe (Point,Point))
wallcoordinatesSouth loc (MazeBlock _ _ south _) = southnew
  where
    coords = vanillaLocationtoCoords loc
    southnew = case south of 
      Hall -> Nothing
      Wall -> Just $ (cellBottomLeft coords, cellBottomRight coords)
      DoorOpen -> Nothing
      DoorClosed -> Just $ (cellBottomLeft coords, cellBottomRight coords)

wallcoordinatesWest :: Location -> MazeNode -> (Maybe (Point,Point))
wallcoordinatesWest loc (MazeBlock _ _ _ west) = weatnew
  where
    coords = vanillaLocationtoCoords loc
    weatnew = case west of 
      Hall -> Nothing
      Wall -> Just $ (cellBottomLeft coords, cellTopLeft coords)
      DoorOpen -> Nothing
      DoorClosed -> Just $ (cellBottomLeft coords, cellTopLeft coords)

toggledoor :: Portal -> Portal
toggledoor DoorOpen = DoorClosed
toggledoor DoorClosed = DoorOpen
toggledoor x = x

openDoors :: MazeNode -> MazeNode
openDoors (MazeBlock a b c d) = MazeBlock (toggledoor a) (toggledoor b) (toggledoor c) (toggledoor d)

openlocaldoors :: Location -> Maze -> Maze
openlocaldoors (x,y) maze = go tuples maze
  where
    locations = [(x,y), (x+1,y), (x - 1,y), (x, y+1), (x, y - 1)]
    tuples = (\x -> (x, openDoors $ MAP.findWithDefault emptyblock x maze)) (x,y) : [(\x -> (x, over (#runWest) toggledoor $ MAP.findWithDefault emptyblock x maze)) (x+1,y),
      (\x -> (x, over (#runEast) toggledoor $ MAP.findWithDefault emptyblock x maze)) (x-1,y), (\x -> (x, over (#runSouth) toggledoor $ MAP.findWithDefault emptyblock x maze)) (x,y+1),
      (\x -> (x, over (#runNorth) toggledoor $ MAP.findWithDefault emptyblock x maze)) (x,y-1)]
    go [] mazep = mazep
    go (a:as) mazep = go as (uncurry MAP.insert a mazep)