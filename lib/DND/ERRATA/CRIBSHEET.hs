{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}


module DND.Turn
where

import Control.Lens (view, set, over)
import Control.Monad.Trans.State.Strict
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Generics.Labels ()
import GHC.Generics (Generic)

import DND.Character
import DND.Effect (durationreduction)
type GameState = NonEmpty Character

type Game = State GameState

currentCharacter :: Game Character
currentCharacter = gets NE.head

rotateCharacters :: Game ()
rotateCharacters = modify go
  where
    go (c :| cs) = snoc cs c
      where
        snoc :: [a] -> a -> NonEmpty a
        snoc [] b = b :| []
        snoc (a : as) b = a :| (as ++ [b])

nextCharacter :: Game Character
nextCharacter = do
    character <- currentCharacter
    rotateCharacters
    return character

turn :: Game ()
turn = do
    character <- currentCharacter
    doStuffWithCharacter character
    rotateCharacters

doStuffWithCharacter :: Character -> Game ()
doStuffWithCharacter = undefined

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