{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DND.SPELLS.Spell
where

import Control.Lens (set, over, view)
import Data.Generics.Labels ()
import GHC.Generics (Generic)
import System.Random (StdGen)
import Control.Lens.Indexed
import Data.Traversable
import Control.Monad.Trans.State.Strict (runState, state)

import DND.DAMAGEDICE.DamageDice (DamageDice (damagedice))
import DND.DAMAGEDICE.Elemental (Elemental (Fire, Acid))

import DND.DEFENSE.Defense (Target)

import DND.STATDAMAGE.StatDamage (StatDamage)

import DND.STATUSEFFECTS.Effect (Effect)
import DND.STATUSEFFECTS.Effects (Temporal, setmodifyer, viewtempmodifyier)

import DND.Action (action, Action(Action, runF))
import DND.Roll (runRoll, Dice)



data SpellSchool = Abjuration | Conjuration | Divination | Enchantment | Evocation | Illusion | Necromancy | Transmutation | Universal deriving (Ord, Show, Eq, Generic)

data SpellTarget = Caster | Allies Int | Enemies Int | SingleTarget Int | AlliesinRange SpellArea Int | EnemiesinRange SpellArea Int | AllInRange SpellArea Int | TargetinginRange Int  | MapArea Int Int deriving (Ord, Show, Eq, Generic)

getRange :: SpellTarget -> Int
getRange (TargetinginRange x) = x
getRange (AllInRange _ x) = x
getRange (EnemiesinRange _ x) = x
getRange (AlliesinRange _ x) = x
getRange (SingleTarget x) = x
getRange (Enemies x) = x
getRange (Allies x) = x
getRange (MapArea _ x) = x
getRange _ = 0

getArea :: SpellTarget -> Maybe SpellArea
getArea (AllInRange x _) = Just x
getArea (EnemiesinRange x _) = Just x
getArea (AlliesinRange x _) = Just x
getArea _ = Nothing

isAllies :: Maybe SpellTarget -> Bool
isAllies mst = case mst of
    Nothing -> False
    Just (Allies _) -> True
    _ -> False

isEnemies :: Maybe SpellTarget -> Bool
isEnemies mst = case mst of
    Nothing -> False
    Just (Enemies _) -> True
    _ -> False

isSingleTarget :: Maybe SpellTarget -> Bool
isSingleTarget mst = case mst of
    Nothing -> False
    Just (SingleTarget _) -> True
    _ -> False

isAlliesBurst :: Maybe SpellTarget -> Bool
isAlliesBurst mst = case mst of
    Nothing -> False
    (Just (AlliesinRange Bursty _)) -> True
    _ -> False

isAllBurst :: Maybe SpellTarget -> Bool
isAllBurst mst = case mst of
    Nothing -> False
    (Just (AllInRange Bursty _)) -> True
    _ -> False

isEnemiesBurst :: Maybe SpellTarget -> Bool
isEnemiesBurst mst = case mst of
    Nothing -> False
    (Just (EnemiesinRange Bursty _)) -> True
    _ -> False

isBurst :: Maybe SpellTarget -> Bool
isBurst mst = case mst of
    Nothing -> False
    (Just (AlliesinRange Bursty _)) -> True
    (Just (EnemiesinRange Bursty _)) -> True
    (Just (AllInRange Bursty _)) -> True
    _ -> False

isMapArea :: Maybe SpellTarget -> Bool
isMapArea mst = case mst of
    Nothing -> False
    (Just (MapArea _ _)) -> True
    _ -> False

isAreaTarget :: Maybe SpellTarget -> Bool
isAreaTarget  mst = case mst of
    Nothing -> False
    (Just (AlliesinRange _ _)) -> True
    (Just (EnemiesinRange _ _)) -> True
    (Just (AllInRange _ _)) -> True
    (Just (MapArea _ _)) -> True
    _ -> False

data SpellArea = Cone | Sphere Int | Line | Bursty deriving (Ord, Show, Eq, Generic)

isSphere :: Maybe SpellArea -> Bool
isSphere msa = case msa of
    Nothing -> False
    Just (Sphere _) -> True
    _ -> False

getSphereRadius :: Maybe SpellArea -> Int
getSphereRadius msa = case msa of
    Nothing -> 0
    Just (Sphere x) -> x
    _ -> 0

getMapAreaRadius :: Maybe SpellTarget -> Int
getMapAreaRadius msa = case msa of
    Nothing -> 0
    Just (MapArea x _) -> x
    _ -> 0

getAreaRadius :: SpellTarget -> Int
getAreaRadius st
    | isSphere . getArea $ st = getSphereRadius . getArea $ st
    | isMapArea . Just $ st = getMapAreaRadius . Just $ st
    | otherwise = 0

data SpellInfo = SpellInfo {name :: String
                           , apcost :: Int
                           , school :: SpellSchool
                           , spellbonus :: Int
                           , sRbonus :: Int
                           , target :: Maybe Target
                           , spelltarget :: SpellTarget} deriving (Ord, Show, Eq, Generic)

data Summon = Summon { creature :: String
                     , duration :: Int
                     , number :: Int}  deriving (Ord, Show, Eq, Generic)

data Spell a = Spell { info :: SpellInfo
                     , rolls :: SpellRoll a
                     , blockaoo :: (Bool, Bool)
                     , mdamdice :: Maybe (DamageDice a)
                     , mstatdamage :: Maybe (StatDamage a)
                     , meffect :: Maybe (Effect (Temporal a))
                     , msummon :: Maybe Summon
                     , mspecial :: Maybe String
                     , isspell :: Bool} deriving (Ord, Show, Eq, Generic)

data SpellRoll a = SpellRoll {ddroll :: a, sdroll :: a, efroll :: a, srroll :: a} deriving (Ord, Show, Eq, Generic)

emptyspellroll :: SpellRoll [Int]
emptyspellroll = SpellRoll [20] [20] [20] [20]

testspellinfo :: SpellInfo
testspellinfo = SpellInfo "Test" 2 Universal 0 0 Nothing Caster

testspell :: Spell [Int]
testspell = Spell testspellinfo emptyspellroll (False,False) Nothing Nothing Nothing Nothing Nothing True

instance Functor SpellRoll where
    fmap f (SpellRoll a1 a2 a3 a4) = SpellRoll (f a1) (f a2) (f a3) (f a4)

instance Applicative SpellRoll where
    pure a = SpellRoll a a a a
    (<*>) (SpellRoll f1 f2 f3 f4) (SpellRoll a1 a2 a3 a4) = SpellRoll (f1 a1) (f2 a2) (f3 a3) (f4 a4)

instance Foldable SpellRoll where
    foldMap f (SpellRoll a1 a2 a3 a4) = (f a1) <> (f a2) <> (f a3) <> (f a4)

instance Traversable SpellRoll where
    traverse f (SpellRoll a1 a2 a3 a4) = SpellRoll <$> f a1 <*> f a2 <*> f a3 <*> f a4

instance Monad SpellRoll where
    (>>=) (SpellRoll a1 a2 a3 a4) f = 
        SpellRoll ((ddroll . f) a1) ((sdroll . f) a2) ((efroll . f) a3) ((srroll . f) a4)
         
instance Functor Spell where
    fmap f (Spell inf rol aoo mdam mstat meff msumm mspecial issp) = Spell inf (f <$> rol) aoo ((f <$>) <$> mdam) ((f <$>) <$> mstat) (((f <$>) <$>) <$> meff) msumm mspecial issp

instance Applicative Spell where
    pure a = Spell testspellinfo (pure a) (False,False) (pure $ pure a) (pure $ pure a) ((pure . pure . pure) a) Nothing Nothing True
    (<*>) (Spell i f1 b f2 f3 f4 msumm mspecial issp) (Spell _ sr _ mdd mstd meff _ _ _) = 
        Spell i (f1 <*> sr) b ((<*>) <$> f2 <*> mdd) ((<*>) <$> f3 <*> mstd) (((<*>) <$> (((<*>) <$>) <$> f4)) <*> meff) msumm mspecial issp

instance Foldable Spell where
    foldMap f (Spell _ sr _ mdd mstd meff _ _ _) = (foldMap f sr) <> (foldMap (foldMap f) mdd) <> (foldMap (foldMap f) mstd) <> (foldMap (foldMap (foldMap f)) meff) 

instance Traversable Spell where 
    traverse f (Spell s sr aoo mdd mstd meff msumm mspecial issp) = 
        (\w x y z -> Spell s w aoo x y z msumm mspecial issp) <$> (sequenceA $ fmap f sr) <*> (sequenceA $ (sequenceA . (fmap f)) <$> mdd) <*> (sequenceA $ (sequenceA . (fmap f)) <$> mstd)
            <*> (sequenceA $ (sequenceA . (sequenceA <$>) . (fmap (f <$>))) <$> meff)
instance Monad Spell where
    return a = pure a
    (>>=) (Spell s sr aoo mdd mstd meff msumm mspecial issp) f = Spell s (sr >>= (rolls . f)) aoo (go3 >>= go4) (go3' >>= go4') (go3'' >>= go4'') msumm mspecial issp
      where
        go = (fmap damagedice mdd)
        go1 = (mdamdice . f)
        go2 = (go >>= go1)
        go3 = damagedice <$> go2
        go4 b = (set (#damagedice) b) <$> mdd
        go' = view (#dice) <$> mstd
        go1' = (mstatdamage . f)
        go2' = (go' >>= go1')
        go3' = view (#dice) <$> go2'
        go4' b = (set (#dice) b) <$> mstd
        go'' = ((viewtempmodifyier <$>)) (view (#temporal) <$> meff)
        go1'' = (meffect . f)
        go2'' = (go'' >>= go1'')
        go3'' = (viewtempmodifyier . (view (#temporal))) <$> go2''
        go4'' b = (over (#temporal) (setmodifyer b)) <$> meff

data SkillFeatProfile = None | Lesser | Greater deriving (Ord, Show, Eq, Generic)

data SpellFeat a = SF { abjuration :: a
                      , conjuration :: a
                      , divination :: a
                      , enchantment :: a
                      , evocation :: a
                      , illusion :: a
                      , necromancy :: a
                      , transmutation :: a} deriving (Ord, Show, Eq, Generic)

instance Functor SpellFeat where
    fmap = fmapDefault

instance FunctorWithIndex SpellSchool SpellFeat

instance Applicative SpellFeat where
    pure a = SF a a a a a a a a
    (<*>) (SF f1 f2 f3 f4 f5 f6 f7 f8) (SF a1 a2 a3 a4 a5 a6 a7 a8) = SF (f1 a1) (f2 a2) (f3 a3) (f4 a4) (f5 a5) (f6 a6) (f7 a7) (f8 a8)

instance Foldable SpellFeat where
    foldMap = foldMapDefault
    length _ = 8

instance FoldableWithIndex SpellSchool SpellFeat

instance Traversable SpellFeat where
    traverse = itraverse . const

instance TraversableWithIndex SpellSchool SpellFeat where
    itraverse ƒ (SF a b c d e f g h) =
        SF <$> ƒ Abjuration a <*> ƒ Conjuration b <*> ƒ Divination c <*> ƒ Enchantment d <*> ƒ Evocation e <*> ƒ Illusion f <*> ƒ Necromancy g <*> ƒ Transmutation h

sfix :: SpellSchool -> (SpellFeat a -> a)
sfix Abjuration = abjuration
sfix Conjuration = conjuration
sfix Divination = divination
sfix Enchantment = enchantment
sfix Evocation = evocation
sfix Illusion = illusion
sfix Necromancy = necromancy
sfix Transmutation = transmutation
sfix _ = evocation

rollspelldice :: StdGen -> Spell [Int] -> Spell Int
rollspelldice g s = ((runRoll <$>) . runF . fst) $ runState (action (Action s)) g

rollspelldice' :: Spell [Int] -> StdGen -> (Spell Int, StdGen)
rollspelldice' s g = go ((runRoll <$>) . runF) $ runState (action (Action s)) g
    where
        go f (a, b) = (f a, b)

rollingspelldice ::  Spell [Int] -> Dice (Spell Int)
rollingspelldice s = state $ rollspelldice' s

regenerationendingspell :: Maybe (Spell a) -> Bool
regenerationendingspell Nothing = False
regenerationendingspell (Just s) = case mdice of
    Nothing -> False
    Just y -> ((view (#elemental) y) == (Just Fire)) || ((view (#elemental) y) == (Just Acid))
    where
        mdice = mdamdice s