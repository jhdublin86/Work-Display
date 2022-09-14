{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DND.CHARACTER.Class
where

import Control.Applicative
import Data.Generics.Labels ()
import GHC.Generics (Generic)
import Control.Lens (view, set, over)
import Control.Lens.Indexed
import Data.Traversable
import Data.List.NonEmpty (NonEmpty ((:|)))

import DND.TemporaryValue (Temporary(T), temporary, permanent, revert, revertToZero)

data Skills a = Skills { acrobatics :: a 
                       , athletics :: a
                       , bluff :: a
                       , computers :: a
                       , culture :: a
                       , diplomacy :: a
                       , disguise :: a
                       , engineering :: a
                       , intimidate :: a
                       , lifeScience :: a
                       , medicine :: a
                       , mysticism :: a
                       , perception :: a
                       , physicalScience :: a
                       , piloting :: a
                       , profession :: a
                       , senseMotive :: a
                       , sleightOfHand :: a
                       , stealth :: a
                       , survival :: a} deriving (Generic, Show, Eq, Ord)

instance Functor Skills where
    fmap f (Skills a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20) = 
      Skills (f a1) (f a2) (f a3) (f a4) (f a5) (f a6) (f a7) (f a8) (f a9) (f a10) (f a11) (f a12) (f a13) (f a14) (f a15) (f a16) (f a17) (f a18) (f a19) (f a20)

instance Applicative Skills where
    pure a = Skills a a a a a a a a a a a a a a a a a a a a
    (<*>) (Skills f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20)
        (Skills a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20) = 
        Skills (f1 a1) (f2 a2) (f3 a3) (f4 a4) (f5 a5) (f6 a6) (f7 a7) (f8 a8) (f9 a9) (f10 a10) (f11 a11) (f12 a12) 
        (f13 a13) (f14 a14) (f15 a15) (f16 a16) (f17 a17) (f18 a18) (f19 a19) (f20 a20)

data Class = Envoy | Mechanic | Mystic | Operative | Solarion | Soldier | Technomancer

data Classes a = Classes { env :: a
                  , mech :: a 
                  , myst :: a
                  , oper :: a
                  , solar :: a
                  , sold :: a
                  , techno :: a} deriving (Generic, Show, Eq, Ord)

instance Functor Classes where
    fmap = fmapDefault

instance Applicative Classes where
    pure a = Classes a a a a a a a
    (<*>) (Classes f1 f2 f3 f4 f5 f6 f7) (Classes a1 a2 a3 a4 a5 a6 a7) = 
        Classes (f1 a1) (f2 a2) (f3 a3) (f4 a4) (f5 a5) (f6 a6) (f7 a7) 
  
instance FunctorWithIndex Class Classes

instance Foldable Classes where
    foldMap = foldMapDefault
    length _ = 7

instance FoldableWithIndex Class Classes

instance Traversable Classes where
    traverse = itraverse . const

instance TraversableWithIndex Class Classes where
    itraverse ƒ (Classes a b c d e f g) =
        Classes <$> ƒ Envoy a <*> ƒ Mechanic b <*> ƒ Mystic c <*> ƒ Operative d <*> ƒ Solarion e <*> ƒ Soldier f
            <*> ƒ Technomancer g 

data ClassInfo a = ClassInfo { vp :: a
                   , hp :: a
                   , bab :: a
                   , fort :: a
                   , ref :: a
                   , wil :: a
                   , level :: a
                   , bsb :: a
                   , bhb :: a}
                   deriving (Generic, Show, Eq, Ord)

instance Functor ClassInfo where
    fmap g (ClassInfo v h ba f r w bs bh l) = ClassInfo (g v) (g h) (g ba) (g f) (g r) (g w) (g bs) (g bh) (g l)

instance Applicative ClassInfo where
    pure a = ClassInfo a a a a a a a a a
    (<*>) (ClassInfo f1 f2 f3 f4 f5 f6 f7 f8 f9) (ClassInfo a1 a2 a3 a4 a5 a6 a7 a8 a9) = ClassInfo (f1 a1) (f2 a2) (f3 a3) (f4 a4) (f5 a5) (f6 a6) (f7 a7) (f8 a8) (f9 a9)

instance Num a => Num (ClassInfo a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger 

instance (Num a) => Semigroup (ClassInfo a) where
    (<>) a b = a + b

instance (Num a) => Monoid (ClassInfo a) where
    mempty = 0

clix :: Class -> (Temporary Int -> ClassInfo (Temporary Int))
clix Envoy = envoy
clix Mechanic = mechanic
clix Mystic = mystic
clix Operative = operative
clix Solarion = solarion
clix Soldier = soldier
clix Technomancer = technomancer

collapseclasses :: Classes (Temporary Int) -> ClassInfo (Temporary Int)
collapseclasses a = ifoldMap clix a

testclasses :: Classes (Temporary Int)
testclasses = Classes 10 0 0 0 0 0 0

changelevel :: Int -> ClassInfo (Temporary Int) -> ClassInfo (Temporary Int)
changelevel int (ClassInfo a b c d e f g h i) = ClassInfo a b c d e f (go int g) h i
    where
        go int' (T (_ :| ts ) p) = T (int' :| (ts) ) p

strongSave :: (Integral a, Num a) => a -> a
strongSave 0 = 0
strongSave a = 2 + (a `div` 2)

weakSave :: (Integral a, Num a) => a -> a
weakSave a = a `div` 3

emptyclass :: ClassInfo (Temporary Int)
emptyclass = pure 0

weird :: (Num a, Eq a) => a -> a
weird 0 = 0
weird a = a

barbarian :: Temporary Int -> ClassInfo (Temporary Int)
barbarian a = ClassInfo (6 * (weird a)) (6 * (weird a)) a (strongSave a) (weakSave a) (weakSave a) (T (0 :| []) (permanent a)) 0 0

bard :: Temporary Int -> ClassInfo (Temporary Int)
bard a = ClassInfo (4 * (weird a)) (4 * (weird a)) (3 * a `div` 4) (weakSave a) (strongSave a) (strongSave a) (T (0 :| []) (permanent a)) a 0

mystic :: Temporary Int -> ClassInfo (Temporary Int)
mystic a = ClassInfo (6 * (weird a)) (6 * (weird a)) (3 * a `div` 4) (weakSave a) (weakSave a) (strongSave a) (T (0 :| []) (permanent a)) 0 a

technomancer :: Temporary Int -> ClassInfo (Temporary Int)
technomancer a = ClassInfo (5 * (weird a)) (5 * (weird a)) (3 * a `div` 4) (weakSave a) (weakSave a) (strongSave a) (T (0 :| []) (permanent a)) a 0

cleric :: Temporary Int -> ClassInfo (Temporary Int)
cleric a = ClassInfo (4 * (weird a)) (4 * (weird a)) (3 * a `div` 4) (strongSave a) (weakSave a) (strongSave a) (T (0 :| []) (permanent a)) 0 a

druid :: Temporary Int -> ClassInfo (Temporary Int)
druid a = ClassInfo (4 * (weird a)) (4 * (weird a)) (3 * a `div` 4) (strongSave a) (weakSave a) (strongSave a) (T (0 :| []) (permanent a)) 0 a

fighter :: Temporary Int -> ClassInfo (Temporary Int)
fighter a = ClassInfo (5 * (weird a)) (5 * (weird a)) a (strongSave a) (weakSave a) (weakSave a) (T (0 :| []) (permanent a)) 0 0

soldier :: Temporary Int -> ClassInfo (Temporary Int)
soldier a = ClassInfo (7 * (weird a)) (7 * (weird a)) a (strongSave a) (weakSave a) (strongSave a) (T (0 :| []) (permanent a)) 0 0

solarion :: Temporary Int -> ClassInfo (Temporary Int)
solarion a = ClassInfo (7 * (weird a)) (7 * (weird a)) a (strongSave a) (weakSave a) (strongSave a) (T (0 :| []) (permanent a)) 0 0

monk :: Temporary Int -> ClassInfo (Temporary Int)
monk a = ClassInfo (4 * (weird a)) (4 * (weird a)) a (strongSave a) (strongSave a) (strongSave a) (T (0 :| []) (permanent a)) 0 0

paladin :: Temporary Int -> ClassInfo (Temporary Int)
paladin a = ClassInfo (5 * (weird a)) (5 * (weird a)) a (strongSave a) (weakSave a) (strongSave a) (T (0 :| []) (permanent a)) 0 a

ranger :: Temporary Int -> ClassInfo (Temporary Int)
ranger a = ClassInfo (5 * (weird a)) (5 * (weird a)) a (strongSave a) (strongSave a) (weakSave a) (T (0 :| []) (permanent a)) 0 a

rogue :: Temporary Int -> ClassInfo (Temporary Int)
rogue a = ClassInfo (4 * (weird a)) (4 * (weird a)) (3 * a `div` 4) (weakSave a) (strongSave a) (weakSave a) (T (0 :| []) (permanent a)) 0 0

operative :: Temporary Int -> ClassInfo (Temporary Int)
operative a = ClassInfo (6 * (weird a)) (6 * (weird a)) (3 * a `div` 4) (weakSave a) (strongSave a) (strongSave a) (T (0 :| []) (permanent a)) 0 0

envoy :: Temporary Int -> ClassInfo (Temporary Int)
envoy a = ClassInfo (6 * (weird a)) (6 * (weird a)) (3 * a `div` 4) (weakSave a) (strongSave a) (strongSave a) (T (0 :| []) (permanent a)) 0 0

mechanic :: Temporary Int -> ClassInfo (Temporary Int)
mechanic a = ClassInfo (6 * (weird a)) (6 * (weird a)) (3 * a `div` 4) (strongSave a) (strongSave a) (weakSave a) (T (0 :| []) (permanent a)) 0 0

sorceror :: Temporary Int -> ClassInfo (Temporary Int)
sorceror a = ClassInfo (3 * (weird a)) (3 * (weird a)) (a `div` 2) (weakSave a) (weakSave a) (strongSave a) (T (0 :| []) (permanent a)) a 0

vindicator :: Temporary Int -> ClassInfo (Temporary Int)
vindicator a = ClassInfo (weird a) (weird a) a (strongSave a) (weakSave a) (strongSave a) (T (0 :| []) (permanent a)) 0 ((3 * a) `div` 4)

dragondisciple :: Temporary Int -> ClassInfo (Temporary Int)
dragondisciple a = ClassInfo (weird a) (weird a) ((3 * a) `div` 4) (strongSave a) (weakSave a) (strongSave a) (T (0 :| []) (permanent a)) ((3 * a) `div` 4) 0

arcanearcher :: Temporary Int -> ClassInfo (Temporary Int)
arcanearcher a = ClassInfo (weird a) (weird a) a (strongSave a) (strongSave a) (weakSave a) (T (0 :| []) (permanent a)) ((3 * a) `div` 4) 0

eledritchknight :: Temporary Int -> ClassInfo (Temporary Int)
eledritchknight a = ClassInfo (weird a) (weird a) a (strongSave a) (weakSave a) (weakSave a) (T (0 :| []) (permanent a)) ((9 * a) `div` 10) 0

wizard :: Temporary Int -> ClassInfo (Temporary Int)
wizard a = ClassInfo (3 * (weird a)) (3 * (weird a)) (a `div` 2) (weakSave a) (weakSave a) (strongSave a) (T (0 :| []) (permanent a)) a 0

updateClassInfo :: ClassInfo (Temporary Int) -> ClassInfo (Temporary Int)
updateClassInfo cinfo
    | temporary (view (#level) cinfo) <= 0 = changelevel 0 $ revert <$> cinfo
    | otherwise = set (#level) (view (#level) cinfo) $ fmap go cinfo
        where
            ratio = (temporary (view (#level) cinfo)) * 100 `div` (view (#level . #permanent) cinfo) 
            go :: Temporary Int -> Temporary Int
            go (T (t :| ts) p) = T ((t - (ratio * t `div` 100)) :| (t : ts)) p

revertClassInfo :: ClassInfo (Temporary Int) -> ClassInfo (Temporary Int)
revertClassInfo cinfo = updateClassInfo $ over (#level) revertToZero cinfo

getLevel :: (Temporary Int) -> Int
getLevel tint = (view (#permanent) tint) - (temporary tint)