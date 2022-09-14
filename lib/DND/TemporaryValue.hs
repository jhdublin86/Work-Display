{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

module DND.TemporaryValue
where

import Control.Applicative
import Data.Generics.Labels ()
import GHC.Generics (Generic)
import Data.List (sort, delete)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE


{- Co-Monad -}
data Temporary a = T { temporaries :: NonEmpty a
                     , permanent :: a }
                   deriving (Generic, Show, Eq, Ord)

instance Functor Temporary where
    fmap f (T as a)  = T (f <$> as) (f a)

instance Applicative Temporary where
    pure a = T (a :| []) a
    (<*>) (T fas f) (T as a) = T (NE.zipWith (\x -> x) fas as) (f a)

instance Num a => Num (Temporary a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger 

instance Enum a => Enum (Temporary a) where
    toEnum = pure . toEnum
    fromEnum = fromEnum . NE.head . temporaries

instance Real a => Real (Temporary a) where
    toRational = toRational . NE.head . temporaries

instance Integral a => Integral (Temporary a) where
    toInteger = toInteger . NE.head . temporaries
    quot = liftA2 quot
    rem =  liftA2 rem
    quotRem a b = (quot a b, rem a b)

instance Num a => Semigroup (Temporary a) where
    (<>) a b = a + b

instance Num a => Monoid (Temporary a) where
    mempty = 0
{--damage :: Int -> Temporary Int -> Temporary Int
damage a t
    | (view (#temporary) t) + a < 0 = set (#temporary) 0 t
    | otherwise = over (#temporary) (+ a) t--}

addtemporary :: Ord a => a -> Temporary a -> Temporary a
addtemporary a' (T (a :| []) pa)
    | a' <= pa = T (a :| []) pa
    | otherwise = T ((max a a') :| [min a a']) pa
addtemporary a' (T (a :| as) pa)
    | a' >= a = T (a' :| (a : as)) pa
    | otherwise = T (a :| (go a' as)) pa
        where
            go b bs = (reverse . sort) (b : bs)

settemporary :: Eq a => a -> Temporary a -> Temporary a
settemporary a' (T (a :| as) pa) = T (a' :| (a : as)) pa

undosettemporary :: Eq a => a -> Temporary a -> Temporary a
undosettemporary a' (T (a :| []) pa)
    | a' == a = T (pa :| []) pa
    | otherwise = T (a :| []) pa
undosettemporary a' (T (a :| as) pa)
    | a' == a = T (head as :| (tail as)) pa
    | otherwise = T (a :| (delete a' as)) pa

removetemporary :: (Ord a) => a -> Temporary a -> Temporary a
removetemporary a' (T (a :| []) pa)
    | a' == a = T (pa :| []) pa
    | otherwise = (T (a :| []) pa)
removetemporary a' (T (a :| as) pa)
    | a' == a = T (((head . reverse . sort) as) :| ((tail . reverse . sort) as)) pa
    | a' > pa = T (a :| (go as)) pa
    | otherwise = (T (a :| as) pa)
        where
            go bs = takeWhile ((\x -> x /= a')) bs ++ (drop 1 (dropWhile (\x -> x /= a') bs))

add2temporary :: a -> Temporary (a) -> Temporary (a)
add2temporary a' (T (a :| as) pa) = (T (a' :| (a : as)) pa)

removefromtemporary :: Eq a => a -> Temporary (a) -> Temporary (a)
removefromtemporary a' (T (a :| []) pa)
    | a' == a = T ( pa :| []) pa
    | otherwise = T (a :| []) pa
removefromtemporary a' (T (a :| as) pa)
    | a' == a = T (head as :| tail as) pa
    | otherwise = T (a :| (go as)) pa
            where
                go bs = takeWhile ((\x -> x /= a')) bs ++ (drop 1 (dropWhile (\x -> x /= a') bs))

revert :: Temporary a -> Temporary a
revert (T _ p) = T (p :| []) p

revertToZero :: Num a => Temporary a -> Temporary a
revertToZero (T _ p) = T (0 :| []) p

temporary :: Temporary a -> a
temporary = NE.head . temporaries

modifytemporary :: (a -> a) -> Temporary a -> Temporary a
modifytemporary f (T (t :| ts) p) = T (f t :| ts) p

temporarize :: Num a => a -> Temporary a
temporarize a = T (0 :| []) a

{-- instance Real a => Real (Temporary a) where
    toRational (T t _) = toRational t

instance Integral a => Integral (Temporary a) where
    quotRem (T t p) (T t' _) = (T q p, T r p)
        where 
            (q, r) = quotRem t t'
    toInteger (T t _) = toInteger t --}

--}