{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module DND.ATTACKS.Weapon
where

import Data.Generics.Labels ()
import GHC.Generics (Generic)
import Control.Lens.Indexed
import Data.Traversable
import Control.Lens (set)

import DND.ATTACKS.DamageReduction (Material (Reduc), DamageType (Piercing, Slashing, Bludgeoning, Normal, Reduced))

import DND.DAMAGEDICE.Elemental (Elemental (Fire, Earth, Ice, Wind, Lightning, Acid, Negative, Positive))
import DND.DAMAGEDICE.DamageDice (DamageDice, defaultDamageDice)

data Enchantment = Fiery | Holy | Icy | Acidic | Thundering | Unholy | Earthen | Windy | Anarchic | Axiomatic 
                    | Distance | Burst | LongSite | Blasting deriving (Generic, Show, Ord, Eq)

iselementalenchantment :: Enchantment -> Bool
iselementalenchantment Fiery = True
iselementalenchantment Holy = True
iselementalenchantment Icy = True
iselementalenchantment Acidic = True
iselementalenchantment Thundering = True
iselementalenchantment Unholy = True
iselementalenchantment Earthen = True
iselementalenchantment Windy = True
iselementalenchantment _ = False

enchantment2elemental :: Enchantment -> Maybe Elemental
enchantment2elemental Fiery = Just Fire
enchantment2elemental Holy = Nothing
enchantment2elemental Icy =  Just Ice
enchantment2elemental Acidic = Just Acid
enchantment2elemental Thundering =  Just Lightning
enchantment2elemental Unholy = Nothing
enchantment2elemental Earthen =  Just Earth
enchantment2elemental Windy = Just Wind
enchantment2elemental _ = Nothing

elemental2enchantment :: Elemental -> Enchantment
elemental2enchantment Fire = Fiery 
elemental2enchantment Positive = Holy
elemental2enchantment Ice = Icy
elemental2enchantment Acid = Acidic 
elemental2enchantment Lightning = Thundering 
elemental2enchantment Negative = Unholy
elemental2enchantment Earth = Earthen
elemental2enchantment Wind = Windy

data Critical = C { threshold :: Int
                  , multiplier :: Int
                  , critbonus :: Int} deriving (Show, Eq, Generic, Ord)

data Range = Melee | Missile deriving (Show, Eq, Generic, Ord)

data WeaponType = Grenade | SmallArm | LongArm | BasicMelee | AdvancedMelee | HeavyWeapon | Sniper deriving (Generic, Show, Ord, Eq)

rangefinder :: WeaponType -> Range
rangefinder Grenade = Missile
rangefinder SmallArm = Missile
rangefinder LongArm = Missile
rangefinder BasicMelee = Melee
rangefinder AdvancedMelee = Melee
rangefinder HeavyWeapon = Missile
rangefinder Sniper = Missile

data Weapon = Weapon { weapondamagetype :: DamageType
                     , weapontype :: WeaponType
                     , weaponddice :: DamageDice [Int]
                     , weaponrange :: Int
                     , enchantments :: [Enchantment]
                     , name :: String } deriving (Generic, Show, Ord, Eq)

defaultWeapon :: Weapon 
defaultWeapon = Weapon Reduced BasicMelee defaultDamageDice 5 [] "Weapon"

data FeatProfile = FP {weaponfeat :: WeaponFeat, weaponfocus :: Bool} deriving (Generic, Show, Ord, Eq)

data WeaponFeat = Untrained | Proficient | Specialization deriving (Generic, Show, Ord, Eq)

data Weapons a = W { grenade :: a
                   , smallArm :: a
                   , longArm :: a
                   , basicMelee :: a
                   , advancedMelee :: a
                   , heavyWeapon :: a
                   , sniper :: a} deriving (Generic, Show, Ord, Eq)

wix :: WeaponType -> (Weapons a -> a)
wix Grenade = grenade
wix SmallArm = smallArm
wix LongArm = longArm
wix BasicMelee = basicMelee
wix AdvancedMelee = advancedMelee
wix HeavyWeapon = heavyWeapon
wix Sniper = sniper

{-data AttackFeats = AF { powerattack :: Bool
                      , combatexpertise :: Bool
                      , multishot :: MultiAttackFeat
                      , twoweaponfighting :: MultiAttackFeat
                      , twoweaponslice :: Bool
                      , arcanestrike :: Bool} deriving (Generic, Show, Ord, Eq)
-}
allproficient :: Weapons FeatProfile
allproficient = pure (FP Proficient False)

noproficient :: Weapons FeatProfile
noproficient = pure (FP Untrained False)

instance Functor Weapons where
    fmap = fmapDefault

instance Applicative Weapons where
    pure a = W a a a a a a a
    (<*>) (W f1 f2 f3 f4 f5 f6 f7) (W a1 a2 a3 a4 a5 a6 a7) = W (f1 a1) (f2 a2) (f3 a3) (f4 a4) (f5 a5) (f6 a6) (f7 a7)
  
instance FunctorWithIndex WeaponType Weapons

instance Foldable Weapons where
    foldMap = foldMapDefault
    length _ = 7

instance FoldableWithIndex WeaponType Weapons

instance Traversable Weapons where
    traverse = itraverse . const

instance TraversableWithIndex WeaponType Weapons where
    itraverse ƒ (W a b c d e f g) =
        W <$> (ƒ Grenade a) <*> (ƒ SmallArm b) <*> (ƒ LongArm c) <*> (ƒ BasicMelee d) <*> (ƒ AdvancedMelee e) <*> (ƒ HeavyWeapon f) <*> (ƒ Sniper g)