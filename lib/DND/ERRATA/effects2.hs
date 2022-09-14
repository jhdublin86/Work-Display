{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module DND.Effects
where
    
import Control.Lens (Lens', set)
import Data.Generics.Labels ()
import GHC.Generics (Generic)
import Control.Lens.Indexed
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Traversable
import Data.Maybe (fromJust)

import DND.TemporaryValue (Temporary (T))
{-
data Temporal a =  NA | Absent | LimitedOff Int a | LimitedOn Int a | Temporary Int a | Permanent a deriving (Generic, Show, Eq, Ord)
-}
data Temporal2 a = NA2 {na2duration :: (Maybe Int), na2modifyer :: a} | Absent2 {absent2 :: (Maybe Int), absent2modifyer :: a} | Off2 {off2duration :: (Maybe Int), off2modifyer :: a} | 
    On2 {on2duration :: (Maybe Int), on2modifyer :: a} | Present2 {present2duration :: (Maybe Int), present2modifyer :: a} | 
    Permanent2 {permanent2duration :: (Maybe Int), permanent2modifyer :: a} deriving (Generic, Show, Eq, Ord)

instance Functor Temporal2 where
    fmap f (NA2 a b) = NA2 a (f b)
    fmap f (Absent2 a b) = Absent2 a (f b)
    fmap f (Off2 a b) = Off2 a (f b)
    fmap f (On2 a b) = On2 a (f b)
    fmap f (Present2 a b) = Present2 a (f b)
    fmap f (Permanent2 a b) = Permanent2 a (f b)

instance Applicative Temporal2 where
    pure a = Present2 Nothing a
    (<*>) a b = (\x -> x ((fromJust . t2modifyer) b)) <$> a

instance Monad Temporal2 where
    return = pure
    (>>=) a f
        | isNA2 a = NA2 $ t2duration a
        | isAbsent2 a = Absent2 $ t2duration a
        | isPermanent2 a = Permanent2 (t2duration a) ((permanent2modifyer . f) (fromJust (t2modifyer a)))
        | isOff2 a = Off2 (t2duration a) ((off2modifyer . f) (fromJust (t2modifyer a)))
        | isOn2 a = On2 (t2duration a) ((on2modifyer . f) (fromJust (t2modifyer a)))
        | otherwise = Present2 (t2duration a) ((present2modifyer . f) (fromJust (t2modifyer a)))

instance Foldable Temporal2 where
    foldMap f a
        | isNA2 a = mempty
        | isAbsent2 a = mempty
        | otherwise = f (fromJust (t2modifyer a))

instance Traversable Temporal2 where
    traverse f a
        | isNA2 a = pure (NA2 $ t2duration a)
        | isAbsent2 a = pure (Absent2 $ t2duration a)
        | isPermanent2 a = (Permanent2 (t2duration a)) <$> (f (fromJust $ t2modifyer a))
        | isOff2 a = (Off2 (t2duration a)) <$> (f (fromJust $ t2modifyer a))
        | isOn2 a = (On2 (t2duration a)) <$> (f (fromJust $ t2modifyer a))
        | otherwise = (Present2 (t2duration a)) <$> (f (fromJust $ t2modifyer a))

selecttemporal2 :: Maybe Int -> Maybe Int -> Maybe Int -> (Temporal2 (Maybe Int))
selecttemporal2 (Just 0) a _ = NA2 a
selecttemporal2 (Just 1) a _ = Absent2 a
selecttemporal2 (Just 2) a b = Off2 a b
selecttemporal2 (Just 3) a b = On2 a b
selecttemporal2 (Just 4) a b = Present2 a b
selecttemporal2 (Just 5) a b = Permanent2 a b

isNA2 :: Temporal2 a -> Bool
isNA2 (NA2 _) = True
isNA2 _ = False

isPermanent2 :: Temporal2 a -> Bool
isPermanent2 (Permanent2 _ _) = True
isPermanent2 _ = False

isActive :: Temporal2 a -> Bool
isActive (On2 _ _) = True
isActive (Present2 _ _) = True
isActive (Permanent2 _ _) = True
isActive _ = False

isOff2 :: Temporal2 a -> Bool
isOff2 (Off2 _ _) = True
isOff2 _ = False

isOn2 :: Temporal2 a -> Bool
isOn2 (On2 _ _) = True
isOn2 _ = False

isAbsent2 :: Temporal2 a -> Bool
isAbsent2 (Absent2 _) = True
isAbsent2 _ = False

t2duration :: Temporal2 a -> Maybe Int
t2duration (NA2 a) = a
t2duration (Absent2 a) = a
t2duration (Off2 a _) = a
t2duration (On2 a _) = a
t2duration (Present2 a _) = a
t2duration (Permanent2 a _) = a

t2modifyer :: Temporal2 a -> Maybe a
t2modifyer (Off2 _ a) = Just a
t2modifyer (On2 _ a) = Just a
t2modifyer (Present2 _ a) = Just a
t2modifyer (Permanent2 _ a) = Just a
t2modifyer _ = Nothing

newt2modifyer :: a -> Temporal2 (b) -> Temporal2 (a)
newt2modifyer b (Off2 a _) = (Off2 a b)
newt2modifyer b (On2 a _) = (On2 a b)
newt2modifyer b (Present2 a _) = (Present2 a b)
newt2modifyer b (Permanent2 a _) = (Permanent2 a b)
newt2modifyer _ (NA2 a) = NA2 a
newt2modifyer _ (Absent2 a) = Absent2 a

t2durationreduction :: Temporal2 (Maybe Int) -> Temporal2 (Maybe Int)
t2durationreduction t2
    | t2duration t2 == (Just 0) = t2
    | isOff2 t2 = t2
    | otherwise = ((\x -> x - 1) <$>) -$- t2

removeemptydurations :: Temporary (Temporal2 (Maybe Int)) -> Temporary (Temporal2 (Maybe Int))
removeemptydurations (T (t :| ts) p) = go2 $ T (t :| (filter (\x -> t2duration x /= (Just 0)) ts)) p
    where
        go2 :: Temporary (Temporal2 (Maybe Int)) -> Temporary (Temporal2 (Maybe Int))
        go2 (T (t1 :| []) p1)
            | t2duration t1 == (Just 0) = T (p1 :| []) p1
            | otherwise = T (t1 :| []) p1
        go2 (T (t1 :| t1s) p1)
            | t2duration t1 == (Just 0) = T ( head t1s :| (tail t1s)) p1
            | otherwise = (T (t1 :| t1s) p1)

removerepeatdurations :: Temporary (Temporal2 (Maybe Int)) -> Temporary (Temporal2 (Maybe Int))
removerepeatdurations (T (t :| []) p) = (T (t :| []) p)
removerepeatdurations (T (t :| ts) p)
        | head ts == t = T (head ts :| tail ts) p
        | otherwise = T (t :| ts) p

settemporal2 :: Temporal2 (Maybe Int) -> Temporary (Temporal2 (Maybe Int)) -> Temporary (Temporal2 (Maybe Int))
settemporal2 t' (T (_ :| ts) p) = T (t' :| ts) p

addtemporal2 :: Temporal2 (Maybe Int) -> Temporary (Temporal2 (Maybe Int)) -> Temporary (Temporal2 (Maybe Int))
addtemporal2 t' (T (t :| ts) p)
    | t' == t = T (t :| ts) p
    | isOff2 t && isOn2 t' = T ((on t) :| ts) p
    | isOn2 t && isOff2 t' = T ((off t) :| ts) p
    | isOn2 t = (T (t' :| ((off t): ts)) p)
    | otherwise = (T (t' :| (t: ts)) p)

removepermanent :: Temporary (Temporal2 (Maybe Int)) -> Temporary (Temporal2 (Maybe Int))
removepermanent (T (t :| []) p)
    | isPermanent2 t = T (p :| []) p
    | otherwise = T (t :| []) p
removepermanent (T (t :| ts) p)
    | isPermanent2 t = T ((head ts) :| (tail ts)) p
    | otherwise = T (t :| ts) p

removena :: Temporary (Temporal2 (Maybe Int)) -> Temporary (Temporal2 (Maybe Int))
removena (T (t :| []) p)
    | isNA2 t = T (p :| []) p
    | otherwise = T (t :| []) p
removena (T (t :| ts) p)
    | isNA2 t = T ((head ts) :| (tail ts)) p
    | otherwise = T (t :| ts) p

removehead :: Temporary (Temporal2 (Maybe Int)) -> Temporary (Temporal2 (Maybe Int))
removehead (T (_ :| []) p) = T (p :| []) p
removehead (T (_ :| ts) p) = T ((head ts) :| (tail ts)) p
   

stoneskindamage2 :: Maybe Int -> Temporal2 (Maybe Int) -> (Temporal2 (Maybe Int), Maybe Int)
stoneskindamage2 a t
    | (not . isActive) t = (t, a)
    | t2modifyer t == Nothing = ((Absent2 Nothing), a)
    | (fromJust $ t2modifyer t) < avoided = (Absent2 Nothing, taken & avoided & (negate <$> (fromJust $ t2modifyer t)))
    | otherwise = ((\x -> x & (negate <$> avoided)) <$> t, taken)
        where
            (taken, avoided) = go a
            go :: Maybe Int -> (Maybe Int, Maybe Int)
            go Nothing = (Nothing, Nothing)
            go (Just i)
                | i < 1 = (Just i, Nothing)
                | i < 16 = (Just 1, Just (i - 1))
                | otherwise = (Just (i - 15), Just 15)

(-$-) :: (Maybe Int -> Maybe Int) -> Temporal2 a -> Temporal2 a
(-$-) f (NA2 a) = NA2 $ f a
(-$-) f (Absent2 a) = Absent2 $ f a
(-$-) f (Off2 a b) = Off2 (f a) b
(-$-) f (On2 a b) = On2 (f a) b
(-$-) f (Present2 a b) = Present2 (f a) b
(-$-) f (Permanent2 a b) = Permanent2 (f a) b

{-
instance Functor Temporal where
    fmap _ Absent = Absent
    fmap _ NA = NA
    fmap f (LimitedOff a b) = LimitedOff a $ f b
    fmap f (LimitedOn a b) = LimitedOn a $ f b
    fmap f (Temporary a b) = Temporary a $ f b
    fmap f (Permanent a) = Permanent $ f a
-}

data StatusEffect =  Undead | Invisible | ImmunetoCriticals | MirrorImage | TrueStrike | GreaterEvasion | Evasion | Sing | 
                    Rage | BearStrength | StoneSkin | Confusion | Blind | Stealth | Bleeding  | Prone | UncannyDodge | Disarmed |
                    PowerAttack | CombatExpertise | MultiShot | TwoWeaponFighting | TwoWeaponSlice | ArcanStrike | Fatigued | Exhausted |
                    Shaken | Fear | Sickened | Nauseated | ImmunetoPoisen | ImmunetoMagic | ImmunetoDiseae | Sleep | PowerCritical | ImmunetoSneakAttack | 
                    TouchofDestiny | Fated | Paralyzed | Confused | Petrified  | Laughter | ImprovedInvisibility | ProtectiveWard | Dazed  | Despair |
                    AuraofDespair | SpellTurning | Dominated | BeastShape | ElementalBodyIII | ElementalBodyIV | SmiteEvil | DivineBond | WeaponFinesse |
                    FormoftheDragon | Stigmata | ChannelSmite | MagicFang | ChannelShield | Aid | Bless | DeadlyAim deriving (Generic, Show, Eq, Ord)

data Effects a = E { undead :: a
                   , invisible :: a
                   , immunetocriticals :: a
                   , mirrorImage :: a
                   , trueStrike :: a
                   , greaterEvasion :: a
                   , evasion :: a
                   , sing :: a
                   , rage :: a
                   , bearStrength :: a
                   , stoneSkin :: a
                   , confusion :: a
                   , blind :: a
                   , stealth :: a
                   , bleeding :: a
                   , prone :: a
                   , uncannydodge :: a
                   , disarmed :: a
                   , powerattack :: a
                   , combatexpertise :: a
                   , multishot :: a
                   , twoweaponfighting :: a
                   , twoweaponslice :: a
                   , arcanestrike :: a
                   , fatigued :: a
                   , exhausted :: a
                   , shaken :: a
                   , fear :: a
                   , sickened :: a
                   , nauseated :: a
                   , immunetopoisen :: a
                   , immunetomagic :: a
                   , immunetodisease :: a
                   , sleep :: a
                   , powercritical :: a
                   , immunetosneakattack :: a
                   , touchofdestiny :: a
                   , fated :: a
                   , paralyzed :: a
                   , confused :: a
                   , petrified :: a
                   , laughter :: a
                   , improvedinvisibility :: a
                   , protectiveward :: a
                   , dazed :: a
                   , despair :: a
                   , auraofdespair :: a
                   , spellturning :: a
                   , dominated :: a
                   , beastshape :: a
                   , elementalbodyiii :: a
                   , elementalbodyiv :: a
                   , smiteevil :: a
                   , divinebond :: a
                   , weaponfinesse :: a
                   , formofthedragon :: a
                   , stigmata :: a
                   , channelsmite :: a
                   , magicfang :: a
                   , channelshield :: a
                   , aid :: a
                   , bless :: a
                   , deadlyaim :: a} deriving (Generic, Show, Eq, Ord)

instance Functor Effects where
    fmap = fmapDefault

instance Applicative Effects where
    pure a = E a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
        a a a a a a a a a a a a
    (<*>) (E f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20 f21 f22 f23 f24 f25 f26 f27 f28 
        f29 f30 f31 f32 f33 f34 f35 f36 f37 f38 f39 f40 f41 f42 f43 f44 f45 f46 f47 f48 f49 f50 f51 f52 f53 f54 f55
        f56 f57 f58 f59 f60 f61 f62 f63)
        (E a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 
        a30 a31 a32 a33 a34 a35 a36 a37 a38 a39 a40 a41 a42 a43 a44 a45 a46 a47 a48 a49 a50 a51 a52 a53 a54 a55 a56
        a57 a58 a59 a60 a61 a62 a63) = 
        E (f1 a1) (f2 a2) (f3 a3) (f4 a4) (f5 a5) (f6 a6) (f7 a7) (f8 a8) (f9 a9) (f10 a10) (f11 a11) (f12 a12) 
        (f13 a13) (f14 a14) (f15 a15) (f16 a16) (f17 a17) (f18 a18) (f19 a19) (f20 a20) (f21 a21) (f22 a22) (f23 a23) 
        (f24 a24) (f25 a25) (f26 a26) (f27 a27) (f28 a28) (f29 a29) (f30 a30) (f31 a31) (f32 a32) (f33 a33) (f34 a34)
        (f35 a35) (f36 a36) (f37 a37) (f38 a38) (f39 a39) (f40 a40) (f41 a41) (f42 a42) (f43 a43) (f44 a44) (f45 a45)
        (f46 a46) (f47 a47) (f48 a48) (f49 a49) (f50 a50) (f51 a51) (f52 a52) (f53 a53) (f54 a54) (f55 a55) (f56 a56)
        (f57 a57) (f58 a58) (f59 a59) (f60 a60) (f61 a61) (f62 a62) (f63 a63)

instance FunctorWithIndex StatusEffect Effects

instance Foldable Effects where
    foldMap = foldMapDefault
    length _ = 62

instance FoldableWithIndex StatusEffect Effects

instance Traversable Effects where
    traverse = itraverse . const

instance TraversableWithIndex StatusEffect Effects where
    itraverse ƒ (E a b c d e f g h i j k l  m  n  o  p  q  r  s  t  u  v  w  x  y  z 
        a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1 r1 s1 t1 u1 v1 w1 x1 y1 z1
        a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2) =
        E <$> ƒ Undead a <*> ƒ Invisible b <*> ƒ ImmunetoCriticals c <*> ƒ MirrorImage d <*> ƒ TrueStrike e 
        <*> ƒ GreaterEvasion f <*> ƒ Evasion g <*> ƒ Sing h <*> ƒ Rage i <*> ƒ BearStrength j <*> ƒ StoneSkin k
        <*> ƒ Confusion l <*> ƒ Blind m <*> ƒ Stealth n <*> ƒ Bleeding o <*> ƒ Prone p <*> ƒ UncannyDodge q
        <*> ƒ Disarmed r <*> ƒ PowerAttack s <*> ƒ CombatExpertise t <*> ƒ MultiShot u <*> ƒ TwoWeaponFighting v 
        <*> ƒ TwoWeaponSlice w <*> ƒ ArcanStrike x <*> ƒ Fatigued y <*> ƒ Exhausted z <*> ƒ Shaken a1 <*> ƒ Fear b1 
        <*> ƒ Sickened c1 <*> ƒ Nauseated d1 <*> ƒ ImmunetoPoisen e1 <*> ƒ ImmunetoMagic f1 <*> ƒ ImmunetoDiseae g1 <*> ƒ Sleep h1
        <*> ƒ PowerCritical i1 <*> ƒ ImmunetoSneakAttack j1 <*> ƒ TouchofDestiny k1 <*> ƒ Fated l1 <*> ƒ Paralyzed m1
        <*> ƒ Confused n1 <*> ƒ Petrified o1 <*> ƒ Laughter p1 <*> ƒ ImprovedInvisibility q1 <*> ƒ ProtectiveWard r1
        <*> ƒ Dazed s1 <*> ƒ Despair t1 <*> ƒ AuraofDespair u1 <*> ƒ SpellTurning v1 <*> ƒ Dominated w1 <*> ƒ BeastShape x1
        <*> ƒ ElementalBodyIII y1 <*> ƒ ElementalBodyIV z1 <*> ƒ SmiteEvil a2 <*> ƒ DivineBond b2 <*> ƒ WeaponFinesse c2
        <*> ƒ FormoftheDragon d2 <*> ƒ Stigmata e2 <*> ƒ ChannelSmite f2 <*> ƒ MagicFang g2 <*> ƒ ChannelShield h2 <*> ƒ Aid i2 
        <*> ƒ Bless j2 <*> ƒ DeadlyAim k2 

effix :: StatusEffect -> (Effects a -> a)
effix Undead = undead
effix Invisible = invisible
effix ImmunetoCriticals = immunetocriticals
effix MirrorImage = mirrorImage
effix TrueStrike = trueStrike
effix GreaterEvasion = greaterEvasion
effix Evasion = evasion
effix Sing = sing
effix Rage = rage
effix BearStrength = bearStrength
effix StoneSkin = stoneSkin
effix Confusion = confusion
effix Blind = blind
effix Stealth = stealth
effix Bleeding = bleeding
effix Prone = prone
effix UncannyDodge = uncannydodge
effix Disarmed = disarmed
effix PowerAttack = powerattack
effix CombatExpertise = combatexpertise
effix MultiShot = multishot
effix TwoWeaponFighting = twoweaponfighting
effix TwoWeaponSlice = twoweaponslice
effix ArcanStrike = arcanestrike
effix Fatigued = fatigued
effix Exhausted = exhausted
effix Shaken = shaken
effix Fear = fear
effix Sickened = sickened
effix Nauseated = nauseated
effix ImmunetoDiseae = immunetodisease
effix ImmunetoMagic = immunetomagic
effix ImmunetoPoisen = immunetopoisen
effix Sleep = sleep
effix PowerCritical = powercritical
effix ImmunetoSneakAttack = immunetosneakattack
effix TouchofDestiny = touchofdestiny
effix Fated = fated
effix Paralyzed = paralyzed
effix Confused = confused
effix Petrified = petrified
effix Laughter = laughter
effix ImprovedInvisibility = improvedinvisibility
effix ProtectiveWard = protectiveward
effix Dazed = dazed
effix Despair = despair
effix AuraofDespair = auraofdespair
effix SpellTurning = spellturning
effix Dominated = dominated
effix BeastShape = beastshape
effix ElementalBodyIII = elementalbodyiii
effix ElementalBodyIV = elementalbodyiv
effix SmiteEvil = smiteevil
effix DivineBond = divinebond
effix WeaponFinesse = weaponfinesse
effix FormoftheDragon = formofthedragon
effix Stigmata = stigmata
effix ChannelSmite = channelsmite
effix MagicFang = magicfang
effix ChannelShield = channelshield
effix Aid = aid
effix Bless = bless
effix DeadlyAim = deadlyaim

{-
limitfinder :: Temporal (Maybe a) -> Maybe a
limitfinder (Permanent a) = a
limitfinder (Temporary _ b) = b
limitfinder _ = Nothing

limitgiver :: Maybe a -> Temporal b -> Temporal (Maybe a)
limitgiver Nothing NA = NA
limitgiver Nothing _ = Absent
limitgiver a b = (\_ -> a) <$> b

duration :: Temporal a -> Int
duration (Temporary d _) = d
duration (LimitedOn d _) = d
duration (LimitedOff d _) = d
duration _ = (-1)

makeduration :: Int -> Temporal a -> Temporal a
makeduration a (Temporary _ b) = Temporary a b
makeduration a (LimitedOn _ b) = LimitedOn a b
makeduration a (LimitedOff _ b) = LimitedOff a b
makeduration _ a = a
-}
noEffects :: Effects (Temporary (Temporal2 (Maybe Int)))
noEffects = pure $ pure (Absent2 Nothing)

isundead :: Effects (Temporary (Temporal2 (Maybe Int)))
isundead = set (#undead) (T ((Present2 Nothing Nothing) :| []) (Present2 Nothing Nothing)) noEffects
{-
isLimitedOff :: Temporal a -> Bool
isLimitedOff (LimitedOff _ _) = True
isLimitedOff _ = False

isLimitedOn :: Temporal a -> Bool
isLimitedOn (LimitedOn _ _) = True
isLimitedOn _ = False

isTemporary :: Temporal a -> Bool
isTemporary (Temporary _ _) = True
isTemporary _ = False

isPermanent :: Temporal a -> Bool
isPermanent (Permanent _) = True
isPermanent _ = False

isPresent :: Temporal a -> Bool
isPresent a = isTemporary a || isPermanent a || isLimitedOn a
-}
on :: Temporal2 a -> Temporal2 a
on (Off2 d b) = On2 d b
on a = a

off :: Temporal2 a -> Temporal2 a
off (On2 d b) = Off2 d b
off a = a

(&) :: Maybe Int -> Maybe Int -> Maybe Int
(&) damage1 damage2
    | damage1 == Nothing = damage2
    | damage2 == Nothing = damage1
    | otherwise = (+) <$> damage1 <*> damage2
{-
stoneskindamage :: Maybe Int -> Temporal (Maybe Int) -> (Temporal (Maybe Int), Maybe Int)
stoneskindamage a t
    | (limitfinder t) < avoided = (Absent, taken & avoided & (negate <$> (limitfinder t)))
    | otherwise = ((\x -> x & (negate <$> avoided)) <$> t, taken)
        where
            (taken, avoided) = go a
            go :: Maybe Int -> (Maybe Int, Maybe Int)
            go Nothing = (Nothing, Nothing)
            go (Just i)
                | i < 1 = (Just i, Nothing)
                | i < 16 = (Just 1, Just (i - 1))
                | otherwise = (Just (i - 15), Just 15)

durationreduction :: Effects (Temporal (Maybe Int)) -> Effects (Temporal (Maybe Int))
durationreduction e = go <$> e
            where
                go :: Temporal a -> Temporal a
                go t
                    | ((duration t) == 0) = t
                    | isTemporary t || isLimitedOn t = makeduration ((duration t) - 1) t
                    | otherwise = t
-}