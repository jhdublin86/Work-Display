{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module DND.STATUSEFFECTS.Effects
where
    
import Control.Lens (set, view, over)
import Data.Generics.Labels ()
import GHC.Generics (Generic)
import Control.Lens.Indexed
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Traversable

import DND.TemporaryValue (Temporary (T))
{-
data Temporal a =  NA | Absent | LimitedOff Int a | LimitedOn Int a | Temporary Int a | Permanent a deriving (Generic, Show, Eq, Ord)
-}
data Temporal a = NA {duration :: (Maybe Int), character :: String, modifyer :: a} | Absent {duration :: (Maybe Int), character :: String, modifyer :: a} | Off {duration :: (Maybe Int), character :: String, modifyer :: a} | 
    On {duration :: (Maybe Int), character :: String, modifyer :: a} | Present {duration :: (Maybe Int), character :: String, modifyer :: a} | 
    Permanent {duration :: (Maybe Int), character :: String, modifyer :: a} deriving (Generic, Show, Eq, Ord)

instance Functor Temporal where
    fmap f (NA a b c) = NA a b (f c)
    fmap f (Absent a b c) = Absent a b (f c)
    fmap f (Off a b c) = Off a b (f c)
    fmap f (On a b c) = On a b (f c)
    fmap f (Present a b c) = Present a b (f c)
    fmap f (Permanent a b c) = Permanent a b (f c)

instance Applicative Temporal where
    pure a = Present Nothing [] a
    (<*>) a b = (\x -> x ((viewtempmodifyier) b)) <$> a

instance Monad Temporal where
    return = pure
    (>>=) a f = over (#modifyer) (view (#modifyer) . f) $ a

instance Foldable Temporal where
    foldMap f a = f (viewtempmodifyier a)

instance Traversable Temporal where
    traverse f a
        | isNA a = (NA (temporalduration a) (view (#character) a)) <$> (f (viewtempmodifyier a))
        | isAbsent a = (Absent (temporalduration a) (view (#character) a)) <$> (f (viewtempmodifyier a))
        | isPermanent a = (Permanent (temporalduration a) (view (#character) a)) <$> (f (viewtempmodifyier a))
        | isOff a = (Off (temporalduration a) (view (#character) a)) <$> (f (viewtempmodifyier a))
        | isOn a = (On (temporalduration a) (view (#character) a)) <$> (f (viewtempmodifyier a))
        | otherwise = (Present (temporalduration a) (view (#character) a)) <$> (f (viewtempmodifyier a))

selecttemporal2 :: Maybe Int -> Maybe Int -> String -> Maybe Int -> (Temporal (Maybe Int))
selecttemporal2 (Just 0) a b c = NA a b c
selecttemporal2 (Just 1) a b c = Absent a b c
selecttemporal2 (Just 2) a b c = Off a b c
selecttemporal2 (Just 3) a b c = On a b c
selecttemporal2 (Just 4) a b c = Present a b c
selecttemporal2 (Just 5) a b c = Permanent a b c
selecttemporal2 _ a b c = Present a b c

isNA :: Temporal a -> Bool
isNA (NA _ _ _) = True
isNA _ = False

isPermanent :: Temporal a -> Bool
isPermanent (Permanent _ _ _) = True
isPermanent _ = False

isActive :: Temporal a -> Bool
isActive (On _ _ _) = True
isActive (Present _ _ _) = True
isActive (Permanent _ _ _) = True
isActive _ = False

isOff :: Temporal a -> Bool
isOff (Off _ _ _) = True
isOff _ = False

isOn :: Temporal a -> Bool
isOn (On _ _ _) = True
isOn _ = False

isAbsent :: Temporal a -> Bool
isAbsent (Absent _ _ _) = True
isAbsent _ = False

temporalduration :: Temporal a -> Maybe Int
temporalduration a = view (#duration) a

viewtempmodifyier :: Temporal a -> a
viewtempmodifyier a  = view (#modifyer) a

setmodifyer :: a -> Temporal (b) -> Temporal (a)
setmodifyer b a = set (#modifyer) b a

setduration :: Maybe Int -> Temporal (a) -> Temporal (a)
setduration b a = set (#duration) b a

replacetemporal :: Temporal a -> Temporal a -> Temporal a
replacetemporal a b = setmodifyer ((viewtempmodifyier) a) b

temporaldurationreduction :: Temporal (Maybe Int) -> Temporal (Maybe Int)
temporaldurationreduction t2
    | temporalduration t2 == (Just 0) = t2
    | isOff t2 = t2
    | (isPermanent t2) && (temporalduration t2 == (Just 1)) = Present (Just 0) (view (#character) t2) (viewtempmodifyier t2)
    | (isNA t2) && (temporalduration t2 == (Just 1)) = Absent (Just 0) (view (#character) t2) (viewtempmodifyier t2)
    | otherwise = ((\x -> x - 1) <$>) -$- t2

removeemptydurations :: Temporary (Temporal (Maybe Int)) -> Temporary (Temporal (Maybe Int))
removeemptydurations (T (t :| ts) p) = go2 $ T (t :| (filter (\x -> temporalduration x /= (Just 0)) ts)) p
    where
        go2 :: Temporary (Temporal (Maybe Int)) -> Temporary (Temporal (Maybe Int))
        go2 (T (t1 :| []) p1)
            | temporalduration t1 == (Just 0) = T (p1 :| []) p1
            | otherwise = T (t1 :| []) p1
        go2 (T (t1 :| t1s) p1)
            | temporalduration t1 == (Just 0) = T ( head t1s :| (tail t1s)) p1
            | otherwise = (T (t1 :| t1s) p1)

removerepeatdurations :: Temporary (Temporal (Maybe Int)) -> Temporary (Temporal (Maybe Int))
removerepeatdurations (T (t :| []) p) = (T (t :| []) p)
removerepeatdurations (T (t :| ts) p)
        | head ts == t = T (head ts :| tail ts) p
        | otherwise = T (t :| ts) p

settemporal :: Temporal (Maybe Int) -> Temporary (Temporal (Maybe Int)) -> Temporary (Temporal (Maybe Int))
settemporal t' (T (_ :| ts) p) = T (t' :| ts) p

addtemporal :: Temporal (Maybe Int) -> Temporary (Temporal (Maybe Int)) -> Temporary (Temporal (Maybe Int))
addtemporal t' (T (t :| ts) p)
    | t' == t = T (t :| ts) p
    | isOff t && isOn t' = T (t' :| ts) p
    | isOn t && isOff t' = T ((off t) :| ts) p
    | isOn t = (T (t' :| ((off t): ts)) p)
    | otherwise = (T (t' :| (t: ts)) p)

removepermanent :: Temporary (Temporal (Maybe Int)) -> Temporary (Temporal (Maybe Int))
removepermanent (T (t :| []) p)
    | isPermanent t = T (p :| []) p
    | otherwise = T (t :| []) p
removepermanent (T (t :| ts) p)
    | isPermanent t = T ((head ts) :| (tail ts)) p
    | otherwise = T (t :| ts) p

removena :: Temporary (Temporal (Maybe Int)) -> Temporary (Temporal (Maybe Int))
removena (T (t :| []) p)
    | isNA t = T (p :| []) p
    | otherwise = T (t :| []) p
removena (T (t :| ts) p)
    | isNA t = T ((head ts) :| (tail ts)) p
    | otherwise = T (t :| ts) p

removehead :: Temporary (Temporal (Maybe Int)) -> Temporary (Temporal (Maybe Int))
removehead (T (_ :| []) p) = T (p :| []) p
removehead (T (_ :| ts) p) = T ((head ts) :| (tail ts)) p

stoneskindamage :: Maybe Int -> Temporal (Maybe Int) -> (Temporal (Maybe Int), Maybe Int)
stoneskindamage a t
    | (not . isActive) t = (t, a)
    | (viewtempmodifyier t) < avoided = (Absent Nothing [] Nothing, taken & avoided & (negate <$> (viewtempmodifyier t)))
    | otherwise = ((\x -> x & (negate <$> avoided)) <$> t, taken)
        where
            (taken, avoided) = go a
            go :: Maybe Int -> (Maybe Int, Maybe Int)
            go Nothing = (Nothing, Nothing)
            go (Just i)
                | i < 1 = (Just i, Nothing)
                | i < 16 = (Just 1, Just (i - 1))
                | otherwise = (Just (i - 15), Just 15)

forcefielddamage2 :: Maybe Int -> Temporal (Maybe Int) -> (Temporal (Maybe Int), Maybe Int)
forcefielddamage2 a t
    | (not . isActive) t = (t, a)
    | otherwise = ((\x -> x & (negate <$> avoided)) <$> t, taken)
        where
            (taken, avoided) = go a
            go :: Maybe Int -> (Maybe Int, Maybe Int)
            go Nothing = (Nothing, Nothing)
            go (Just i)
                | i < 1 = (Just i, Nothing)
                | otherwise = (Just ((max 0 $ i - (maybeint2int $ viewtempmodifyier t))), Just (min (maybeint2int $ viewtempmodifyier t) i))

maybeint2int :: Maybe Int -> Int
maybeint2int mi = case mi of
  Nothing -> 0
  Just y -> y

(-$-) :: (Maybe Int -> Maybe Int) -> Temporal a -> Temporal a
(-$-) f (NA a b c) = NA (f a) b c
(-$-) f (Absent a b c) = Absent (f a) b c
(-$-) f (Off a b c) = Off (f a) b c
(-$-) f (On a b c) = On (f a) b c
(-$-) f (Present a b c) = Present (f a) b c
(-$-) f (Permanent a b c) = Permanent (f a) b c

data StatusEffect =  Undead | Invisible | ImmunetoCriticals | MirrorImage | TrueStrike | GreaterEvasion | Evasion | Sing | 
                    Rage | BearStrength | StoneSkin | Repelled | Blind | Stealth | Bleeding  | Prone | UncannyDodge | Disarmed |
                    Getem | ExpertAttack | ImprovedGetem | ForceField | Mobility | ArcanStrike | Fatigued | Exhausted |
                    Shaken | Fear | Sickened | Nauseated | ImmunetoPoisen | ImmunetoMagic | ImmunetoDiseae | Sleep | KeenSenses | ImmunetoSneakAttack | 
                    SpellFocus | Burning | Paralyzed | Confused | Petrified  | Laughter | ImprovedInvisibility | ProtectiveWard | Dazed  | Despair |
                    AuraofDespair | SpellTurning | Dominated | BeastShape | ElementalBodyIII | ElementalBodyIV | SmiteEvil | DivineBond | WeaponFinesse |
                    FormoftheDragon | Stigmata | ChannelSmite | MagicFang | ChannelShield | Aid | Bless | DeadlyAim | WereRaptor | Shield | Blur | Heroism |
                    FireShield | CatsGrace | Held | BullsEndurance | GrowClaws | Entangled | BarkSkin | Polymorth | Summoned | CombatShot | SeeInvisibility | DivineFavour |
                    Poisened | ImmuneMindInfluencingEffects | Stunned | StunningFist | Ki | Staggered | DarkHeal | Grappled | Cover | Charged | Incorporal | Pounce | Turned | Commanded |
                    ChannelResistance | ShieldofFaith | ProtectionFrom | DefensiveStance | ImprovedGrapple | ImprovedDisarm | ImprovedTrip | MageArmour | Regeneration | Haste | Slow |
                    Cleave | ImprovedDoubleAttack | TripleAttack | PhotonMode | GravitonMode | HIPS | TrickAttack | FlatFooted | DarkMatter deriving (Generic, Show, Eq, Ord)

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
                   , repelled :: a
                   , blind :: a
                   , stealth :: a
                   , bleeding :: a
                   , prone :: a
                   , uncannydodge :: a
                   , disarmed :: a
                   , getem :: a
                   , expertattack :: a
                   , improvedgetem :: a
                   , forcefield :: a
                   , mobility :: a
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
                   , keensenses :: a
                   , immunetosneakattack :: a
                   , spellfocus :: a
                   , burning :: a
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
                   , deadlyaim :: a
                   , wereraptor :: a
                   , shield :: a
                   , blur :: a
                   , heroism :: a
                   , fireshield :: a
                   , catsgrace :: a
                   , held :: a
                   , bullsendurance :: a
                   , growclaws :: a
                   , entangled :: a
                   , barkskin :: a
                   , polymorth :: a
                   , summoned :: a
                   , combatShot :: a
                   , seeinvisibility :: a
                   , divinefavour :: a
                   , poisened :: a 
                   , immuneMindInfluencingEffects :: a
                   , stunned :: a
                   , stunningfist :: a
                   , ki :: a
                   , staggered :: a
                   , darkheal :: a
                   , grappled :: a
                   , cover :: a
                   , charged :: a
                   , incorporal :: a
                   , pounce :: a
                   , turned :: a
                   , commanded :: a
                   , channelresistance :: a
                   , shieldoffaith :: a
                   , protectionfrom :: a
                   , defensivestance :: a
                   , improvedgrapple :: a
                   , improveddisarm :: a
                   , improvedtrip :: a
                   , magearmour :: a
                   , regeneration :: a
                   , haste :: a
                   , slow :: a
                   , cleave :: a
                   , improveddoubleattack :: a
                   , tripleattack :: a
                   , photonmode :: a
                   , gravitonmode :: a
                   , hips :: a
                   , trickattack :: a
                   , flatfooted :: a
                   , darkmatter :: a}  deriving (Generic, Show, Eq, Ord)

instance Functor Effects where
    fmap = fmapDefault

instance Applicative Effects where
    pure a = E a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
        a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
        a a a a a a a
    (<*>) (E f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20 f21 f22 f23 f24 f25 f26 f27 f28 
        f29 f30 f31 f32 f33 f34 f35 f36 f37 f38 f39 f40 f41 f42 f43 f44 f45 f46 f47 f48 f49 f50 f51 f52 f53 f54 f55
        f56 f57 f58 f59 f60 f61 f62 f63 f64 f65 f66 f67 f68 f69 f70 f71 f72 f73 f74 f75 f76 f77 f78 f79 f80 f81 f82
        f83 f84 f85 f86 f87 f88 f89 f90 f91 f92 f93 f94 f95 f96 f97 f98 f99 f100 f101 f102 f103 f104 f105 f106 f107 
        f108 f109 f110 f111 f112 f113)
        (E a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 
        a30 a31 a32 a33 a34 a35 a36 a37 a38 a39 a40 a41 a42 a43 a44 a45 a46 a47 a48 a49 a50 a51 a52 a53 a54 a55 a56
        a57 a58 a59 a60 a61 a62 a63 a64 a65 a66 a67 a68 a69 a70 a71 a72 a73 a74 a75 a76 a77 a78 a79 a80 a81 a82 a83
        a84 a85 a86 a87 a88 a89 a90 a91 a92 a93 a94 a95 a96 a97 a98 a99 a100 a101 a102 a103 a104 a105 a106 a107 a108
        a109 a110 a111 a112 a113) = 
        E (f1 a1) (f2 a2) (f3 a3) (f4 a4) (f5 a5) (f6 a6) (f7 a7) (f8 a8) (f9 a9) (f10 a10) (f11 a11) (f12 a12) 
        (f13 a13) (f14 a14) (f15 a15) (f16 a16) (f17 a17) (f18 a18) (f19 a19) (f20 a20) (f21 a21) (f22 a22) (f23 a23) 
        (f24 a24) (f25 a25) (f26 a26) (f27 a27) (f28 a28) (f29 a29) (f30 a30) (f31 a31) (f32 a32) (f33 a33) (f34 a34)
        (f35 a35) (f36 a36) (f37 a37) (f38 a38) (f39 a39) (f40 a40) (f41 a41) (f42 a42) (f43 a43) (f44 a44) (f45 a45)
        (f46 a46) (f47 a47) (f48 a48) (f49 a49) (f50 a50) (f51 a51) (f52 a52) (f53 a53) (f54 a54) (f55 a55) (f56 a56)
        (f57 a57) (f58 a58) (f59 a59) (f60 a60) (f61 a61) (f62 a62) (f63 a63) (f64 a64) (f65 a65) (f66 a66) (f67 a67)
        (f68 a68) (f69 a69) (f70 a70) (f71 a71) (f72 a72) (f73 a73) (f74 a74) (f75 a75) (f76 a76) (f77 a77) (f78 a78)
        (f79 a79) (f80 a80) (f81 a81) (f82 a82) (f83 a83) (f84 a84) (f85 a85) (f86 a86) (f87 a87) (f88 a88) (f89 a89)
        (f90 a90) (f91 a91) (f92 a92) (f93 a93) (f94 a94) (f95 a95) (f96 a96) (f97 a97) (f98 a98) (f99 a99) (f100 a100)
        (f101 a101) (f102 a102) (f103 a103) (f104 a104) (f105 a105) (f106 a106) (f107 a107) (f108 a108) (f109 a109)
        (f110 a110) (f111 a111) (f112 a112) (f113 a113)

instance FunctorWithIndex StatusEffect Effects

instance Foldable Effects where
    foldMap = foldMapDefault
    length _ = 113

instance FoldableWithIndex StatusEffect Effects

instance Traversable Effects where
    traverse = itraverse . const

instance TraversableWithIndex StatusEffect Effects where
    itraverse ƒ (E a b c d e f g h i j k l  m  n  o  p  q  r  s  t  u  v  w  x  y  z 
        a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1 r1 s1 t1 u1 v1 w1 x1 y1 z1
        a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 m2 n2 o2 p2 q2 r2 s2 t2 u2 v2 w2 x2 y2 z2
        a3 b3 c3 d3 e3 f3 g3 h3 i3 j3 k3 l3 m3 n3 o3 p3 q3 r3 s3 t3 u3 v3 w3 x3 y3 z3
        a4 b4 c4 d4 e4 f4 g4 h4 i4) =
        E <$> ƒ Undead a <*> ƒ Invisible b <*> ƒ ImmunetoCriticals c <*> ƒ MirrorImage d <*> ƒ TrueStrike e 
        <*> ƒ GreaterEvasion f <*> ƒ Evasion g <*> ƒ Sing h <*> ƒ Rage i <*> ƒ BearStrength j <*> ƒ StoneSkin k
        <*> ƒ Repelled l <*> ƒ Blind m <*> ƒ Stealth n <*> ƒ Bleeding o <*> ƒ Prone p <*> ƒ UncannyDodge q
        <*> ƒ Disarmed r <*> ƒ Getem s <*> ƒ ExpertAttack t <*> ƒ ImprovedGetem u <*> ƒ ForceField v 
        <*> ƒ Mobility w <*> ƒ ArcanStrike x <*> ƒ Fatigued y <*> ƒ Exhausted z <*> ƒ Shaken a1 <*> ƒ Fear b1 
        <*> ƒ Sickened c1 <*> ƒ Nauseated d1 <*> ƒ ImmunetoPoisen e1 <*> ƒ ImmunetoMagic f1 <*> ƒ ImmunetoDiseae g1 <*> ƒ Sleep h1
        <*> ƒ KeenSenses i1 <*> ƒ ImmunetoSneakAttack j1 <*> ƒ SpellFocus k1 <*> ƒ Burning l1 <*> ƒ Paralyzed m1
        <*> ƒ Confused n1 <*> ƒ Petrified o1 <*> ƒ Laughter p1 <*> ƒ ImprovedInvisibility q1 <*> ƒ ProtectiveWard r1
        <*> ƒ Dazed s1 <*> ƒ Despair t1 <*> ƒ AuraofDespair u1 <*> ƒ SpellTurning v1 <*> ƒ Dominated w1 <*> ƒ BeastShape x1
        <*> ƒ ElementalBodyIII y1 <*> ƒ ElementalBodyIV z1 <*> ƒ SmiteEvil a2 <*> ƒ DivineBond b2 <*> ƒ WeaponFinesse c2
        <*> ƒ FormoftheDragon d2 <*> ƒ Stigmata e2 <*> ƒ ChannelSmite f2 <*> ƒ MagicFang g2 <*> ƒ ChannelShield h2 <*> ƒ Aid i2 
        <*> ƒ Bless j2 <*> ƒ DeadlyAim k2 <*> ƒ WereRaptor l2 <*> ƒ Shield m2 <*> ƒ Blur n2 <*> ƒ Heroism o2 <*> ƒ FireShield p2
        <*> ƒ CatsGrace q2 <*> ƒ Held r2 <*> ƒ BullsEndurance s2 <*> ƒ GrowClaws t2 <*> ƒ Entangled u2 <*> ƒ BarkSkin v2
        <*> ƒ Polymorth w2 <*> ƒ Summoned x2 <*> ƒ CombatShot y2 <*> ƒ SeeInvisibility z2 <*> ƒ DivineFavour a3 <*> ƒ Poisened b3
        <*> ƒ ImmuneMindInfluencingEffects c3 <*> ƒ Stunned d3 <*> ƒ StunningFist e3 <*> ƒ Ki f3 <*> ƒ Staggered g3 <*> ƒ DarkHeal h3
        <*> ƒ Grappled i3 <*> ƒ Cover j3 <*> ƒ Charged k3 <*> ƒ Incorporal l3 <*> ƒ Pounce m3 <*> ƒ Turned n3 <*> ƒ Commanded o3
        <*> ƒ ChannelResistance p3 <*> ƒ ShieldofFaith q3 <*> ƒ ProtectionFrom r3 <*> ƒ DefensiveStance s3 <*> ƒ ImprovedGrapple t3
        <*> ƒ ImprovedDisarm u3 <*> ƒ ImprovedTrip v3 <*> ƒ MageArmour w3 <*> ƒ Regeneration x3 <*> ƒ Haste y3 <*> ƒ Slow z3
        <*> ƒ Cleave a4 <*> ƒ ImprovedDoubleAttack b4 <*> ƒ TripleAttack c4 <*> ƒ PhotonMode d4 <*> ƒ GravitonMode e4 <*> ƒ HIPS f4
        <*> ƒ TrickAttack g4 <*> ƒ FlatFooted h4  <*> ƒ DarkMatter i4 

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
effix Repelled = repelled
effix Blind = blind
effix Stealth = stealth
effix Bleeding = bleeding
effix Prone = prone
effix UncannyDodge = uncannydodge
effix Disarmed = disarmed
effix Getem = getem
effix ExpertAttack = expertattack
effix ImprovedGetem = improvedgetem
effix ForceField = forcefield
effix Mobility = mobility
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
effix KeenSenses = keensenses
effix ImmunetoSneakAttack = immunetosneakattack
effix SpellFocus = spellfocus
effix Burning = burning
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
effix WereRaptor = wereraptor
effix Shield = shield
effix Blur = blur
effix Heroism = heroism
effix FireShield = fireshield
effix CatsGrace = catsgrace
effix Held = held
effix BullsEndurance = bullsendurance
effix GrowClaws = growclaws
effix Entangled = entangled
effix BarkSkin = barkskin
effix Polymorth = polymorth
effix Summoned = summoned
effix CombatShot = combatShot
effix SeeInvisibility = seeinvisibility
effix DivineFavour = divinefavour
effix Poisened = poisened
effix ImmuneMindInfluencingEffects = immuneMindInfluencingEffects
effix Stunned = stunned
effix StunningFist = stunningfist
effix Ki = ki
effix Staggered = staggered
effix DarkHeal = darkheal
effix Grappled = grappled
effix Cover = cover
effix Charged = charged
effix Incorporal = incorporal
effix Pounce = pounce
effix Turned = turned
effix Commanded = commanded
effix ChannelResistance = channelresistance
effix ShieldofFaith = shieldoffaith
effix ProtectionFrom = protectionfrom
effix DefensiveStance = defensivestance
effix ImprovedGrapple = improvedgrapple
effix ImprovedDisarm = improveddisarm
effix ImprovedTrip = improvedtrip
effix MageArmour = magearmour
effix Regeneration = regeneration
effix Haste = haste
effix Cleave = cleave
effix ImprovedDoubleAttack = improveddoubleattack
effix TripleAttack = tripleattack
effix PhotonMode = photonmode
effix GravitonMode = gravitonmode
effix HIPS = hips
effix Slow = slow
effix TrickAttack = trickattack
effix FlatFooted = flatfooted
effix DarkMatter = darkmatter

noEffects :: Effects (Temporary (Temporal (Maybe Int)))
noEffects = set (#commanded) (pure $ NA Nothing [] Nothing :: Temporary (Temporal (Maybe Int))) $ 
    set (#turned) (pure $ NA Nothing [] Nothing :: Temporary (Temporal (Maybe Int)))
    (pure $ pure (Absent Nothing [] Nothing) :: Effects (Temporary (Temporal (Maybe Int))))

isundead :: Effects (Temporary (Temporal (Maybe Int)))
isundead = set (#undead) (T ((Present Nothing [] Nothing) :| []) (Present Nothing [] Nothing)) noEffects

on :: Temporal a -> Temporal a
on (Off d c b) = On d c b
on a = a

off :: Temporal a -> Temporal a
off (On d c b) = Off d c b
off a = a

(&) :: Maybe Int -> Maybe Int -> Maybe Int
(&) damage1 damage2
    | damage1 == Nothing = damage2
    | damage2 == Nothing = damage1
    | otherwise = (+) <$> damage1 <*> damage2