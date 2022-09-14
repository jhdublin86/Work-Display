{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module DND.Parser
where

-- base
import Control.Lens (Lens', set, over, view)
import Control.Applicative (Alternative ((<|>), empty), liftA2, many, some)
import Control.Monad (guard)
import Control.Monad.Fail (fail, MonadFail)
import Data.Char (isSpace)
import Data.Foldable (foldl', traverse_)
import Data.Functor (void)
import Prelude hiding (fail, takeWhile)
import Data.List.NonEmpty (NonEmpty ((:|)))
import GHC.Float (int2Float, rationalToFloat)
import System.IO (Handle, IOMode (WriteMode, ReadMode), hPrint, hClose, openFile, hShow, hGetContents)
import qualified Data.Map as MAP


-- mtl
import Control.Monad.State.Class (MonadState (get, put, state))

-- transformers
import Control.Monad.Trans.Class (MonadTrans (lift))

import DND.ATTACKS.Attack (AttackInfo (AInfo), AttackRoll (AttackRoll), Attack (Attack))
import DND.ATTACKS.DamageReduction (Material (..), DamageReduction (DR), DamageType (..))
import DND.ATTACKS.Smite (FavouredEnemy (FE), Alignment (Al), Benificence (..), Lawfulness (..), Race (..), Smite (Smite))
import DND.ATTACKS.Weapon (WeaponType (..), Weapon (Weapon), Enchantment (..), Weapons (W), WeaponFeat (..),
    FeatProfile (FP), Range (..), Critical (C))

import DND.CHARACTER.AbilityScores (AbilityScores (AS), Ability (..))
import DND.CHARACTER.Bonuses (BonusTypes (BT), Bonuses (B))
import DND.CHARACTER.Character (Character (Character), Location, defaultPicture, temple)
import DND.CHARACTER.Class (Skills (Skills), ClassInfo (ClassInfo), Classes (Classes))
import DND.CHARACTER.ClassFeatures (ClassFeatures (CF), SpellBook (SB), Feats (Feats), SpellLevel (SL), SolarionFeatures (SolFeat), EnvoyFeatures (EnvFeat))
import DND.CHARACTER.Status (Vision (Vision), VisionCategory (NormalVision, Dark, Low), Status (Status), Team (Ally, Enemy, Enemy2, Bystander), SpellTemplate, Size (..), Health (..), emptystatus, channelenergy, channelenergy', turnundead, EquipmentArea (Feet, Forearms, Amulet, LeftRing, RightRing, Helmet, Cloak), MagicItem (MagicItem), Equipment (Equipment), magicitemtest, emptystatus)

import DND.DAMAGEDICE.DamageDice (SaveProfile (..), DamageDice (D))
import DND.DAMAGEDICE.Elemental (Profile (..), ElementalResistance (ER), Elemental (..))

import DND.DEFENSE.Armour (Armour (A))
import qualified DND.DEFENSE.Armour as NArmour (NonArmour (NA))
import DND.DEFENSE.Defense (Defense (..), Target (Target))

import DND.SPELLS.Spell (SkillFeatProfile (..), SpellFeat (SF), SkillFeatProfile (..), SpellSchool (..), Spell (..), Spell (..), SpellTarget (..), SpellArea (..), SpellInfo (SpellInfo), Summon (Summon), SpellRoll (SpellRoll), testspell)
import DND.SPELLS.SpellFunctions (darkheal, firebolt, sleep1, causewounds, healpoisen, divinefavour, bless, stabilize, noncombatspell, charmanimal, solarfurnace,
    balefullpolymorth, flamestrike, barkskin, entangle, bullsendurancemass, bearsstrength, bullsendurance, catsgrace, burninghandswand, blackhole, glowofhealth,
    masscurewounds, curewounds, doom, causefear, hold, confusion, hideouslaughter, dominateperson, fireshield, heroism, tnaglefootbag, flashpowder,
    shieldoffaithpotion, scorchingray, lightningbolt, mirrorimage, phantasmalkiller, restoration, greaterrestoration, shield, shieldoffaith, supernova,
    truestrike, fireball, fireballwand, shockinggrasp, burninghands, blur, invisibility, protectionfrom, seeinvisibility, magearmour, magicmissile, grenade, 
    divinegrenade, entangletrap, dragongland, commandundead, mindthrust, holographicclone, improvedhurry, dispellmagic, removeaffliction, slowpoison, keensenses,
    wallosteel, repulsionspell, destruction, coneofcoldwand, improvedinvisibility, haste)
import DND.SPELLS.Summons (woodengaze, summonskeleton, summonnaturesally1, summonnaturesally2, summonnaturesally3, summonnaturesally4, summonnaturesally5, summonnaturesally6, 
    summonvulture, nixiecharmperson, breathweaponbeetle, trample, breathweaponlarge, breathweaponwyrmling, vineentangle, zombiefy, auraoffear, breathweaponmulti)

import DND.STATDAMAGE.StatDamage (StatDamageType (..), StatDamage (SD))

import DND.STATUSEFFECTS.Effect (Effect (Effect))
import DND.STATUSEFFECTS.Effects (Temporal (..), Effects (E), StatusEffect (..))

import DND.TemporaryValue (Temporary (T))
import DND.Turn
import DND.Maps

data Identity a = Identity {runId :: a} deriving (Show)

instance Functor Identity where
    fmap f a = (Identity . f . runId) a

instance Applicative Identity where
    pure a = Identity a
    (<*>) a b = Identity . runId a $ runId b

instance Monad Identity where
    return = pure
    (>>=) a f = (f . runId) a

instance Foldable Identity where
    foldMap f (Identity a) = f a

instance Traversable Identity where
    traverse f (Identity a) = Identity <$> f a


newtype StateT s m a = StateT (s -> m (a, s))

instance Functor m => Functor (StateT s m) where
    fmap f (StateT m) = StateT $ fmap (\(a, s) -> (f a, s)) . m

instance Monad m => Applicative (StateT s m) where
    pure a = StateT $ \s -> pure (a, s)

    StateT mf <*> StateT ma = StateT $ \s -> do
        (f, s') <- mf s
        (a, s'') <- ma s'
        pure (f a, s'')

instance (Alternative m, Monad m) => Alternative (StateT s m) where
    empty = StateT $ \_ -> empty
    StateT ma <|> StateT mb = StateT $ \s -> ma s <|> mb s

instance Monad m => Monad (StateT s m) where
    StateT ma >>= f = StateT $ \s -> do
        (a, s') <- ma s
        let StateT mb = f a
        mb s'

instance MonadFail m => MonadFail (StateT s m) where
    fail message = StateT $ \s -> (\a -> (a, s)) <$> fail message

instance Monad m => MonadState s (StateT s m) where
    state f = StateT $ pure . f

instance MonadTrans (StateT s) where
    lift m = StateT $ \s -> fmap (\a -> (a, s)) m


newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance Functor m => Functor (MaybeT m) where
    fmap f (MaybeT a) = MaybeT $ fmap f <$> a

instance Applicative m => Applicative (MaybeT m) where
    pure = MaybeT . pure . pure
    MaybeT f <*> MaybeT a = MaybeT $ liftA2 (<*>) f a

instance MonadTrans MaybeT where
    lift = MaybeT . fmap pure

instance Foldable f => Foldable (MaybeT f) where
    foldMap f (MaybeT a) = foldMap (foldMap f) a

instance Traversable f => Traversable (MaybeT f) where
    traverse f (MaybeT a) = MaybeT <$> traverse (traverse f) a

instance Monad m => Monad (MaybeT m) where
    return = pure
    MaybeT m >>= f = MaybeT $ m >>= maybe (pure Nothing) (runMaybeT . f)

instance Applicative m => Alternative (MaybeT m) where
    empty = MaybeT $ pure empty
    MaybeT a <|> MaybeT b = MaybeT $ liftA2 (<|>) a b

instance Monad m => MonadFail (MaybeT m) where
    fail _ = MaybeT $ pure empty




newtype IdentityT m a = IdentityT {runIdT :: m (Identity a)}

instance Functor m => Functor (IdentityT m) where
    fmap f (IdentityT a) = IdentityT $ (f <$>) <$> a

instance Applicative m => Applicative (IdentityT m) where
    pure = IdentityT . pure . Identity
    (<*>) (IdentityT f) (IdentityT a) = IdentityT $ liftA2 (<*>) f a



{-instance Monad m => Monad (IdentityT m) where
    return = pure
    ia >>= f = IdentityT $ runIdT ia >>= (runIdT . f . runId)-}

instance Monad m => Monad (IdentityT m) where
    return = pure
    ia >>= f = IdentityT $ do
        ix  <- runIdT ia
        (runIdT . f . runId) ix

instance MonadTrans IdentityT where
    lift = IdentityT . fmap Identity

instance (Foldable f) => Foldable (IdentityT f) where
    foldMap f (IdentityT a) = foldMap (foldMap f) a

instance (Traversable f) => Traversable (IdentityT f) where
    traverse f (IdentityT a) = IdentityT <$> (traverse (traverse f) a)





newtype ParserT m a = ParserT (StateT String (MaybeT m) a)
  deriving (Functor, Applicative, Monad, Alternative)

parseT :: Functor m => ParserT m a -> String -> m (Maybe (a, String))
parseT (ParserT (StateT f)) = runMaybeT . f

type Parser = ParserT Identity

parse :: Parser a -> String -> Maybe (a, String)
parse parser = runId . parseT parser

instance MonadTrans ParserT where
    lift = ParserT . lift . lift



next :: Monad m => ParserT m Char
next = ParserT $ do
    c : cs <- get
    put cs
    pure c
{-
    s <- get
    case s of
        [] -> empty
        c : cs -> do
            put cs
            pure c
-}

satisfy :: Monad m => (Char -> Bool) -> ParserT m Char
satisfy f = do
    c <- next
    guard $ f c
    pure c

takeWhileparser :: Monad m => (Char -> Bool) -> ParserT m [Char]
takeWhileparser = many . satisfy

takeWhile1 :: Monad m => (Char -> Bool) -> ParserT m [Char]
takeWhile1 = some . satisfy

char :: Monad m => Char -> ParserT m ()
char = void . satisfy . (==)

string :: Monad m => [Char] -> ParserT m ()
string = traverse_ char

takestring :: Monad m => ParserT m [Char]
takestring = do
    a <- takeWhileparser (\x -> (fromEnum x /= 44) && (fromEnum x /= 125))
    return $ filter (\ x -> x /= '\\' && x /= '"') a

takestringlist :: Monad m => ParserT m a -> ParserT m [a]
takestringlist item = char '[' *> char ']' *> pure []
    <|> char '[' *> fmap pure item <* char ']'
    <|> char '[' *> liftA2 (:) item (many go) <* char ']'
    where
        go = char ',' *> item

skipSpace :: Monad m => ParserT m ()
skipSpace = void $ takeWhileparser isSpace

skipSpace1 :: Monad m => ParserT m ()
skipSpace1 = void $ takeWhile1 isSpace


integral :: Monad m => ParserT m Int
integral = fmap fromIntegral $
    char '-' *> fmap negate natural
    <|> natural

float :: Monad m => ParserT m Float
float = do
    head <- integral
    _ <- char '.'
    tail <- takeWhileparser (\x -> fromEnum x >= 48 && fromEnum x <= 57)
    maybetailnumerator <- parseT integral tail
    let tailsize = length tail
    return $ int2Float head + (rationalToFloat (toInteger $ tailnumerator maybetailnumerator) (toInteger $ gohelperfunction tailsize 1))
    where
        tailnumerator x = case x of
            Nothing -> 0
            Just y -> fst y
    
gohelperfunction :: Int -> Int -> Int
gohelperfunction int1 int2 = if int1 <= 0 then int2 else gohelperfunction (int1 - 1) (10 * int2)

integral2 :: Monad m => ParserT m Int
integral2 = do
    skipSpace
    integral

natural :: Monad m => ParserT m Word
natural = fromDigits <$> some digit

bool :: Monad m => ParserT m Bool
bool = string "false" *> pure False
    <|> string "true" *> pure True
    <|> string "False" *> pure False
    <|> string "True" *> pure True

list :: Monad m => ParserT m a -> ParserT m [a]
list item = char '[' *> skipSpace *> char ']' *> pure []
    <|> char '[' *> skipSpace *> fmap pure item <* skipSpace <* char ']'
    <|> char '[' *> skipSpace *> liftA2 (:) item (many go) <* skipSpace <* char ']'
  where
    go = skipSpace *> char ',' *> skipSpace *> item

nelist :: Monad m => ParserT m a -> ParserT m (NonEmpty a)
nelist item = do
    a <- item
    skipSpace
    string ":|"
    skipSpace
    b <- list item
    return (a :| b)

temporaryparse :: Monad m => ParserT m a -> ParserT m (Temporary a)
temporaryparse item = do
    char 'T'
    skipSpace
    string "{temporaries = "
    a <- nelist item
    string ", permanent = "
    b <-  item
    char '}'
    return (T a b)
    <|>
    parens (temporaryparse item)
    where
        parens p = char '(' *> p <* char ')'    

classinfo :: Monad m => ParserT m a -> ParserT m (ClassInfo a)
classinfo item = do
    string "ClassInfo {vp = "
    i <- item
    string ", hp = "
    a <- item
    string ", bab = "
    b <- item
    string ", fort = "
    c <- item
    string ", ref = "
    d <- item
    string ", wil = "
    e <- item
    string ", level = "
    f <- item
    string ", bsb = "
    g <- item
    string ", bhb = "
    h <- item
    char '}'
    return $ ClassInfo i a b c d e f g h
    <|>
    parens (classinfo item)
    where
        parens p = char '(' *> p <* char ')'

spellarea :: Monad m => ParserT m SpellArea
spellarea = string "Cone" *> pure Cone <|> string "Line" *> pure Line <|> string "Bursty" *> pure Bursty <|> do
    string "Sphere " 
    a1 <- integral
    return $ Sphere a1
    <|>
    parens spellarea
    where
        parens p = char '(' *> p <* char ')'


spelltarget :: Monad m => ParserT m SpellTarget
spelltarget = string "Caster" *> pure Caster <|> do
    string "Allies " 
    a1 <- integral
    return $ Allies a1 
    <|> do
    string "Enemies " 
    a1 <- integral
    return $ Enemies a1 
    <|> do
    string "SingleTarget " 
    a1 <- integral
    return $ SingleTarget a1 
    <|> do
    string "TargetinginRange " 
    a1 <- integral
    return $ SingleTarget a1 
    <|> do
    string "AlliesinRange " 
    a1 <- spellarea
    char ' '
    a2 <- integral
    return $ AlliesinRange a1 a2 
    <|> do
    string "EnemiesinRange " 
    a1 <- spellarea
    char ' '
    a2 <- integral
    return $ EnemiesinRange a1 a2 
    <|> do
    string "AllInRange " 
    a1 <- spellarea
    char ' '
    a2 <- integral
    return $ AllInRange a1 a2 
    <|> do
    string "MapArea " 
    a1 <- integral
    char ' '
    a2 <- integral
    return $ MapArea a1 a2
    <|>
    parens spelltarget
    where
        parens p = char '(' *> p <* char ')'

spellinfo :: Monad m => ParserT m SpellInfo
spellinfo = do
    string "SpellInfo {name = "
    a <- takestring
    string ", apcost = "
    i <- integral
    string ", school = "
    b <- spellschool
    string ", spellbonus = "
    c <- integral
    string ", sRbonus = "
    d <- integral
    string ", target = "
    e <- maybeparse target
    string ", spelltarget = "
    f <- spelltarget
    char '}'
    return $ SpellInfo a i b c d e f
    <|>
    parens spellinfo
    where
        parens p = char '(' *> p <* char ')'

summon :: Monad m => ParserT m Summon
summon = do
    string "Summon {creature = "
    a <- takestring
    string ", duration = "
    b <- integral
    string ", number = "
    c <- integral
    char '}'
    return $ Summon a b c
    <|>
    parens summon
    where
        parens p = char '(' *> p <* char ')'
                                
solfeatures :: Monad m => ParserT m SolarionFeatures
solfeatures = do
    string "SolFeat {stellarrush = "
    a <- bool
    string ", solaracceleration = "
    b <- bool
    string ", sidereal = "
    c <- list integral
    char '}'
    return $ SolFeat a b c
    <|>
    parens solfeatures
    where
        parens p = char '(' *> p <* char ')'
                                
envfeatures :: Monad m => ParserT m EnvoyFeatures
envfeatures = do
    string "EnvFeat {expertise = "
    a <- list integral
    string ", improvedgetem = "
    b <- bool
    char '}'
    return $ EnvFeat a b
    <|>
    parens envfeatures
    where
        parens p = char '(' *> p <* char ')'
                
spellroll :: Monad m => ParserT m a -> ParserT m (SpellRoll a)
spellroll item = do
    string "SpellRoll {ddroll = "
    a <- item
    string ", sdroll = "
    b <- item
    string ", efroll = "
    c <- item
    string ", srroll = "
    d <- item
    char '}'
    return $ SpellRoll a b c d
    <|>
    parens (spellroll item)
    where
        parens p = char '(' *> p <* char ')'

spell :: Monad m => ParserT m a -> ParserT m (Spell a)
spell item = do
    string "Spell {info = "
    a <- spellinfo
    string ", rolls = "
    b <-  spellroll item
    string ", blockaoo = "
    bool1 <-  tuple bool bool
    string ", mdamdice = "
    c <- maybeparse (damageDice item)
    string ", mstatdamage = "
    d <- maybeparse (statdamage item)
    string ", meffect = "
    e <- maybeparse (effect (temporal2 item))
    string ", msummon = "
    f <- maybeparse summon
    string ", mspecial = "
    g <- maybeparse takestring
    string ", isspell = "
    bool2 <-  bool
    char '}'
    return $ Spell a b bool1 c d e f g bool2
    <|>
    parens (spell item)
    where
        parens p = char '(' *> p <* char ')'

team :: Monad m => ParserT m Team
team = string "Ally" *> pure Ally <|> string "Enemy2" *> pure Enemy2 <|> string "Enemy" *> pure Enemy <|> string "Bystander" *> pure Bystander

elemental :: Monad m => ParserT m Elemental
elemental = string "Fire" *> pure Fire <|> string "Earth" *> pure Earth
    <|> string "Acid" *> pure Acid <|> string "Wind" *> pure Wind
    <|> string "Lightning" *> pure Lightning <|> string "Ice" *> pure Ice
    <|> string "Positive" *> pure Positive <|> string "Negative" *> pure Negative

spellschool :: Monad m => ParserT m  SpellSchool 
spellschool = do
    string "Abjuration" *> pure Abjuration <|> string "Conjuration" *> pure  Conjuration 
    <|> string "Divination" *> pure  Divination <|> string "Enchantment" *> pure  Enchantment 
    <|> string "Evocation" *> pure  Evocation <|> string "Illusion" *> pure  Illusion 
    <|> string "Necromancy" *> pure  Necromancy <|> string "Transmutation" *> pure  Transmutation 
    <|> string "Universal" *> pure  Universal

material :: Monad m => ParserT m Material
material =  string "Magical" *> pure Magical <|> string "GooD" *> pure GooD 
    <|> string "EviL" *> pure EviL <|> string "ChaoS" *> pure ChaoS 
    <|> string "LaW" *> pure LaW <|> string "Bludgeon" *> pure Bludgeon 
    <|> string "Slash" *> pure Slash <|> string "Pierce" *> pure Pierce 
    <|> string "Norm" *> pure Norm <|> string "Reduc" *> pure Reduc

weapontype :: Monad m => ParserT m WeaponType
weapontype = string "Grenade" *> pure Grenade <|> string "Sniper" *> pure Sniper <|> string "LongArm" *> pure 
    LongArm <|> string "SmallArm" *> pure SmallArm <|> string "BasicMelee" *> pure 
    BasicMelee <|> string "AdvancedMelee" *> pure AdvancedMelee <|> string "HeavyWeapon" *> pure 
    HeavyWeapon

enchantment :: Monad m => ParserT m Enchantment
enchantment = string "Fiery" *> pure Fiery <|> string "Holy" *> pure Holy <|> string "Icy" *> pure Icy 
    <|> string "Acidic" *> pure Acidic <|> string "Thundering" *> pure Thundering <|> string "Unholy" *> pure Unholy 
    <|> string "Earthen" *> pure Earthen <|> string "Windy" *> pure Windy <|> string "Anarchic" *> pure Anarchic 
    <|> string "Axiomatic" *> pure Axiomatic <|> string "Distance" *> pure Distance <|> string "Burst" *> pure Burst 
    <|> string "LongSite" *> pure LongSite <|> string "Blasting" *> pure Blasting

weaponfeat :: Monad m => ParserT m WeaponFeat
weaponfeat = string "Untrained" *> pure Untrained <|> string "Proficient" *> pure Proficient 
    <|> string "Specialization" *> pure Specialization

spelltemplate :: Monad m => ParserT m SpellTemplate
spelltemplate = string "Dark Heal" *> pure darkheal <|> string "Fireball (Wand)" *> pure fireballwand <|> string "Divine Grenade" *> pure divinegrenade 
    <|> string "Grenade" *> pure grenade <|> string "Channel 'Positive' Energy" *> pure channelenergy' <|> string "Channel Positive Energy" *> pure channelenergy
    <|> string "Channel Negative Energy" *> pure channelenergy<|> string "Turn Undead" *> pure turnundead <|> string "Phantasmal Killer" *> pure phantasmalkiller 
    <|> string "Restoration" *> pure restoration<|> string "Greater Restoration" *> pure greaterrestoration <|> do
        string "Shield of Faith Potion "
        a <- integral
        return (shieldoffaithpotion a) 
    <|> string "Shield of Faith" *> pure shieldoffaith
    <|> string "Shield" *> pure shield <|> string "True Strike" *> pure truestrike <|> string "Fireball" *> pure fireball <|> string "Super Nova" *> pure supernova
    <|> string "Burning Hands Wand" *> pure burninghandswand <|> string "Burning Hands" *> pure burninghands <|> string "Blur" *> pure blur 
    <|> string "Protection from Evil" *> pure (protectionfrom "Evil") <|> string "Protection from Good" *> pure (protectionfrom "Good") 
    <|> string "Protection from Lawful" *> pure (protectionfrom "Lawful") <|> string "Protection from Chaotic" *> pure (protectionfrom "Chaotic") 
    <|> string "See Invisibility" *> pure seeinvisibility <|> string "Mirror Image" *> pure mirrorimage <|> string "Lightning Bolt" *> pure lightningbolt 
    <|> string "Scorching Ray" *> pure scorchingray <|> string "Herosim" *> pure heroism <|> string "Fire Shield" *> pure fireshield 
    <|> string "Hideous Laughter" *> pure hideouslaughter <|> string "Confusion" *> pure confusion <|> string "Hold Person" *> pure hold 
    <|> string "Doom" *> pure doom <|> string "Mystic Cure 1" *> pure (curewounds 1) <|> string "Mystic Cure 2" *> pure (curewounds 2)
    <|> string "Mystic Cure 3" *> pure (curewounds 3) <|> string "Mystic Cure 4" *> pure (curewounds 4)
    <|> string "Mass Cure Light Wounds" *> pure (masscurewounds 1) <|> string "Mass Cure Moderate Wounds" *> pure (masscurewounds 2)
    <|> string "Mass Cure Serious Wounds" *> pure (masscurewounds 3) <|> string "Mass Cure Critical Wounds" *> pure (masscurewounds 4)
    <|> string "Cats Grace" *> pure catsgrace <|> string "Bulls Endurance" *> pure bullsendurance <|> string "Black Hole" *> pure blackhole
    <|> string "Bears Strength" *> pure bearsstrength <|> string "Mass Bulls Endurance" *> pure bullsendurancemass
    <|> string "Entangle" *> pure entangle <|> string "Bark Skin" *> pure barkskin <|> string "Shocking Grasp" *> pure shockinggrasp 
    <|> string "Flame Strike" *> pure flamestrike <|> string "Baleful Polymorth" *> pure balefullpolymorth <|> string "Glow Of Health" *> pure glowofhealth 
    <|> string "Solar Furnace" *> pure solarfurnace <|> string "Bless" *> pure bless <|> string "Divine Favour" *> pure divinefavour 
    <|> string "Dominate Person" *> pure dominateperson <|> string "Heal Poisen" *> pure healpoisen <|> string "Cause Light Wounds" *> pure (causewounds 1) 
    <|> string "Cause Moderate Wounds" *> pure (causewounds 2) <|> string "Cause Serious Wounds" *> pure (causewounds 3) <|> string "Cause Critical Wounds" *> pure (causewounds 4) 
    <|> string "Sleep (4 HD total)" *> pure sleep1 <|> string "Fire Bolt" *> pure firebolt <|> string "Cause Fear" *> pure causefear <|> string "Invisibility" *> pure invisibility
    <|> string "Summon Skeleton" *> pure summonskeleton <|> string "Summon Natures Ally III" *> pure summonnaturesally3
    <|> string "Summon Natures Ally II" *> pure summonnaturesally2 <|> string "Summon Natures Ally IV" *> pure summonnaturesally4
    <|> string "Summon Natures Ally I" *> pure summonnaturesally1 <|> string "Summon Natures Ally VI" *> pure summonnaturesally6
    <|> string "Summon Natures Ally V" *> pure summonnaturesally5 <|> string "Summon Vulture" *> pure summonvulture <|> string "Mage Armour" *> pure magearmour
    <|> string "Charm Animal" *> pure charmanimal <|> string "Magic Missile" *> pure magicmissile <|> string "Create Water" *> pure (noncombatspell "Create Water") 
    <|> string "Detect Magic" *> pure (noncombatspell "Detect Magic") <|> string "Guidance" *> pure (noncombatspell "Guidance") <|> string "Stabilize" *> pure stabilize
    <|> string "Nixie Charm Person" *> pure nixiecharmperson <|> string "Beetle Explosion" *> pure breathweaponbeetle <|> string "Trample" *> pure trample
    <|> string "Large Breath Weapon" *> pure breathweaponlarge <|> string "Small Breath Weapon" *> pure breathweaponwyrmling <|> string "Vine Entangle" *> pure vineentangle
    <|> string "Yellow Musk Zombie" *> pure zombiefy <|> string "Wooden Gaze" *> pure woodengaze <|> string "Tangle Foot Bag" *> pure tnaglefootbag 
    <|> string "Flash Power" *> pure flashpowder <|> string "Dragon Gland" *> pure dragongland <|> string "Holographic Clone" *> pure holographicclone 
    <|> string "Improved Hurry" *> pure improvedhurry <|> string "Dispel Magic" *> pure dispellmagic <|> string "Remove Affliction" *> pure removeaffliction 
    <|> string "Slow Poison" *> pure slowpoison <|> string "Keen Senses" *> pure keensenses <|> string "Mind Thrust 1" *> pure (mindthrust 1) 
    <|> string "Mind Thrust 2" *> pure (mindthrust 2) <|> string "Mind Thrust 3" *> pure (mindthrust 3) <|> string "Mind Thrust 4" *> pure (mindthrust 4) 
    <|> string "Command Undead" *> pure commandundead <|> string "Wall of Steel" *> pure wallosteel <|> string "Repulsion" *> pure repulsionspell
    <|> string "Destruction" *> pure destruction <|> string "Aura of Fear" *> pure auraoffear <|> string "Improved Invisibility" *> pure improvedinvisibility
    <|> string "Haste" *> pure haste
    <|> do
    string "Cone of Cold (Wand) "
    a1 <- integral
    string " "
    a2 <- integral
    return (coneofcoldwand a1 a2)
    <|> do
    string "Breath: "
    a1 <- elemental
    string " "
    a2 <- list integral
    string " "
    a3 <- integral
    return (breathweaponmulti a1 a2 a3)
    {-<|> do
    a1 <- takestring
    return (noncombatspell a1)-}

statusEffect :: Monad m => ParserT m StatusEffect 
statusEffect = string "Undead" *> pure Undead <|> string "Invisible" *> pure  Invisible <|> string "ImmunetoCriticals" *> pure  ImmunetoCriticals 
    <|> string "MirrorImage" *> pure  MirrorImage <|> string "TrueStrike" *> pure  TrueStrike <|> string "GreaterEvasion" *> pure  GreaterEvasion 
    <|> string "Evasion" *> pure  Evasion <|> string "Sing" *> pure  Sing <|> string "Rage" *> pure  Rage 
    <|> string "BearStrength" *> pure  BearStrength <|> string "StoneSkin" *> pure  StoneSkin 
    <|> string "Blind" *> pure  Blind <|> string "Stealth" *> pure  Stealth <|> string "Repelled" *> pure  Repelled  
    <|> string "Prone" *> pure  Prone <|> string "UncannyDodge" *> pure  UncannyDodge <|> string "Disarmed" *> pure  Disarmed 
    <|> string "Getem" *> pure  Getem <|> string "ExpertAttack" *> pure  ExpertAttack <|> string "ImprovedGetem" *> pure  ImprovedGetem 
    <|> string "ForceField" *> pure  ForceField <|> string "Mobility" *> pure  Mobility <|> string "ArcanStrike" *> pure  ArcanStrike 
    <|> string "Fatigued" *> pure  Fatigued <|> string "Exhausted" *> pure  Exhausted <|> string "Shaken" *> pure  Shaken 
    <|> string "Fear" *> pure  Fear <|> string "Sickened" *> pure  Sickened <|> string "Nauseated" *> pure  Nauseated 
    <|> string "ImmunetoPoisen" *> pure  ImmunetoPoisen <|> string "ImmunetoMagic" *> pure  ImmunetoMagic <|> string "ImmunetoDiseae" *> pure  ImmunetoDiseae 
    <|> string "Sleep" *> pure  Sleep <|> string "KeenSenses" *> pure  KeenSenses <|> string "ImmunetoSneakAttack" *> pure  ImmunetoSneakAttack 
    <|> string "SpellFocus" *> pure  SpellFocus <|> string "Burning" *> pure  Burning <|> string "Paralyzed" *> pure  Paralyzed 
    <|> string "Confused" *> pure  Confused <|> string "Petrified" *> pure  Petrified  <|> string "Laughter" *> pure  Laughter 
    <|> string "ImprovedInvisibility" *> pure  ImprovedInvisibility <|> string "ProtectiveWard" *> pure  ProtectiveWard <|> string "Dazed" *> pure  Dazed  
    <|> string "Despair" *> pure  Despair <|> string "AuraofDespair" *> pure  AuraofDespair <|> string "SpellTurning" *> pure  SpellTurning 
    <|> string "Dominated" *> pure  Dominated <|> string "BeastShape" *> pure  BeastShape <|> string "ElementalBodyIII" *> pure  ElementalBodyIII 
    <|> string "ElementalBodyIV" *> pure  ElementalBodyIV <|> string "SmiteEvil" *> pure  SmiteEvil <|> string "DivineBond" *> pure  DivineBond 
    <|> string "WeaponFinesse" *> pure  WeaponFinesse <|> string "FormoftheDragon" *> pure  FormoftheDragon <|> string "Stigmata" *> pure  Stigmata 
    <|> string "ChannelSmite" *> pure  ChannelSmite <|> string "MagicFang" *> pure  MagicFang <|> string "ChannelShield" *> pure  ChannelShield 
    <|> string "Aid" *> pure  Aid <|> string "Bless" *> pure  Bless <|> string "DeadlyAim" *> pure  DeadlyAim 
    <|> string "WereRaptor" *> pure  WereRaptor <|> string "Shield" *> pure  Shield <|> string "Blur" *> pure  Blur 
    <|> string "Heroism" *> pure  Heroism <|> string "FireShield" *> pure  FireShield <|> string "CatsGrace" *> pure  CatsGrace 
    <|> string "Held" *> pure  Held <|> string "BullsEndurance" *> pure  BullsEndurance <|> string "GrowClaws" *> pure  GrowClaws 
    <|> string "Entangled" *> pure  Entangled <|> string "BarkSkin" *> pure  BarkSkin <|> string "Polymorth" *> pure  Polymorth 
    <|> string "Summoned" *> pure  Summoned <|> string "CombatShot" *> pure  CombatShot <|> string "SeeInvisibility" *> pure  SeeInvisibility 
    <|> string "DivineFavour" *> pure  DivineFavour <|> string "Poisened" *> pure  Poisened <|> string "ImmuneMindInfluencingEffects" *> pure  ImmuneMindInfluencingEffects 
    <|> string "Stunned" *> pure  Stunned <|> string "StunningFist" *> pure  StunningFist <|> string "Ki" *> pure  Ki 
    <|> string "Staggered" *> pure  Staggered <|> string "DarkHeal" *> pure  DarkHeal <|> string "Grappled" *> pure  Grappled 
    <|> string "Cover" *> pure  Cover <|> string "Charged" *> pure  Charged <|> string "Incorporal" *> pure  Incorporal 
    <|> string "Pounce" *> pure  Pounce <|> string "Turned" *> pure  Turned <|> string "Commanded" *> pure  Commanded 
    <|> string "ChannelResistance" *> pure  ChannelResistance <|> string "ShieldofFaith" *> pure  ShieldofFaith <|> string "ProtectionFrom" *> pure  ProtectionFrom 
    <|> string "DefensiveStance" *> pure  DefensiveStance <|> string "ImprovedGrapple" *> pure  ImprovedGrapple <|> string "ImprovedDisarm" *> pure  ImprovedDisarm 
    <|> string "ImprovedTrip" *> pure ImprovedTrip <|> string "Haste" *> pure Haste <|> string "Slow" *> pure Slow <|> string "Cleave" *> pure Cleave
    <|> string "ImprovedDoubleAttack" *> pure ImprovedDoubleAttack <|> string "TripleAttack" *> pure TripleAttack <|> string "PhotonMode" *> pure PhotonMode 
    <|> string "GravitonMode" *> pure GravitonMode <|> string "HIPS" *> pure HIPS <|> string "TrickAttack" *> pure TrickAttack <|> string "FlatFooted" *> pure FlatFooted
    <|> string "DarkMatter" *> pure DarkMatter

race :: Monad m => ParserT m Race 
race = string "Human" *> pure Human <|> string "Elf" *> pure  Elf <|> string "Gnome" *> pure  Gnome 
    <|> string "Orc" *> pure  Orc <|> string "Halfling" *> pure  Halfling 
    <|> string "Animal" *> pure  Animal <|> string "Outsider" *> pure  Outsider 
    <|> string "Abnormal" *> pure  Abnormal <|> string "Plant" *> pure  Plant 
    <|> string "Elemental" *> pure  Elemental <|> string "Dragon" *> pure  Dragon 
    <|> string "UnDead" *> pure  UnDead <|> string "Tengu" *> pure  Tengu 
    <|> string "Kitsune" *> pure  Kitsune <|> string "Construct" *> pure  Construct <|> string "Fey" *> pure Fey
    <|> string "Kasatha" *> pure  Kasatha <|> string "Lashunta" *> pure  Lashunta <|> string "Android" *> pure Android
    <|> string "Shirren" *> pure  Shirren <|> string "Vesk" *> pure  Vesk <|> string "Ysoki" *> pure Ysoki

size :: Monad m => ParserT m Size 
size = string "Diminutive" *> pure Diminutive <|> string "Tiny" *> pure  Tiny 
    <|> string "Small" *> pure  Small <|> string "Medium" *> pure  Medium 
    <|> string "Large" *> pure  Large <|> string "Huge" *> pure  Huge 
    <|> string "Gargantuan" *> pure  Gargantuan

health :: Monad m => ParserT m Health
health = string "Dead" *> pure Dead <|> string "Stabelized" *> pure Stabelized <|> string "Dieing" *> pure Dieing 
    <|> string "Danger" *> pure Danger <|> string "Wounded" *> pure Wounded <|> string "Hurt" *> pure Hurt 
    <|> string "Blooded" *> pure  Blooded <|> string "Grazed" *> pure Grazed <|> string "Healthy" *> pure Healthy

lawfulness :: Monad m => ParserT m Lawfulness 
lawfulness = string "Lawful" *> pure Lawful 
    <|> string "Neutral" *> pure Neutral 
    <|> string "Chaotic" *> pure Chaotic

benificence :: Monad m => ParserT m Benificence 
benificence = string "Evil" *> pure Evil 
    <|> string "NeutraL" *> pure NeutraL 
    <|> string "Good" *> pure Good 

damageType :: Monad m => ParserT m DamageType
damageType = string "Normal" *> pure Normal <|> string "Bludgeoning" *> pure Bludgeoning <|> string "Slashing" *> pure Slashing <|> string "Piercing" *> pure Piercing <|> string "Reduced" *> pure Reduced

ability :: Monad m => ParserT m Ability 
ability = string "Strength" *> pure Strength <|> string "Dexterity" *> pure Dexterity 
    <|> string "Constitution" *> pure Constitution <|> string "Intelligence" *> pure Intelligence 
    <|> string "Wisdom" *> pure Wisdom <|> string "Charisma" *> pure Charisma

statDamageType :: Monad m => ParserT m StatDamageType 
statDamageType = string "Magic" *> pure Magic <|> string "Poisen" *> pure Poisen <|> string "Disease" *> pure Disease

smite :: Monad m => ParserT m Smite
smite = do
    string "Smite {condition = "
    a1 <- eitherparser lawfulness benificence
    string ", smiteattack = "
    a2 <- integral
    string ", smitedamage = "
    a3 <- integral
    char '}'
    return $ Smite a1 a2 a3
    <|>
    parens smite
    where
        parens p = char '(' *> p <* char ')'

attackInfo :: Monad m => ParserT m a -> ParserT m (AttackInfo a)
attackInfo item = do
                        string "AInfo {attackname = " 
                        a1 <- takestring
                        string ", range = " 
                        a2 <- range
                        string ", increment = " 
                        a3 <- integral
                        string ", damagetype = " 
                        a4 <- damageType
                        string ", material = " 
                        a5 <- material
                        string ", attackbonus = "
                        a6 <- maybeparse item
                        string ", versus = " 
                        a7 <- defense
                        string ", critical = " 
                        a8 <- critical
                        string ", dambonus = " 
                        a12 <- maybeparse ability
                        string ", alignment = " 
                        a13 <- alingment
                        string ", preparedagainstcharge = " 
                        a14 <- bool
                        string ", spellproxy = " 
                        a15 <- tuple bool bool
                        char '}'
                        return $ AInfo a1 a2 a3 a4 a5 a6 a7 a8 a12 a13 a14 a15
    <|>
    parens (attackInfo item)
    where
        parens p = char '(' *> p <* char ')'

attack :: Monad m => ParserT m a -> ParserT m b -> ParserT m (Attack a b) 
attack item1 item2 = do
                        string "Attack {info = " 
                        a1 <- attackInfo item1
                        string ", rolls = " 
                        a2 <- attackRoll (item2)
                        string ", ddice = " 
                        a3 <- maybeparse (damageDice item2)
                        string ", mddice = " 
                        a4 <- list (damageDice item2)
                        string ", mstatdamage = " 
                        a5 <- maybeparse (statdamage item2)
                        string ", meffect = " 
                        a6 <- maybeparse (effect (temporal2 item2))
                        string ", msingddice = " 
                        a7 <- maybeparse (damageDice item2)
                        string ", msingeffect = " 
                        a9 <- list (effect (temporal2 item2))
                        char '}'
                        return $ Attack a1 a2 a3 a4 a5 a6 a7 a9
    <|>
    parens (attack item1 item2)
    where
        parens p = char '(' *> p <* char ')'

critical :: Monad m => ParserT m Critical 
critical = do
                string "C {threshold = " 
                a1 <- integral
                string ", multiplier = " 
                a2 <- integral
                string ", critbonus = " 
                a3 <- integral
                char '}'
                return $ C a1 a2 a3
    <|>
    parens critical
    where
        parens p = char '(' *> p <* char ')'

attackRoll :: Monad m => ParserT m a -> ParserT m (AttackRoll a)
attackRoll item = do
                            string "AttackRoll {tohitroll = " 
                            a1 <- item
                            string ", hitpercentage = " 
                            a3 <- item
                            string ", criticalpercentage = " 
                            a4 <- item
                            string ", mIpercentage = " 
                            a5 <- item
                            string ", stealthroll = " 
                            a2 <- item
                            string ", effectroll = " 
                            a7 <- item
                            string ", statdamageroll = " 
                            a8 <- item
                            string ", msddroll = " 
                            a9 <- item
                            char '}'
                            return $ AttackRoll a1 a3 a4 a5 a2 a7 a8 a9
    <|>
    parens (attackRoll item)
    where
        parens p = char '(' *> p <* char ')'

effect :: Monad m => ParserT m a -> ParserT m (Effect a)
effect item = do
    string "Effect {statuseffect = " 
    a1 <- statusEffect
    string ", temporal = " 
    a2 <- item
    string ", temporalmodifierbonus = " 
    a3 <- maybeparse integral
    string ", efftarget = " 
    a4 <- maybeparse target
    char '}'
    return $ Effect a1 a2 a3 a4
    <|>
    parens (effect item)
    where
        parens p = char '(' *> p <* char ')'

statdamage :: Monad m => ParserT m a -> ParserT m (StatDamage a)
statdamage item = do
                    string "SD {statdamagetype = " 
                    a1 <- maybeparse statDamageType
                    string ", defense = " 
                    a2 <- maybeparse target
                    string ", target = " 
                    a3 <- maybeparse ability
                    string ", permdice = " 
                    a4 <- list integral
                    string ", dice = " 
                    a5 <- item
                    string ", dicebonus = " 
                    a6 <- integral
                    string ", duration = " 
                    a7 <- list integral
                    string ", durationbonus = " 
                    a8 <- integral
                    char '}'
                    return $ SD a1 a2 a3 a4 a5 a6 a7 a8
    <|>
    parens (statdamage item)
    where
        parens p = char '(' *> p <* char ')'

defense :: Monad m => ParserT m Defense 
defense = string "Defense" *> pure Defense <|> string "Flatfooted" *> pure Flatfooted 
    <|> string "Touch" *> pure Touch <|> string "Reflex" *> pure Reflex 
    <|> string "Fortitude" *> pure Fortitude <|> string "Will" *> pure Will 
    <|> string "CMD" *> pure CMD <|> string "Perception" *> pure Perception

range :: Monad m => ParserT m Range 
range = string "Melee" *> pure Melee <|> string "Missile" *> pure Missile 

target :: Monad m => ParserT m Target 
target = do
    string "Target {defensetype = " 
    a1 <- defense
    string ", bonus = "
    a2 <- integral
    char '}'
    return $ Target a1 a2
    <|>
    parens (target)
    where
        parens p = char '(' *> p <* char ')'

saveProfile :: Monad m => ParserT m SaveProfile 
saveProfile = string "Negates" *> pure Negates <|> string "Half" *> pure Half <|> do
                                                                                    string "Special {damage = "
                                                                                    a1 <- integral 
                                                                                    char '}'
                                                                                    return $ Special a1

damageDice :: Monad m => ParserT m a -> ParserT m (DamageDice a)
damageDice item = do
                    string "D {damagedice = " 
                    a1 <- item
                    string ", damagebonus = " 
                    a2 <- integral
                    string ", elemental = " 
                    a3 <- maybeparse elemental
                    string ", ddtarget = " 
                    a4 <- maybeparse target
                    string ", saveprofile = " 
                    a5 <- saveProfile
                    char '}'
                    return $ D a1 a2 a3 a4 a5
                    <|>
                    parens (damageDice item)
                    where
                        parens p = char '(' *> p <* char ')'

alingment :: Monad m => ParserT m Alignment 
alingment = do
    string "Al {lawfullness = "
    a <- lawfulness
    string ", benificence = "
    b <- benificence
    char '}'
    return $ Al a b
    <|>
    parens (alingment)
    where
        parens p = char '(' *> p <* char ')'

favouredEnemy :: Monad m => ParserT m a -> ParserT m (FavouredEnemy a)
favouredEnemy item = do
    string "FE {human = " 
    a1 <- item
    string ", elf = " 
    a2 <- item
    string ", gnome = " 
    a3 <- item
    string ", orc = " 
    a4 <- item
    string ", halfling = " 
    a5 <- item
    string ", animal = " 
    a6 <- item
    string ", outsider = " 
    a7 <- item
    string ", abnormal = " 
    a8 <- item
    string ", plant = " 
    a9 <- item
    string ", elemental = " 
    a10 <- item
    string ", dragon = " 
    a11 <- item
    string ", undead = " 
    a12 <- item
    string ", tengu = " 
    a13 <- item
    string ", kitsune = " 
    a14 <- item
    string ", construct = " 
    a15 <- item
    string ", fey = " 
    a16 <- item
    char '}'
    return $ FE a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16
    <|>
    parens (favouredEnemy item)
    where
        parens p = char '(' *> p <* char ')'

classFeatures :: Monad m => ParserT m b -> ParserT m a -> ParserT m (ClassFeatures b a)
classFeatures item1 item2 = do
    string "CF {mysticspells = " 
    a1 <- spellBook item1 item2
    string ", arcanespells = " 
    a2 <- spellBook item1 item2
    string ", solarionfeatures = " 
    a3 <- solfeatures
    string ", envoyfeatures = " 
    a4 <- envfeatures
    string ", feats = " 
    a5 <- feats
    char '}'
    return $ CF a1 a2 a3 a4 a5
    <|>
    parens (classFeatures item1 item2)
    where
        parens p = char '(' *> p <* char ')'

spellBook :: Monad m => ParserT m b -> ParserT m a -> ParserT m (SpellBook b a)
spellBook item1 item2 = do
    string "SB {known = "
    a1 <- spellLevel . list $ item1
    string ", perday = "
    a2 <- spellLevel item2
    char '}'
    return $ SB a1 a2
    <|>
    parens (spellBook item1 item2)
    where
        parens p = char '(' *> p <* char ')'

spellLevel :: Monad m => ParserT m a -> ParserT m (SpellLevel a)
spellLevel item = do
    string "SL {zero = " 
    a1 <- item
    string ", first = " 
    a2 <- item
    string ", second = " 
    a3 <- item
    string ", third = " 
    a4 <- item
    string ", fourth = " 
    a5 <- item
    string ", fifth = " 
    a6 <- item
    string ", sixth = " 
    a7 <- item
    string ", seventh = " 
    a8 <- item
    string ", eighth = " 
    a9 <- item
    string ", ninth = " 
    a0 <- item
    char '}'
    return $ SL a1 a2 a3 a4 a5 a6 a7 a8 a9 a0
    <|>
    parens (spellLevel item)
    where
        parens p = char '(' *> p <* char ')'

tuple :: Monad m => ParserT m a -> ParserT m b -> ParserT m (a, b)
tuple item1 item2 = char '(' *> liftA2 (,) item1 go <* char ')'
    <|>
    parens (tuple item1 item2)
    where
        parens p = char '(' *> p <* char ')'
        go = char ',' *> item2

eitherparser :: Monad m => ParserT m a -> ParserT m b -> ParserT m (Either a b)
eitherparser item1 item2 = string "Left" *> skipSpace *> (fmap Left item1)
                    <|> string "Right" *> skipSpace *> (fmap Right item2) <|> parens (eitherparser item1 item2)
                    where
                        parens p = char '(' *> p <* char ')'

feats :: Monad m => ParserT m Feats
feats = do
    string "Feats {deadlyaim = " 
    a1 <- bool
    string ", weaponfinesse = " 
    a2 <- bool
    string ", arcanestrike = " 
    a3 <- bool
    string ", improvedinitiative = " 
    a4 <- bool
    string ", improveddisarm = " 
    a5 <- bool
    string ", improvedtrip = " 
    a6 <- bool
    string ", improvedgrapple = " 
    a7 <- bool
    char '}'
    return $ Feats a1 a2 a3 a4 a5 a6 a7
    <|>
    parens (feats)
    where
        parens p = char '(' *> p <* char ')'

profile :: Monad m => ParserT m a -> ParserT m (Profile a)
profile item = do
    string "NormalDamage {runA = "
    a <- item
    char '}'
    return $ NormalDamage a
    <|> string "Immune" *> pure Immune
    <|> string "Cures" *> pure Cures
    <|> string "Vulnerable" *> pure Vulnerable
    <|>
    parens (profile item)
    where
        parens p = char '(' *> p <* char ')'

temporal2 :: Monad m => ParserT m a -> ParserT m (Temporal a)
temporal2 item = do
    string "NA {duration = "
    a <- maybeparse integral
    string ", character = "
    b <- takestring
    string ", modifyer = "
    c <- item
    char '}'
    return $ NA a b c
    <|> do
    string "Absent {duration = "
    a <- maybeparse integral
    string ", character = "
    b <- takestring
    string ", modifyer = "
    c <- item
    char '}'
    return $ Absent a b c
    <|> do
    string "Off {duration = "
    a <- maybeparse integral
    string ", character = "
    b <- takestring
    string ", modifyer = "
    c <- item
    char '}'
    return $ Off a b c
    <|> do
    string "On {duration = "
    a <- maybeparse integral
    string ", character = "
    b <- takestring
    string ", modifyer = "
    c <- item
    char '}'
    return $ On a b c
    <|> do
    string "Present {duration = "
    a <- maybeparse integral
    string ", character = "
    b <- takestring
    string ", modifyer = "
    c <- item
    char '}'
    return $ Present a b c
    <|> do
    string "Permanent {duration = "
    a <- maybeparse integral
    string ", character = "
    b <- takestring
    string ", modifyer = "
    c <- item
    char '}'
    return $ Permanent a b c
    <|>
    parens (temporal2 item)
    where
        parens p = char '(' *> p <* char ')'

skillfeatprofile :: Monad m => ParserT m SkillFeatProfile
skillfeatprofile = string "None" *> pure None <|> string "Lesser" *> pure Lesser <|> string "Greater" *> pure Greater

featprofile :: Monad m => ParserT m FeatProfile
featprofile = do
    string "FP {weaponfeat = "
    a <- weaponfeat
    string ", weaponfocus = "
    b <- bool
    char '}'
    return $ FP a b
    <|>
    parens (featprofile)
    where
        parens p = char '(' *> p <* char ')'

spellfeat :: Monad m => ParserT m a -> ParserT m (SpellFeat a)
spellfeat item = do
    string "SF {abjuration = "
    a <- item
    string ", conjuration = "
    b <- item
    string ", divination = "
    c <- item
    string ", enchantment = "
    d <- item
    string ", evocation = "
    e <- item
    string ", illusion = "
    f <- item
    string ", necromancy = "
    g <- item
    string ", transmutation = "
    h <- item
    char '}'
    return $ SF a b c d e f g h
    <|>
    parens (spellfeat item)
    where
        parens p = char '(' *> p <* char ')'

maybeparse :: Monad m => ParserT m a -> ParserT m (Maybe a)
maybeparse item = string "Just" *> skipSpace *> fmap pure item <|> string "Nothing" *> pure Nothing <|> parens (maybeparse item)
    where
        parens p = char '(' *> p <* char ')'


abilityscores :: Monad m => ParserT m a -> ParserT m (AbilityScores a)
abilityscores item = do
    string "AS "
    a <- item
    skipSpace
    b <- item
    skipSpace
    c <- item
    skipSpace
    d <- item
    skipSpace
    e <- item
    skipSpace
    f <- item
    return $ AS a b c d e f
    <|>
    parens (abilityscores item)
    where
        parens p = char '(' *> p <* char ')'

bonustypes :: Monad m => ParserT m a -> ParserT m (BonusTypes a)
bonustypes item = do
    string "BT {luck = "
    a <- item
    string ", deflection = "
    b <- item
    string ", magic = "
    c <- item
    string ", holy = "
    d <- item
    string ", misc = "
    e <- item
    string ", size = "
    f <- item
    string ", item = "
    g <- item
    string ", penalty = "
    h <- item
    char '}'
    return $ BT a b c d e f g h
    <|>
    parens (bonustypes item)
    where
        parens p = char '(' *> p <* char ')'

elementalresistance :: Monad m => ParserT m a -> ParserT m (ElementalResistance a)
elementalresistance item = do
    string "ER {fire = "
    a <- item
    string ", earth = "
    b <- item
    string ", ice = "
    c <- item
    string ", wind = "
    d <- item
    string ", lightning = "
    e <- item
    string ", acid = "
    f <- item
    string ", negative = "
    g <- item
    string ", positive = "
    h <- item
    char '}'
    return $ ER a b c d e f g h
    <|>
    parens (elementalresistance item)
    where
        parens p = char '(' *> p <* char ')'

armour :: Monad m => ParserT m a -> ParserT m (Armour a)
armour item = do
    string "A {energy = "
    a <- item
    string ", physical = "
    b <- item
    string ", maxDex = "
    c <- maybeparse integral
    string ", achPenalty = "
    d <- maybeparse integral
    string ", name = "
    e <- takestring
    char '}'
    return $ A a b c d e
    <|>
    parens (armour item)
    where
        parens p = char '(' *> p <* char ')'

nonarmour :: Monad m => ParserT m a -> ParserT m (NArmour.NonArmour a)
nonarmour item = do
    string "NA {saves = "
    a <- item
    string ", attacks = "
    b <- item
    char '}'
    return $ NArmour.NA a b
    <|>
    parens (nonarmour item)
    where
        parens p = char '(' *> p <* char ')'

bonuses :: Monad m => ParserT m a -> ParserT m (Bonuses a)
bonuses item = do
    string "B {abilityscores = "
    a <- abilityscores item
    string ", miscclass = "
    b <- classinfo item
    string ", defense = "
    c <- nonarmour item
    string ", maxdex = "
    d <- item
    string ", damage = "
    e <- item
    string ", spellresistance = "
    f <- item
    string ", initiative = "
    g <- item
    string ", skills = "
    h <- skills item
    char '}'
    return $ B a b c d e f g h
    <|>
    parens (bonuses item)
    where
        parens p = char '(' *> p <* char ')'

weapon :: Monad m => ParserT m Weapon
weapon = do
    string "Weapon {weapondamagetype = "
    a <- damageType
    string ", weapontype = "
    b <- weapontype
    string ", weaponddice = "
    c <- damageDice (list integral)
    string ", weaponrange = "
    d <- integral
    string ", enchantments = "
    e <- list enchantment
    string ", name = "
    f <- takestring
    char '}'
    return $ Weapon a b c d e f
    <|>
    parens (weapon)
    where
        parens p = char '(' *> p <* char ')'

effects :: Monad m => ParserT m a -> ParserT m (Effects a)
effects item = do
    string "E {undead = "
    a1 <- item
    string ", invisible = "
    a2 <- item
    string ", immunetocriticals = "
    a3 <- item
    string ", mirrorImage = "
    a4 <- item
    string ", trueStrike = "
    a5 <- item
    string ", greaterEvasion = "
    a6 <- item
    string ", evasion = "
    a7 <- item
    string ", sing = "
    a8 <- item
    string ", rage = "
    a9 <- item
    string ", bearStrength = "
    a10 <- item
    string ", stoneSkin = "
    a11 <- item
    string ", repelled = "
    a12 <- item
    string ", blind = "
    a13 <- item
    string ", stealth = "
    a14 <- item
    string ", bleeding = "
    a15 <- item
    string ", prone = "
    a16 <- item
    string ", uncannydodge = "
    a17 <- item
    string ", disarmed = "
    a18 <- item
    string ", getem = "
    a19 <- item 
    string ", expertattack = "
    a20 <- item
    string ", improvedgetem = "
    a21 <- item
    string ", forcefield = "
    a22 <- item
    string ", mobility = "
    a23 <- item
    string ", arcanestrike = "
    a24 <- item
    string ", fatigued = "
    a25 <- item
    string ", exhausted = "
    a26 <- item
    string ", shaken = "
    a27 <- item
    string ", fear = "
    a28 <- item
    string ", sickened = "
    a29 <- item
    string ", nauseated = "
    a30 <- item
    string ", immunetopoisen = "
    a31 <- item
    string ", immunetomagic = "
    a32 <- item
    string ", immunetodisease = "
    a33 <- item
    string ", sleep = "
    a34 <- item
    string ", keensenses = "
    a35 <- item
    string ", immunetosneakattack = "
    a36 <- item
    string ", spellfocus = "
    a37 <- item
    string ", burning = "
    a38 <- item
    string ", paralyzed = "
    a39 <- item
    string ", confused = "
    a40 <- item
    string ", petrified = "
    a41 <- item
    string ", laughter = "
    a42 <- item
    string ", improvedinvisibility = "
    a43 <- item
    string ", protectiveward = "
    a44 <- item
    string ", dazed = "
    a45 <- item
    string ", despair = "
    a46 <- item
    string ", auraofdespair = "
    a47 <- item
    string ", spellturning = "
    a48 <- item
    string ", dominated = "
    a49 <- item
    string ", beastshape = "
    a50 <- item
    string ", elementalbodyiii = "
    a51 <- item
    string ", elementalbodyiv = "
    a52 <- item
    string ", smiteevil = "
    a53 <- item
    string ", divinebond = "
    a54 <- item
    string ", weaponfinesse = "
    a55 <- item
    string ", formofthedragon = "
    a56 <- item
    string ", stigmata = "
    a57 <- item
    string ", channelsmite = "
    a58 <- item
    string ", magicfang = "
    a59 <- item
    string ", channelshield = "
    a60 <- item
    string ", aid = "
    a61 <- item
    string ", bless = "
    a62 <- item
    string ", deadlyaim = "
    a63 <- item
    string ", wereraptor = "
    a64 <- item
    string ", shield = "
    a65 <- item
    string ", blur = "
    a66 <- item
    string ", heroism = "
    a67 <- item
    string ", fireshield = "
    a68 <- item
    string ", catsgrace = "
    a69 <- item
    string ", held = "
    a70 <- item
    string ", bullsendurance = "
    a71 <- item
    string ", growclaws = "
    a72 <- item
    string ", entangled = "
    a73 <- item
    string ", barkskin = "
    a74 <- item
    string ", polymorth = "
    a75 <- item
    string ", summoned = "
    a76 <- item
    string ", combatShot = "
    a77 <- item
    string ", seeinvisibility = "
    a78 <- item
    string ", divinefavour = "
    a79 <- item
    string ", poisened = "
    a80 <- item 
    string ", immuneMindInfluencingEffects = "
    a81 <- item
    string ", stunned = "
    a82 <- item
    string ", stunningfist = "
    a83 <- item
    string ", ki = "
    a84 <- item
    string ", staggered = "
    a85 <- item
    string ", darkheal = "
    a86 <- item
    string ", grappled = "
    a87 <- item
    string ", cover = "
    a88 <- item
    string ", charged = "
    a89 <- item
    string ", incorporal = "
    a90 <- item
    string ", pounce = "
    a91 <- item
    string ", turned = "
    a92 <- item
    string ", commanded = "
    a93 <- item
    string ", channelresistance = "
    a94 <- item
    string ", shieldoffaith = "
    a95 <- item
    string ", protectionfrom = "
    a96 <- item
    string ", defensivestance = "
    a97 <- item
    string ", improvedgrapple = "
    a98 <- item
    string ", improveddisarm = "
    a99 <- item
    string ", improvedtrip = "
    a100 <- item
    string ", magearmour = "
    a101 <- item
    string ", regeneration = "
    a102 <- item
    string ", haste = "
    a103 <- item
    string ", slow = "
    a104 <- item
    string ", cleave = "
    a105 <- item
    string ", improveddoubleattack = "
    a106 <- item
    string ", tripleattack = "
    a107 <- item
    string ", photonmode = "
    a108 <- item
    string ", gravitonmode = "
    a109 <- item
    string ", hips = "
    a110 <- item
    string ", trickattack = "
    a111 <- item
    string ", flatfooted = "
    a112 <- item
    string ", darkmatter = "
    a113 <- item
    char '}'
    return (E a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 
        a30 a31 a32 a33 a34 a35 a36 a37 a38 a39 a40 a41 a42 a43 a44 a45 a46 a47 a48 a49 a50 a51 a52 a53 a54 a55 a56
        a57 a58 a59 a60 a61 a62 a63 a64 a65 a66 a67 a68 a69 a70 a71 a72 a73 a74 a75 a76 a77 a78 a79 a80 a81 a82 a83
        a84 a85 a86 a87 a88 a89 a90 a91 a92 a93 a94 a95 a96 a97 a98 a99 a100 a101 a102 a103 a104 a105 a106 a107 a108 a109 
        a110 a111 a112 a113)
    <|>
    parens (effects item)
    where
        parens p = char '(' *> p <* char ')'

skills :: Monad m => ParserT m a -> ParserT m (Skills a)
skills item = do
        string "Skills {acrobatics = "         
        a1 <- item
        string ", athletics = " 
        a2 <- item
        string ", bluff = " 
        a3 <- item
        string ", computers = " 
        a4 <- item
        string ", culture = " 
        a5 <- item
        string ", diplomacy = " 
        a6 <- item
        string ", disguise = " 
        a7 <- item
        string ", engineering = " 
        a8 <- item
        string ", intimidate = " 
        a9 <- item
        string ", lifeScience = " 
        a10 <- item
        string ", medicine = " 
        a11 <- item
        string ", mysticism = " 
        a12 <- item
        string ", perception = " 
        a13 <- item
        string ", physicalScience = " 
        a14 <- item
        string ", piloting = " 
        a15 <- item
        string ", profession = " 
        a16 <- item
        string ", senseMotive = " 
        a17 <- item
        string ", sleightOfHand = " 
        a18 <- item
        string ", stealth = " 
        a19 <- item
        string ", survival = " 
        a20 <- item
        char '}'
        return (Skills a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20)
        <|>
        parens (skills item)
        where
            parens p = char '(' *> p <* char ')'

damagereduction :: Monad m => ParserT m a -> ParserT m (DamageReduction a)
damagereduction item = do
    string "DR {reduc = " 
    a1 <- item
    string ", good = " 
    a2 <- item
    string ", evil = " 
    a3 <- item
    string ", chaos = " 
    a4 <- item
    string ", law = " 
    a5 <- item
    string ", bludgeon = " 
    a6 <- item
    string ", slash = "
    a7 <- item 
    string ", pierc = " 
    a8 <- item
    string ", magical = " 
    a9 <- item
    string ", norm = " 
    a10 <- item
    char '}'
    return (DR a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)
    <|>
    parens (damagereduction item)
    where
        parens p = char '(' *> p <* char ')'

weapons :: Monad m => ParserT m a -> ParserT m (Weapons a)
weapons item = do
    string "W {grenade = "
    a1 <- item
    string ", smallArm = "
    a2 <- item
    string ", longArm = "
    a3 <- item
    string ", basicMelee = "
    a4 <- item
    string ", advancedMelee = "
    a5 <- item
    string ", heavyWeapon = "
    a6 <- item
    string ", sniper = "
    a7 <- item
    char '}'
    return $ W a1 a2 a3 a4 a5 a6 a7
    <|>
    parens (weapons item)
    where
        parens p = char '(' *> p <* char ')'

classes :: Monad m => ParserT m a -> ParserT m (Classes a) 
classes item = do
    string "Classes {env = "
    a1 <- item
    string ", mech = "
    a2 <- item
    string ", myst = "
    a3 <- item
    string ", oper = "
    a4 <- item
    string ", solar = "
    a5 <- item
    string ", sold = "
    a6 <- item
    string ", techno = "
    a7 <- item
    char '}'
    return $ Classes a1 a2 a3 a4 a5 a6 a7
    <|>
    parens (classes item)
    where
        parens p = char '(' *> p <* char ')'

equipmentparser ::  Monad m => ParserT m a -> ParserT m (Equipment a)
equipmentparser item = do
    string "Equipment {forcefield = "
    i <- maybeparse (tuple integral integral)
    string ", feet = "
    a1 <- item
    string ", forearms = "
    a2 <- item
    string ", leftring = "
    a3 <- item
    string ", rightring = "
    a4 <- item
    string ", amulet = "
    a5 <- item
    string ", cloak = "
    a6 <- item
    string ", helmet = "
    a7 <- item
    char '}'
    return $ Equipment i a1 a2 a3 a4 a5 a6 a7
    <|>
    parens (equipmentparser item)
    where
        parens p = char '(' *> p <* char ')'

equipmentArea :: Monad m => ParserT m EquipmentArea
equipmentArea = string "Feet" *> pure Feet <|> string "Forearms" *> pure Forearms <|> string "LeftRing" *> pure LeftRing 
    <|> string "RightRing" *> pure RightRing <|> string "Amulet" *> pure Amulet <|> string "Cloak" *> pure Cloak 
    <|> string "Helmet" *> pure Helmet <|> parens equipmentArea
    where
        parens p = char '(' *> p <* char ')'

magicitem :: Monad m => ParserT m a -> ParserT m (MagicItem a)
magicitem item = do
    string "MagicItem {bonai = "
    a1 <- bonuses (bonustypes item)
    string ", spells = "
    a2 <- list spelltemplate
    string ", name = "
    a3 <- takestring
    string ", area = "
    a4 <- equipmentArea
    char '}'
    return $ MagicItem a1 a2 a3 a4
    <|>
    parens (magicitem item)
    where
        parens p = char '(' *> p <* char ')'

digit :: (Monad m, Num a) => ParserT m a
digit = next >>= charToInt

stringToInt :: (Alternative m, Num a) => String -> m a
stringToInt = fmap fromDigits . traverse charToInt

charToInt :: (Alternative m, Num a) => Char -> m a
charToInt '0' = pure 0
charToInt '1' = pure 1
charToInt '2' = pure 2
charToInt '3' = pure 3
charToInt '4' = pure 4
charToInt '5' = pure 5
charToInt '6' = pure 6
charToInt '7' = pure 7
charToInt '8' = pure 8
charToInt '9' = pure 9
charToInt _ = empty

fromDigits :: Num a => [a] -> a
fromDigits = foldl' (\b a -> a + 10 * b) 0

edebugger :: Monad m => ParserT m (Equipment (Maybe (MagicItem Int)))
edebugger = do
    string ", equipment = " 
    a51 <- equipmentparser (maybeparse (magicitem integral))
    string ", primaryhand = " 
    return a51

teststatus = over (#otherweapons) ((Left magicitemtest) :) emptystatus


statusparse :: Monad m => ParserT m Status 
statusparse = do
    string "Status {primclass = " 
    a1 <- classinfo (temporaryparse integral)
    string ", illumination = " 
    a1p1 <- maybeparse vision
    string ", vision = " 
    a1p2 <- temporaryparse vision
    string ", team = " 
    a2 <- temporaryparse team
    string ", abilityscores = " 
    a3 <- abilityscores (temporaryparse integral)
    string ", bonuses = " 
    a4 <- bonuses (bonustypes (temporaryparse integral))
    string ", equipedarmour = " 
    a5 <- (armour integral)
    string ", equipment = " 
    a51 <- equipmentparser (maybeparse (magicitem integral))
    string ", primaryhand = " 
    a6 <- weapon
    string ", otherweapons = " 
    a8 <- list (eitherparser (magicitem integral) (eitherparser (armour integral) weapon))
    string ", weaponbonus = " 
    a9 <- weapons featprofile
    string ", spellfeat = " 
    a10 <- spellfeat skillfeatprofile
    string ", damage = " 
    a11 <- temporaryparse integral
    string ", injury = " 
    a12 <- temporaryparse integral
    string ", resolve = " 
    a125 <- temporaryparse integral
    string ", elementalresistance = " 
    a13 <- elementalresistance (temporaryparse (profile (maybeparse integral)))
    string ", effects = " 
    a14 <- effects (temporaryparse (temporal2 (maybeparse integral)))
    string ", damagereduction = " 
    a15 <- damagereduction (temporaryparse (maybeparse integral))
    string ", alignment = " 
    a16 <- temporaryparse alingment
    string ", race = " 
    a17 <- temporaryparse race
    string ", classfeatures = " 
    a18 <- classFeatures spelltemplate integral
    string ", specialabilities = " 
    a19 <- list spelltemplate
    string ", itemspells = " 
    a20 <- list spelltemplate
    string ", tempsize = " 
    a21 <- temporaryparse size
    string ", concealment = " 
    a22 <- temporaryparse integral
    string ", health = " 
    a23 <- health
    string ", statdamage = " 
    a24 <- (list $ statdamage (list integral))
    string ", playerclasses = " 
    a25 <- classes integral
    string ", monsterAttack = " 
    a26 <- nelist (attack integral (list integral))
    string ", specialAttack = " 
    a27 <- attack integral (list integral)
    string ", cmb = " 
    a28 <- attack integral (list integral)
    string ", ap = " 
    a29 <- integral
    string ", movement = "
    a30 <- integral
    string ", movementtotal = "
    a31 <- integral
    string ", aura = "
    a31p2 <- maybeparse spelltemplate
    string ", skills = " 
    a32 <- skills integral
    char '}'
    return $ Status a1 a1p1 a1p2 a2 a3 a4 a5 a51 a6 a8 a9 a10 a11 a12 a125 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 a31p2 a32
    <|>
    parens statusparse
    where
        parens p = char '(' *> p <* char ')'

characterparse :: Monad m => ParserT m Character 
characterparse = do
    string "Character {name = "
    a1 <- takestring
    string ", runLocation = "
    a2 <- tuple integral integral
    string ", runStartLocation = "
    a3 <- tuple integral integral
    string ", runPicture = "
    _ <- takestring
    string ", mouseSelect = "
    a4 <- bool
    string ", topofOrder = "
    a5 <- bool
    string ", inCombat = "
    a6 <- bool
    string ", pC = "
    a61 <- bool
    string ", mousePoint = "
    a62 <- tuple float float
    string ", status = "
    a7 <- statusparse
    string ", immediateVision = "
    a8 <- list (tuple integral integral)
    string ", illuminatedSquares = "
    a9 <- list (tuple integral integral)
    char '}'
    return $ Character a1 a2 a3 defaultPicture a4 a5 a6 a61 a62 a7 a8 a9
    <|> do
    string "Character {name = "
    a1 <- takestring
    string ", runLocation = "
    a2 <- tuple integral integral
    string ", runStartLocation = "
    a3 <- tuple integral integral
    string ", runPicture = "
    _ <- takestring
    string ", mouseSelect = "
    a4 <- bool
    string ", topofOrder = "
    a5 <- bool
    string ", inCombat = "
    a6 <- bool
    string ", mousePoint = "
    a61 <- tuple float float
    string ", status = "
    a7 <- statusparse
    string ", immediateVision = "
    a8 <- list (tuple integral integral)
    char '}'
    return $ Character a1 a2 a3 defaultPicture a4 a5 a6 False a61 a7 a8 []
    <|>
    parens characterparse
    where
        parens p = char '(' *> p <* char ')'

portal :: Monad m => ParserT m Portal
portal = do
    string "Wall {hardness = "
    a1 <- maybeparse integral
    string ", hp = "
    a2 <- maybeparse integral
    char '}'
    (return $ Wall a1 a2)
    <|>do
    string "DoorOpen {hardness = "
    a1 <- maybeparse integral
    string ", hp = "
    a2 <- maybeparse integral
    char '}'
    (return $ DoorOpen a1 a2)
    <|>do
    string "DoorClosed {hardness = "
    a1 <- maybeparse integral
    string ", hp = "
    a2 <- maybeparse integral
    string ", runLockDC = "
    a3 <- maybeparse integral
    char '}'
    (return $ DoorClosed a1 a2 a3)
    <|>string "Wall" *> pure (Wall Nothing Nothing) <|> string "Hall" *> pure Hall <|> string "DoorOpen" *> pure (DoorOpen Nothing Nothing) <|> do
    string "DoorClosed {runLockDC = "
    a1 <- maybeparse integral
    char '}'
    (return $ DoorClosed Nothing Nothing a1)
    <|>
    parens portal
    <|> string "DoorClosed" *> (pure $ DoorClosed Nothing Nothing Nothing)
    where
        parens p = char '(' *> p <* char ')'

illumination :: Monad m => ParserT m VisionCategory
illumination = string "NormalVision" *> pure NormalVision <|> string "Low" *> pure Low <|> string "Dark" *> pure Dark
    <|>
    parens illumination
    where
        parens p = char '(' *> p <* char ')'

vision :: Monad m => ParserT m Vision
vision = do
    string "Vision {runCategory = "
    a1 <- illumination
    string ", runDistance = "
    a2 <- integral
    char '}'
    return $ Vision a1 a2
    <|>
    parens vision
    where
        parens p = char '(' *> p <* char ')'

object :: Monad m => ParserT m Object
object = do
    string "HiddenObject {name = "
    part0 <- takestring
    string ", runPerceptionDC = "
    part1 <- integral
    string ", runContents = "
    part2 <- list (eitherparser (eitherparser (magicitem integral) spelltemplate) (eitherparser (armour integral) weapon))
    char '}'
    return $ HiddenObject part0 part1 part2
    <|>do
    string "Object {name = "
    part1 <- takestring
    string ", runContents = "
    part2 <- list (eitherparser (eitherparser (magicitem integral) spelltemplate) (eitherparser (armour integral) weapon))
    char '}'
    return $ Object part1 part2
    <|>do
    string "HiddenTrap {name = "
    part1 <- takestring
    string ", runPerceptionDC = "
    part2 <- integral
    string ", runDisableDC = "
    part3 <- integral
    string ", runTriggerLocations = "
    part4 <- list (tuple integral integral)
    string ", runspelltrap = "
    part5 <- spell (list integral)
    char '}'
    return $ HiddenTrap part1 part2 part3 part4 part5
    <|>do
    string "Trap {name = "
    part1 <- takestring
    string ", runDisableDC = "
    part3 <- integral
    string ", runTriggerLocations = "
    part4 <- list (tuple integral integral)
    string ", runspelltrap = "
    part5 <- spell (list integral)
    char '}'
    return $ Trap part1 part3 part4 part5
    <|>
    parens object
    where
        parens p = char '(' *> p <* char ')'

mazeblock :: Monad m => ParserT m a -> ParserT m (MazeBlock a)
mazeblock item = do
    string "MazeBlock {runNorth = "
    a1 <- item
    string ", runEast = "
    a2 <- item
    string ", runSouth = "
    a3 <- item
    string ", runWest = "
    a4 <- item
    string ", runObject = "
    a5 <- maybeparse object
    string ", runIllumination = "
    a6 <- illumination
    string ", runLight = "
    a7 <- maybeparse vision
    string ", runspell = "
    a8 <- maybeparse (tuple (spell (list integral)) integral)
    char '}'
    return $ MazeBlock a1 a2 a3 a4 a5 a6 a7 a8
    <|>
    parens (mazeblock item)
    where
        parens p = char '(' *> p <* char ')'

gamemode :: Monad m => ParserT m GameMode
gamemode = string "CombatTargetMode" *> pure CombatTargetMode <|> string "Map" *> pure Map <|> string "TargetMode" *> pure TargetMode 
    <|> do
    string "CastSpell {runIsCrit = "
    a1 <- bool
    char '}'
    return $ CastSpell a1
    <|> string "Combat" *> pure Combat <|> string "SpellTargetMode" *> pure SpellTargetMode 
    <|> string "SingleSpellTargetMode" *> pure SingleSpellTargetMode <|> string "ToggleMode" *> pure ToggleMode <|> string "AttackChoice" *> pure AttackChoice 
    <|> do
    string "EquipmentTargeting {runMaybeLocation = "
    a1 <- maybeparse (tuple integral integral)
    char '}'
    return $ EquipmentTargeting a1
    <|> string "EquipmentTarget" *> pure EquipmentTarget 
    <|>
    parens gamemode
    where
        parens p = char '(' *> p <* char ')'

mazenode :: Monad m => ParserT m MazeNode
mazenode = mazeblock portal
    <|>
    parens mazenode
    where
        parens p = char '(' *> p <* char ')'

mazeparse :: Monad m => ParserT m Maze
mazeparse = do
    char '"'
    listform <- list (tuple (tuple integral integral) (mazeblock portal))
    return $ MAP.fromList listform
    <|>
    parens mazeparse
    where
        parens p = char '(' *> p <* char ')'

backgroundparse :: Monad m => ParserT m [Location]
backgroundparse = do
    char '"'
    listform <- list (tuple integral integral)
    return listform
    <|>
    parens backgroundparse
    where
        parens p = char '(' *> p <* char ')'

load :: Monad m => ParserT m (NonEmpty Character)
load = do
    char '"'
    a1 <- nelist characterparse
    return $ a1

loadcharactersing :: Monad m => ParserT m Character
loadcharactersing = do
    char '"'
    a1 <- characterparse
    return $ a1

characterlist :: Monad m => ParserT m [Character]
characterlist = do
    char '"'
    a1 <- list characterparse
    return $ a1