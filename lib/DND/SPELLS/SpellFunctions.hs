{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module DND.SPELLS.SpellFunctions
where
    
import Control.Lens (Lens', set, over, view)
import Data.Generics.Labels ()
import Data.Maybe (fromJust, isJust)
import System.Random (StdGen)
import Control.Monad (join)

import DND.ATTACKS.Smite (Lawfulness (Chaotic, Lawful), Benificence (Good, Evil))
import DND.ATTACKS.Weapon (Weapon(Weapon), WeaponType (Grenade, SmallArm), FeatProfile (FP), WeaponFeat (Specialization, Proficient), wix)

import DND.CHARACTER.AbilityScores (Ability (Strength, Dexterity, Constitution, Intelligence, Wisdom, Charisma))
import DND.CHARACTER.Class (getLevel)
import DND.CHARACTER.ClassFeatures (SpellLevel)
import DND.CHARACTER.Status (Health (..), Status, SpellTemplate, Health (Dead), Team (Ally, Enemy, Enemy2, Bystander), 
            abilitybonuses, spellresistance, damager, healthupdate, gatherbonuses, emptystatus, effectivespelllevel, damager', gettotalcurrent, resolvepoints)

import DND.DAMAGEDICE.DamageDice (DamageDice (D), SaveProfile (Half, Negates, Special))
import DND.DAMAGEDICE.DamageDiceFunctions (mddstatus)
import DND.DAMAGEDICE.Elemental (Elemental (Fire, Lightning, Positive, Negative, Acid, Ice))


import DND.DEFENSE.Defense (Target (Target), Defense (Reflex, Touch, Will, Fortitude))
import DND.DEFENSE.DefenseFunctions (defense2defensetype, fortitude)

import DND.STATDAMAGE.StatDamageFunctions (statdamage)

import DND.STATUSEFFECTS.StatusEffectFunctions2 (t2ix)
import DND.STATUSEFFECTS.StatusEffectFunctions3 (effect2s2s)
import DND.STATUSEFFECTS.Effect (Effect (Effect))
import DND.STATUSEFFECTS.Effects ((-$-), isActive, (&), settemporal, addtemporal, Temporal (Absent, Present, NA, Off), 
        StatusEffect (..), 
        viewtempmodifyier)

import DND.SPELLS.Spell

import DND.Roll (đ)
import DND.TemporaryValue (temporary, revert)

close :: Int -> Int
close ecl = 25 + (5 * ecl `div` 2)

medium :: Int -> Int
medium ecl = 100 + 10 * ecl

long :: Int -> Int
long ecl = 400 + 40 * ecl

w2sp :: SpellTemplate
w2sp _ _ s = if wt /= Grenade then Spell
    (SpellInfo name 2 Evocation 0 0 Nothing (AllInRange Cone wr))
    emptyspellroll (False,True)
    (Just $ over (#ddtarget) ((fmap $ over (#bonus) (+ (babbonus + dexbonus + tohitbonus + deadlyaimpenalty)))) $ over (#damagebonus) (+ (specdamage + deadlyaimbonus + miscdamagebonus + arcanedamage)) wdd)
    Nothing
    Nothing
    Nothing
    Nothing
    False
    else Spell
    (SpellInfo name 2 Evocation 0 0 Nothing (AllInRange (Sphere wr) 30))
    emptyspellroll (False,True)
    (Just $ over (#ddtarget) ((fmap $ over (#bonus) (+ (level `div` 2 + dexbonus + tohitbonus)))) $ over (#damagebonus) (+ (deadlyaimbonus + miscdamagebonus + arcanedamage)) wdd)
    Nothing
    Nothing
    Nothing
    Nothing
    False
    where
        (Weapon dt wt wdd wr ench name) = view (#primaryhand) s
        babbonus = (temporary $ view (#primclass . #bab) s) + (temporary . sum . view (#miscclass . #bab) . gatherbonuses $ s)
        dexbonus = view (#dexterity) $ abilitybonuses s
        level :: Int
        level = getLevel $ view (#primclass . #level) s
        miscdamagebonus = temporary . sum . view (#damage) $ gatherbonuses s
        featconstant = max 1 ((temporary $ view (#primclass . #bab) s) `div` 2)
        wfp = view (#weaponbonus) s
        FP profile focus = wix wt $ wfp
        tohitbonus = if focus then focusbonus else if profile >= Proficient then 0 else (- 4)
        focusbonus = if (getLevel $ view (#primclass . #level) s) - (temporary $ view (#primclass . #bab) s) > 3 then 2 else 1
        (deadlyaimbonus, deadlyaimpenalty)
            | (isActive . temporary) $ view (#effects . #deadlyaim) s = (featconstant, (-2))
            | otherwise = (0,0)
        arcanedamage
            | (isActive . temporary) $ view (#effects . #arcanestrike) s = max (((temporary $ view (#primclass . #bsb) s) `div` 4) + 1) (((temporary $ view (#primclass . #bhb) s) `div` 4) + 1)
            | otherwise = 0
        specdamage
            | profile == Specialization && wt == SmallArm = (\x -> x `div` 2) . sum $ view (#playerclasses) s
            | profile == Specialization = sum $ view (#playerclasses) s
            | otherwise = 0

applyspell :: Maybe (Spell Int) -> Status -> Status -> Status
applyspell Nothing _ defender = defender
applyspell s caster defender
  | isimmunetomind = defender
  | isimmunetomagic && issp = defender
  | srtotal <= (pure $ spellresistance defender) && issp = defender
  | otherwise = 
    mddstatus (view (#spellbonus) inf + channeldefense + protectedfromdefense) (view (#ddroll) rol) mdam $
    statdamage (view (#spellbonus) inf + protectedfromdefense) (view (#sdroll) rol) mstat $
    effect2s2s (view (#spellbonus) inf + channeldefense + protectedfromdefense) (view (#efroll) rol) (((Just <$>) <$>) <$> meff) $
    go defender
    where
        (Just (Spell inf rol _ mdam mstat meff _ _ issp)) = s
        endregeneration = regenerationendingspell s && (isActive . temporary $ view (#effects . #regeneration) defender)
        go sp
            | endregeneration = t2ix Regeneration (NA Nothing [] Nothing) sp
            | otherwise = sp
        ischannelspell = view (#name) inf == "Command Undead" || view (#name) inf == "Turn Undead" || (take 7 $ view (#name) inf) == "Channel"
        channeldefense
            | ischannelspell && (isActive . temporary $ view (#effects . #channelresistance) defender) = (-4)
            | otherwise = 0
        align = temporary $ view (#alignment) caster
        isprotectedfrom = isActive . temporary $ view (#effects . #protectionfrom) defender
        protectedfromwhat = viewtempmodifyier . temporary $ view (#effects . #protectionfrom) defender
        protectedfromdefense
            | not isprotectedfrom = 0
            | protectedfromwhat == (Just 1) && (view (#benificence) align == Evil) = (-2)
            | protectedfromwhat == (Just 2) && (view (#benificence) align == Good) = (-2)
            | protectedfromwhat == (Just 3) && (view (#lawfullness) align == Lawful) = (-2)
            | protectedfromwhat == (Just 4) && (view (#lawfullness) align == Chaotic) = (-2)
            | otherwise = 0
        isimmunetomagic = (isActive . temporary) (view (#effects . #immunetomagic) defender)
        isimmunetomind = (isActive . temporary) (view (#effects . #immuneMindInfluencingEffects) defender) && view (#school) inf == Enchantment
        srroll = Just $ view (#srroll) rol
        srtotal = (Just $ view (#sRbonus) inf) & srroll

abix :: Maybe Ability -> Status -> Int
abix (Just Strength) s = view (#strength) $ abilitybonuses s
abix (Just Dexterity) s = view (#dexterity) $ abilitybonuses s
abix (Just Constitution) s = view (#constitution) $ abilitybonuses s
abix (Just Intelligence) s = view (#intelligence) $ abilitybonuses s
abix (Just Wisdom) s = view (#wisdom) $ abilitybonuses s
abix (Just Charisma) s = view (#charisma) $ abilitybonuses s
abix Nothing _ = 0

maybeint2int :: Maybe Int -> Int
maybeint2int mi
  | isJust mi = fromJust mi
  | otherwise = 0

miix :: Maybe Int -> Lens' (SpellLevel a) a
miix (Just 0) = #zero
miix (Just 1) = #first
miix (Just 2) = #second
miix (Just 3) = #third
miix (Just 4) = #fourth
miix (Just 5) = #fifth
miix (Just 6) = #sixth
miix (Just 7) = #seventh
miix (Just 8) = #eighth
miix (Just 9) = #ninth
miix _ = #zero

msix :: Maybe Int -> Maybe Int -> Lens' Status [SpellTemplate]
msix (Just 1) mi = (#classfeatures . #mysticspells . #known . (miix mi))
msix (Just 2) mi = (#classfeatures . #arcanespells . #known . (miix mi))
msix (Just 8) _ = #specialabilities
msix (Just 9) _ = #itemspells
msix _ _ = #specialabilities

msix' :: Maybe Int -> Maybe Int -> Lens' Status Int
msix' (Just 1) mi = (#classfeatures . #mysticspells . #perday . (miix mi))
msix' (Just 2) mi = (#classfeatures . #arcanespells . #perday . (miix mi)) 
msix' _ _ = (#classfeatures . #mysticspells . #perday . #zero)

spellsperday :: Maybe Int -> Lens' Status (SpellLevel Int)
spellsperday (Just 1) = (#classfeatures . #mysticspells . #perday)
spellsperday (Just 2) = (#classfeatures . #arcanespells . #perday) 
spellsperday _ = (#classfeatures . #mysticspells . #perday)

maix :: Maybe Int -> Maybe Ability
maix (Just 1) = Just Wisdom
maix (Just 2) = Just Intelligence
maix (Just 3) = Just Charisma
maix (Just 4) = Just Wisdom
maix (Just 5) = Just Intelligence
maix (Just 6) = Just Charisma
maix (Just 7) = Just Charisma
maix _ = Nothing

dispelllist :: [StatusEffect]
dispelllist = [Confused, Invisible, MirrorImage, BearStrength, StoneSkin, Burning, Blind, 
                Fatigued, Exhausted, Shaken, Fear, Sickened, Nauseated, Sleep, 
                Paralyzed, Laughter, Dazed, Despair, Dominated, BeastShape, ElementalBodyIII,
                FormoftheDragon, Aid, Bless, Shield, Blur, Heroism, FireShield, CatsGrace, 
                Held, BullsEndurance, Entangled, BarkSkin, SeeInvisibility, DivineFavour, 
                Commanded, ProtectionFrom, MageArmour, Regeneration, Haste, Slow, Repelled, ImprovedInvisibility] 

removeafflictionlist :: [StatusEffect]
removeafflictionlist = [Confused, Burning, Blind, Fatigued, Exhausted, Shaken, Fear, Sickened, Nauseated, Sleep, 
                Paralyzed, Laughter, Dazed, Despair, Dominated, Held, Entangled, Commanded, Slow, Repelled] 

removeafflictionfucntion :: Status -> Status
removeafflictionfucntion = foldr (\x y -> y . t2ix x (Absent Nothing [] Nothing)) id removeafflictionlist

getspells :: Maybe Int ->  Maybe Int -> Status -> ([Spell [Int]],String)
getspells mi Nothing s = ((\x -> x mi (maix mi) s) <$> (view (msix mi (Just 0)) s), (show $ abix (maix mi) s))
getspells mi mi2 s
    | (mi == (Just 8)) || (mi == (Just 9)) = (filter go $ (((\x -> x mi (maix mi) s)) <$>) (view (msix mi mi2) s), stringinfo)
    | view (msix' mi mi2) s == 0 = ([],"")
    | otherwise = ((((\x -> over (#info . #spellbonus) ( + (fromJust mi2 + spellfocusbonus)) x) . (\x -> x mi (maix mi) s)) <$>) (view (msix mi mi2) s), stringinfo)
    where
        stringinfo = (show . maybeint2int $ mi2) ++ ", " ++ (show spellfocusbonus) ++ ", " ++ (show $ abix (maix mi) s) ++ ", " ++ (show . getLevel $ view (#primclass . #level) s)
        spellfocusbonus = if isActive . temporary $ view (#effects . #spellfocus) s then levelbonus else 0
        levelbonus
            | (getLevel $ view (#primclass . #level) s) >= 17 = 3
            | (getLevel $ view (#primclass . #level) s) >= 11 = 2
            | otherwise = 1
        go :: Spell [Int] -> Bool
        go sp = (view (#info . #name) sp /= "Super Nova" || ((\x -> x >= (Just 3)) . viewtempmodifyier . temporary . view (#effects . #photonmode) $ s)) &&
            (view (#info . #name) sp /= "Black Hole" || ((\x -> x >= (Just 3)) . viewtempmodifyier . temporary . view (#effects . #gravitonmode) $ s))

function2 :: Int -> Int
function2 0 = 0
function2 a = a - 1

castspell :: Maybe Int -> Maybe Int -> Maybe Int -> StdGen -> Status -> Status -> ((Status -> Status), (Status -> Status))
castspell spelltype spelllevel spellchoice g spellcaster victim
    | isAreaTarget maybespelltarget && turningactive = (specialcaster, newvictim)
    | turningactive && issp = (specialcaster . defenderfunction . castspellat, newvictim)
    | srtotal <= (pure $ spellresistance victim) && issp = (specialcaster, id)
    | isimmunetomind = (specialcaster, id)
    | otherwise = (specialcaster, defenderfunction . castspellat)
        where
            castspellat :: Status -> Status
            castspellat s'
                | (isActive . temporary) (view (#effects . #immunetomagic) s') && issp = s'
                | spellchoice !? (fst $ getspells spelltype spelllevel spellcaster) == Nothing = s'
                | otherwise = applyspell spell spellcaster s'
            srroll = (view (#rolls . #srroll) <$>) spell
            srtotal = (view (#info . #sRbonus) <$>) spell & srroll
            spell = (((rollspelldice g) <$>) . (spellchoice !?)) (fst $ getspells spelltype spelllevel spellcaster)
            issp = case spell of
                Nothing -> False
                Just y -> view (#isspell) y
            maybespelltarget = fmap (view (#info . #spelltarget)) spell
            turningactive = isActive temporal
            temporal = temporary (view (#effects . #spellturning) victim)
            turningmodifyer
                    | turningactive = (viewtempmodifyier) temporal
                    | otherwise = Nothing
            newvictim
                    | turningactive && (turningmodifyer == (Just 1)) = over (#effects . #spellturning) (settemporal (Absent Nothing [] Nothing))
                    | turningactive = over (#effects . #spellturning) (settemporal (((\x -> x - 1) <$>) <$> temporal))
                    | otherwise = castspellat
            isenchantmentspell = (view (#info . #school) <$> spell) == (Just Enchantment)
            isimmunetomind = (isActive . temporary) (view (#effects . #immuneMindInfluencingEffects) victim) && isenchantmentspell
            enchantmentspellfails = victim == (castspellat victim)
            (specialcaster, specialdefender) = mstring2s2s (join $ view (#mspecial) <$> spell)
            defenderfunction =  testtarget 
                                (specialdefender    ((view (#rolls . #sdroll)) <$> spell)   (fmap (view (#info . #spellbonus)) spell)) 
                                (fmap (view (#info)) spell) 
                                ((view (#rolls . #ddroll)) <$> spell)

castspell2 :: Maybe (Spell Int) -> Status -> Status -> Status
castspell2 mspell spellcaster victim
    | turningactive && issp = newvictim
    | srtotal <= (pure $ spellresistance victim) && issp = victim
    | isimmunetomind = victim
    | otherwise = defenderfunction castspellat
        where
            castspellat :: Status
            castspellat
                | (isActive . temporary) (view (#effects . #immunetomagic) victim) && issp = victim
                | mspell == Nothing = victim
                | otherwise = applyspell mspell spellcaster victim 
            isenchantmentspell = (view (#info . #school) <$> mspell) == (Just Enchantment)
            isimmunetomind = (isActive . temporary) (view (#effects . #immuneMindInfluencingEffects) victim) && isenchantmentspell           
            issp = case mspell of
                Nothing -> False
                Just y -> view (#isspell) y
            srroll = (view (#rolls . #srroll) <$>) mspell
            srtotal = (view (#info . #sRbonus) <$>) mspell & srroll
            maybespelltarget = fmap (view (#info . #spelltarget)) mspell
            turningactive = isActive temporal
            temporal = temporary (view (#effects . #spellturning) victim)
            turningmodifyer
                    | turningactive = (viewtempmodifyier) temporal
                    | otherwise = Nothing
            newvictim
                    | turningactive && (turningmodifyer == (Just 1)) = over (#effects . #spellturning) (settemporal (Absent Nothing [] Nothing)) victim
                    | turningactive = over (#effects . #spellturning) (settemporal (((\x -> x - 1) <$>) <$> temporal)) victim
                    | otherwise = castspellat
            (specialcaster, specialdefender) = mstring2s2s (join $ view (#mspecial) <$> mspell)
            defenderfunction =  testtarget 
                                (specialdefender    ((view (#rolls . #sdroll)) <$> mspell)   (fmap (view (#info . #spellbonus)) mspell)) 
                                (fmap (view (#info)) mspell) 
                                ((view (#rolls . #ddroll)) <$> mspell)

castAreaspell :: Maybe (Spell Int) -> Status -> Status
castAreaspell mspell victim
    | turningactive && issp = newvictim
    | isimmunetomind = victim
    | otherwise = defenderfunction castspellat
        where
            castspellat :: Status
            castspellat
                | (isActive . temporary . view (#effects . #immunetomagic)) victim && issp = victim
                | otherwise = applyspell mspell emptystatus victim      
            isenchantmentspell = (view (#info . #school) <$> mspell) == (Just Enchantment)
            isimmunetomind = (isActive . temporary) (view (#effects . #immuneMindInfluencingEffects) victim) && isenchantmentspell     
            issp = case mspell of
                Nothing -> False
                Just y -> view (#isspell) y
            turningactive = isActive temporal
            temporal = temporary (view (#effects . #spellturning) victim)
            turningmodifyer
                    | turningactive = (viewtempmodifyier) temporal
                    | otherwise = Nothing
            newvictim
                    | turningactive && (turningmodifyer == (Just 1)) = over (#effects . #spellturning) (settemporal (Absent Nothing [] Nothing)) victim
                    | turningactive = over (#effects . #spellturning) (settemporal (((\x -> x - 1) <$>) <$> temporal)) victim
                    | otherwise = castspellat
            (specialcaster, specialdefender) = mstring2s2s (join $ view (#mspecial) <$> mspell)
            defenderfunction =  testtarget 
                                (specialdefender    ((view (#rolls . #sdroll)) <$> mspell)   (fmap (view (#info . #spellbonus)) mspell)) 
                                (fmap (view (#info)) mspell) 
                                ((view (#rolls . #ddroll)) <$> mspell)

prepareAreaSpell :: Status -> (Spell [Int]) -> (Spell [Int])
prepareAreaSpell caster spell = over (#meffect) (fmap (over (#temporal) (\x -> (-$-) (const (Just 1)) x))) $ spell
    where
        Spell inf rol _ mdam mstat meff _ _  _ = spell
        srroll = view (#srroll) rol

(!?) :: Maybe Int -> [a] -> Maybe a
(!?) Nothing _ = Nothing
(!?) (Just i) as
    | i < 0 = Nothing
    | otherwise = go as i
      where
        go :: [a] -> Int -> Maybe a
        go (x:_) 0 = Just x
        go (_:ys) j = go ys (j -1)
        go [] _ = Nothing

mstring2s2s :: Maybe String -> (Status -> Status, Maybe Int -> Maybe Int -> Status -> Status)
mstring2s2s Nothing = (id, (\_ _ -> id))
mstring2s2s (Just spellwords)
    | spellwords == "Restoration" = restorationfunction
    | spellwords == "Greater Restoration" = greaterrestorationfunction
    | spellwords == "Stabilize" = stabilizefunction
    | spellwords == "Phantasmal Killer" = (id, phantasmalkillerfunction)
    | spellwords == "Super Nova" = (supernovafunction, (\_ _ -> id))
    | spellwords == "Black Hole" = (blackholefunction, (\_ _ -> id))
    | spellwords == "RPCost" = (rpcostfunction, (\_ _ -> id))
    | spellwords == "Remove Affliction" = (id, (\ _ _ -> removeafflictionfucntion))
    | spellwords == "Slow Poison" = (id, (\ _ _ -> spfunction))
    | spellwords == "Disintegrate" = (id, (\ _ _ -> disintegratefunction))
    | otherwise = (id, (\_ _ -> id))

testtarget :: (Status -> Status) -> Maybe SpellInfo -> Maybe Int -> Status -> Status
testtarget _ Nothing _ s = s
testtarget f (Just info) mi2 s
    | view (#target) info == Nothing = f s
    | mi2 == (Just 20) = f s
    | mi2 == (Just 1) = s
    | (mi2 & (Just $ view (#spellbonus) info)) > (defense2defensetype (view (#defensetype) (fromJust (view (#target) info))) s) = f s
    | otherwise = s

noncombatspell :: String -> SpellTemplate
noncombatspell string _ _ _ = set (#info . #name) string testspell

rpcostfunction :: (Status -> Status)
rpcostfunction = damager' (#resolve) (Just 1)

spfunction :: (Status -> Status)
spfunction = set (#statdamage) []

disintegratefunction :: Status -> Status
disintegratefunction s = if gettotalcurrent s <= 0 then (set (#health) Dead . healthupdate $ damager' (#resolve) (Just . fst $ resolvepoints s) s) else s

supernovafunction :: (Status -> Status)
supernovafunction = t2ix PhotonMode (Off Nothing [] Nothing)

blackholefunction :: (Status -> Status)
blackholefunction = t2ix GravitonMode (Off Nothing [] Nothing)

stabilizefunction :: (Status -> Status, Maybe Int -> Maybe Int -> Status -> Status)
stabilizefunction = (id, stable)
    where
        stable _ _ s' = over (#effects . #bleeding) (revert) $ set (#health) Stabelized s'

restorationfunction :: (Status -> Status, Maybe Int -> Maybe Int -> Status -> Status)
restorationfunction = (id, restore)
    where
        restore _ _ s' = over (#primclass . #level) revert $ over (#abilityscores) (fmap revert) s'

greaterrestorationfunction :: (Status -> Status, Maybe Int -> Maybe Int -> Status -> Status)
greaterrestorationfunction = (exhausted, restore)
    where
        restore _ _ s' = over (#damage) revert $ over (#primclass . #level) revert $ over (#abilityscores) (fmap revert) s'
        exhausted s' = over (#effects . #exhausted) (addtemporal $ Present Nothing [] Nothing) s'

phantasmalkillerfunction :: Maybe Int -> Maybe Int -> Status -> Status
phantasmalkillerfunction mi1 mi2 s
    | mi1 == (Just 20) = set (#health) Dead $ damager (Just 1000) s
    | mi1 == (Just 1) = healthupdate $ damager (Just 6) s
    | (mi1 & mi2) > (fortitude s) = set (#health) Dead $ damager (Just 1000) s
    | otherwise = healthupdate $ damager (Just 6) s
    
phantasmalkiller :: SpellTemplate
phantasmalkiller mi a s = Spell (SpellInfo ("Phantasmal Killer") 2 Illusion go1 0 (Just $ Target Will 0) (SingleTarget (medium level))) emptyspellroll (False,False)
                    Nothing Nothing Nothing Nothing (Just "Phantasmal Killer") True
                    where
                        go1 = abix a s
                        level = effectivespelllevel mi s

restoration :: SpellTemplate
restoration _ _ _ = Spell (SpellInfo ("Restoration") 2 Abjuration 0 0 Nothing (SingleTarget 9)) emptyspellroll (False,False)
                    Nothing Nothing Nothing Nothing (Just "Restoration") True

slowpoison :: SpellTemplate
slowpoison _ _ _ = Spell (SpellInfo ("Slow Poison") 2 Abjuration 0 0 Nothing (SingleTarget 9)) emptyspellroll (False,False)
                    Nothing Nothing Nothing Nothing (Just "Slow Posion") True

stabilize :: SpellTemplate
stabilize _ _ _ = Spell (SpellInfo ("Stabilize") 2 Abjuration 0 0 Nothing (SingleTarget 9)) emptyspellroll (False,False)
                    Nothing Nothing Nothing Nothing (Just "Stabilize") True

removeaffliction :: SpellTemplate
removeaffliction mi _ s = Spell (SpellInfo ("Remove Affliction") 2 Abjuration 0 0 Nothing (SingleTarget $ close level)) emptyspellroll (False,False)
                    Nothing Nothing Nothing Nothing (Just "Remove Affliction") True
                where
                    level = effectivespelllevel mi s

dispellmagic :: SpellTemplate
dispellmagic mi _ s = Spell (SpellInfo ("Dispel Magic") 2 Abjuration 0 0 Nothing (AllInRange (Sphere 30) (medium level))) emptyspellroll (False,False)
                    Nothing Nothing Nothing Nothing (Just "Dispel Magic") True
                where
                    level = effectivespelllevel mi s

wallosteel :: SpellTemplate
wallosteel mi _ s = Spell (SpellInfo  "Wall of Steel" 2 Evocation 0 0 Nothing (TargetinginRange (medium level))) emptyspellroll (False,False)
                    Nothing Nothing Nothing Nothing Nothing True
                where
                    level = effectivespelllevel mi s

greaterrestoration :: SpellTemplate
greaterrestoration _ _ _ = Spell (SpellInfo ("Greater Restoration") 2 Abjuration 0 0 Nothing (SingleTarget 9)) emptyspellroll (False,False)
                            Nothing Nothing Nothing Nothing (Just "Greater Restoration") True

shield :: SpellTemplate
shield mi _ s = Spell (SpellInfo ("Shield") 2 Abjuration 0 0 Nothing Caster) emptyspellroll (False,False)
                Nothing Nothing (Just $ Effect Shield (Present go [] [0]) Nothing Nothing) Nothing Nothing True
                where
                    go = Just $ level * 10
                    level = effectivespelllevel mi s

magearmour :: SpellTemplate
magearmour mi _ s = Spell (SpellInfo ("Mage Armour") 2 Abjuration 0 0 Nothing Caster) emptyspellroll (False,False)
                Nothing Nothing (Just $ Effect MageArmour (Present go [] [0]) Nothing Nothing) Nothing Nothing True
                where
                    go = Just $ level * 10
                    level = effectivespelllevel mi s

shieldoffaith :: SpellTemplate
shieldoffaith mi _ s = Spell (SpellInfo ("Shield of Faith") 2 Abjuration 0 0 Nothing Caster) emptyspellroll (False,False)
                Nothing Nothing (Just $ Effect ShieldofFaith (Present go [] [0]) go2 Nothing) Nothing Nothing True
                where
                    go = Just $ level * 10
                    go2 = (Just 2) & Just (level `div` 6)
                    level = effectivespelllevel mi s

shieldoffaithpotion :: Int -> SpellTemplate
shieldoffaithpotion int _ _ _ = Spell (SpellInfo ("Shield of Faith Potion " ++ show int) 2 Abjuration 0 0 Nothing Caster) emptyspellroll (False,False)
                Nothing Nothing (Just $ Effect ShieldofFaith (Present (Just 10) [] [0]) (Just int) Nothing) Nothing Nothing False

truestrike :: SpellTemplate
truestrike mi _ s = Spell (SpellInfo ("True Strike") 2 Divination 0 0 Nothing Caster) emptyspellroll (False,False)
                Nothing Nothing (Just $ Effect TrueStrike (Present go [] [0]) Nothing Nothing) Nothing Nothing True
                where
                    go = Just $ level * 10
                    level = effectivespelllevel mi s

fireball :: SpellTemplate
fireball mi a s = Spell
    (SpellInfo  "Fireball" 2 Evocation go1 0 Nothing (AllInRange (Sphere 20) (close level)))
    emptyspellroll (False,False)
    (Just $ D (go) 0 (Just Fire) (Just $ Target Reflex 0) Half)
    Nothing
    Nothing
    Nothing
    Nothing
    True
        where
            go1 = abix a s
            go = (min level 10) `đ` 6
            level = effectivespelllevel mi s

destruction :: SpellTemplate
destruction mi a s = Spell
    (SpellInfo  "Destruction" 2 Evocation go1 0 Nothing (SingleTarget (close level)))
    emptyspellroll (False,False)
    (Just $ D [0] (level * 10) Nothing (Just $ Target Fortitude 0) (Special 35))
    Nothing
    Nothing
    Nothing
    (Just "Disintegrate")
    True
        where
            go1 = abix a s
            level = effectivespelllevel mi s

glowofhealth :: SpellTemplate
glowofhealth _ _ s = Spell
    (SpellInfo  "Glow of Health" 1 Evocation 0 0 Nothing Caster)
    emptyspellroll (True,False)
    (Just $ D [0] go (Just Positive) Nothing Half)
    Nothing
    Nothing
    Nothing
    (Just "RPCost")
    False
        where
            go = if (isActive . temporary $ view (#effects . #photonmode) s) then 3 * level else 2 * level
            level = view (#playerclasses . #solar) s

solarfurnace :: SpellTemplate
solarfurnace _ _ s = Spell
    (SpellInfo  "Solar Furnace" 1 Evocation 0 0 Nothing Caster)
    emptyspellroll (True,False)
    Nothing
    Nothing
    (Just $ Effect Poisened (Absent Nothing [] [0]) Nothing Nothing)
    Nothing
    (Just "RPCost")
    False

supernova :: SpellTemplate
supernova _ _ s = Spell
    (SpellInfo  "Super Nova" 2 Evocation go1 0 Nothing (AllInRange Bursty go2))
    emptyspellroll (True,False)
    (Just $ D (go) 0 (Just Fire) (Just $ Target Reflex 0) Half)
    Nothing
    Nothing
    Nothing
    (Just "Super Nova")
    False
        where
            go1 = abix (Just Charisma) s + level `div` 2
            go2
                | level >= 17 = 20
                | level >= 9 = 15
                | otherwise = 10
            go = (level + 1) `đ` 6
            level = view (#playerclasses . #solar) s

blackhole :: SpellTemplate
blackhole _ _ s = Spell
    (SpellInfo  "Black Hole" 2 Evocation go1 0 (Just $ Target Fortitude 0) (TargetinginRange go2))
    emptyspellroll (True,False)
    Nothing
    Nothing
    Nothing
    Nothing
    (Just "Black Hole")
    False
        where
            go1 = abix (Just Charisma) s + level `div` 2
            go2
                | level >= 20 = 40
                | level >= 15 = 35
                | level >= 10 = 30
                | level >= 5 = 25
                | otherwise = 20
            level = view (#playerclasses . #solar) s

fireballwand :: SpellTemplate
fireballwand _ _ _ = Spell
    (SpellInfo  "Fireball (Wand)" 2 Evocation go1 0 Nothing (AllInRange (Sphere 20) (close 0)))
    emptyspellroll (False,False)
    (Just $ D (go) 0 (Just Fire) (Just $ Target Reflex 0) Half)
    Nothing
    Nothing
    Nothing
    Nothing
    True
        where
            go1 = 5
            go = 5 `đ` 6

coneofcoldwand :: Int -> Int -> SpellTemplate
coneofcoldwand i1 i2 _ _ _ = Spell
    (SpellInfo  ("Cone of Cold (Wand) " ++ show i1 ++ " " ++ show i2) 2 Evocation go1 0 Nothing (AllInRange Cone 60))
    emptyspellroll (False,False)
    (Just $ D (go) 0 (Just Ice) (Just $ Target Reflex 0) Half)
    Nothing
    Nothing
    Nothing
    Nothing
    True
        where
            go1 = i2
            go = i1 `đ` 6

fireballaura :: SpellTemplate
fireballaura _ _ _ = Spell
    (SpellInfo  "Fire Aura" 2 Evocation go1 0 Nothing (AllInRange Bursty 10))
    emptyspellroll (False,False)
    (Just $ D (go) 0 (Just Fire) (Just $ Target Reflex 0) Half)
    Nothing
    Nothing
    Nothing
    Nothing
    True
        where
            go1 = 5
            go = 5 `đ` 6

fallpittrap :: SpellTemplate
fallpittrap _ _ _ = Spell
    (SpellInfo  "Fall" 2 Evocation go1 0 Nothing (AllInRange (Sphere 7) (close 0)))
    emptyspellroll (False,False)
    (Just $ D (go) 0 Nothing (Just $ Target Reflex 0) Half)
    Nothing
    Nothing
    Nothing
    Nothing
    False
        where
            go1 = 10
            go = 2 `đ` 6

grenade :: SpellTemplate
grenade _ _ _ = Spell
    (SpellInfo  "Grenade" 2 Evocation go1 0 Nothing (AllInRange (Sphere 10) (close 0)))
    emptyspellroll (False,False)
    (Just $ D (go) 0 (Just Fire) (Just $ Target Reflex 0) Half)
    Nothing
    Nothing
    Nothing
    Nothing
    False
        where
            go1 = 2
            go = 2 `đ` 6

divinegrenade :: SpellTemplate
divinegrenade _ _ _ = Spell
    (SpellInfo  "Divine Grenade" 2 Evocation go1 0 Nothing (AllInRange (Sphere 10) (close 0)))
    emptyspellroll (False,False)
    (Just $ D (go) 0 (Just Fire) (Just $ Target Reflex 0) Half)
    Nothing
    (Just $ Effect Fear (Present (Just 4) [] [0]) Nothing (Just $ Target Will 3))
    Nothing
    Nothing
    False
        where
            go1 = 4 
            go = 4 `đ` 6

magicmissile :: SpellTemplate
magicmissile mi _ s = Spell
    (SpellInfo  "Magic Missile" 2 Evocation 0 0 Nothing (SingleTarget (medium level)))
    emptyspellroll (False,False)
    (Just $ D (go) go1 Nothing Nothing Half)
    Nothing
    Nothing
    Nothing
    Nothing
    True
        where
            go = (min level 5) `đ` 4
            go1 = (min level 5)
            level = effectivespelllevel mi s

shockinggrasp :: SpellTemplate
shockinggrasp mi _ s = Spell
    (SpellInfo  "Shocking Grasp" 2 Evocation go1 0 Nothing (SingleTarget 9))
    emptyspellroll (True,False)
    (Just $ D (go) 0 (Just Fire) (Just $ Target Touch 3) Negates)
    Nothing
    Nothing
    Nothing
    Nothing
    True
        where
            go1 = view (#dexterity) (abilitybonuses s) + (temporary $ view (#primclass . #bab) s) + (temporary . sum . view (#miscclass . #bab) . gatherbonuses $ s)
            go = (min level 5) `đ` 6
            level = effectivespelllevel mi s

dragongland :: SpellTemplate
dragongland mi _ s = Spell
    (SpellInfo  "Dragon Gland" 2 Evocation go1 0 Nothing (AllInRange Cone 15))
    emptyspellroll (True,False)
    (Just $ D (go) 0 (Just Acid) (Just $ Target Reflex 0) Half)
    Nothing
    Nothing
    Nothing
    Nothing
    False
        where
            go1 = abix (Just Constitution) s + (level `div` 2)
            go = level `đ` 6
            level = sum $ view (#playerclasses) s

burninghands :: SpellTemplate
burninghands mi a s = Spell
    (SpellInfo  "Burning Hands" 2 Evocation go1 0 Nothing (AllInRange Cone 15))
    emptyspellroll (False,False)
    (Just $ D (go) 0 (Just Fire) (Just $ Target Reflex 0) Half)
    Nothing
    Nothing
    Nothing
    Nothing
    True
        where
            go1 = abix a s
            go = (min level 5) `đ` 4
            level = effectivespelllevel mi s

burninghandswand :: SpellTemplate
burninghandswand _ _ _ = Spell
    (SpellInfo  "Burning Hands Wand" 2 Evocation 3 0 Nothing (AllInRange Cone 15))
    emptyspellroll (False,False)
    (Just $ D [4,4,4] 0 (Just Fire) (Just $ Target Reflex 0) Half)
    Nothing
    Nothing
    Nothing
    Nothing
    True

blur :: SpellTemplate
blur mi _ s = Spell (SpellInfo ("Blur") 2 Illusion 0 0 Nothing (SingleTarget 9)) emptyspellroll (False,False)
                Nothing Nothing (Just $ Effect Blur (Present go [] [0]) Nothing Nothing) Nothing Nothing True
                where
                    go = Just $ level * 10
                    level = effectivespelllevel mi s

blurpotion :: SpellTemplate
blurpotion _ _ _ = Spell (SpellInfo ("Blur") 2 Illusion 0 0 Nothing (SingleTarget 9)) emptyspellroll (False,False)
                Nothing Nothing (Just $ Effect Blur (Present go [] [0]) Nothing Nothing) Nothing Nothing True
                where
                    go = Just 10

invisibility :: SpellTemplate
invisibility mi _ s = Spell (SpellInfo ("Invisibility") 2 Illusion 0 0 Nothing (SingleTarget 9)) emptyspellroll (False,False)
                Nothing Nothing (Just $ Effect Invisible (Present go [] [0]) Nothing Nothing) Nothing Nothing True
                where
                    go = Just $ level * 10
                    level = effectivespelllevel mi s

improvedinvisibility :: SpellTemplate
improvedinvisibility mi _ s = Spell (SpellInfo ("Improved Invisibility") 2 Illusion 0 0 Nothing (SingleTarget 9)) emptyspellroll (False,False)
                Nothing Nothing (Just $ Effect ImprovedInvisibility (Present go [] [0]) Nothing Nothing) Nothing Nothing True
                where
                    go = Just $ level * 10
                    level = effectivespelllevel mi s

keensenses :: SpellTemplate
keensenses mi _ s = Spell (SpellInfo ("Keen Senses") 2 Illusion 0 0 Nothing (SingleTarget 9)) emptyspellroll (False,False)
                Nothing Nothing (Just $ Effect KeenSenses (Present go [] [0]) Nothing Nothing) Nothing Nothing True
                where
                    go = Just $ level * 10
                    level = effectivespelllevel mi s

protectionfrom :: String -> SpellTemplate
protectionfrom string mi _ s = Spell (SpellInfo ("Protection from " ++ string) 2 Abjuration 0 0 Nothing (SingleTarget 9)) emptyspellroll (False,False)
                Nothing Nothing (Just $ Effect ProtectionFrom (Present go [] [0]) go2 Nothing) Nothing Nothing True
                where
                    go = Just $ level * 10
                    level = effectivespelllevel mi s
                    go2
                        | string == "Evil" = Just 1
                        | string == "Good" = Just 2
                        | string == "Lawful" = Just 3
                        | string == "Chaotic" = Just 4
                        | otherwise = Just 1

seeinvisibility :: SpellTemplate
seeinvisibility mi _ s = Spell (SpellInfo ("See Invisibility") 2 Divination 0 0 Nothing Caster) emptyspellroll (False,False)
                Nothing Nothing (Just $ Effect SeeInvisibility (Present go [] [0]) Nothing Nothing) Nothing Nothing True
                    where
                        go = Just $ level * 100
                        level = effectivespelllevel mi s

holographicclone :: SpellTemplate
holographicclone mi _ s = Spell (SpellInfo ("Holographic Clone") 2 Illusion 0 0 Nothing Caster) emptyspellroll (True,False)
                Nothing Nothing (Just $ Effect MirrorImage (Present go [] [rolledimages]) Nothing Nothing) Nothing Nothing False
                where
                    go = Just $ level * 10
                    level = sum $ view (#playerclasses) s
                    rolledimages = 4

mirrorimage :: SpellTemplate
mirrorimage mi _ s = Spell (SpellInfo ("Mirror Image") 2 Illusion 0 0 Nothing Caster) emptyspellroll (False,False)
                Nothing Nothing (Just $ Effect MirrorImage (Present go [] [rolledimages]) bonusimages Nothing) Nothing Nothing True
                where
                    go = Just $ level * 10
                    level = effectivespelllevel mi s
                    bonusimages = Just $ min 8 (level `div` 3)
                    rolledimages = min 4 (8 - (min 8 (level `div` 3)))

lightningbolt :: SpellTemplate
lightningbolt mi a s = Spell
    (SpellInfo  "Lightning Bolt" 2 Evocation go1 0 Nothing (AllInRange Line 120))
    emptyspellroll (False,False)
    (Just $ D (go) 0 (Just Lightning) (Just $ Target Reflex 0) Half)
    Nothing
    Nothing
    Nothing
    Nothing
    True
        where
            go1 = abix a s
            go = (min level 10) `đ` 6
            level = effectivespelllevel mi s

scorchingray :: SpellTemplate
scorchingray mi _ s = Spell
    (SpellInfo  "Scorching Ray" 2 Evocation go1 0 Nothing (SingleTarget (close level)))
    emptyspellroll (False,False)
    (Just $ D (go) 0 (Just Fire) (Just $ Target Touch 0) Negates)
    Nothing
    Nothing
    Nothing
    Nothing
    True
        where
            go1 = view (#dexterity) (abilitybonuses s) + (temporary $ view (#primclass . #bab) s) + (temporary . sum . view (#miscclass . #bab) . gatherbonuses $ s)
            go2 sorclevel
                | sorclevel == 11 = 3
                | sorclevel == 7 = 2
                | otherwise = 1
            go = (4 * (go2 level)) `đ` 4
            level = effectivespelllevel mi s

heroism :: SpellTemplate
heroism mi _ s = Spell (SpellInfo ("Herosim") 2 Transmutation 0 0 Nothing (SingleTarget 9)) emptyspellroll (False,False)
                Nothing Nothing (Just $ Effect Heroism (Present go [] [0]) Nothing Nothing) Nothing Nothing True
                where
                    go = Just $ level * 100
                    level = effectivespelllevel mi s

repulsionspell :: SpellTemplate
repulsionspell mi _ s = Spell (SpellInfo ("Repulsion") 2 Transmutation 0 0 Nothing (EnemiesinRange Bursty (5 * level))) emptyspellroll (False,False)
                Nothing Nothing (Just $ Effect Repelled (Present go [] [0]) (Just (5 * level)) (Just $ Target Will 0)) Nothing Nothing True
                where
                    go = Just $ level
                    level = effectivespelllevel mi s

fireshield :: SpellTemplate
fireshield mi _ s = Spell (SpellInfo ("Fire Shield") 2 Evocation 0 0 Nothing Caster) emptyspellroll (False,False)
                Nothing Nothing (Just $ Effect FireShield (Present go [] [0]) Nothing Nothing) Nothing Nothing True
                where
                    go = Just $ level
                    level = effectivespelllevel mi s

dominateperson :: SpellTemplate
dominateperson mi a s = Spell (SpellInfo ("Dominate Person") 2 Enchantment go1 0 Nothing (SingleTarget 9)) emptyspellroll (True,False)
                Nothing Nothing (Just $ Effect Dominated (Present go [] [0]) go2 (Just $ Target Will 0)) Nothing Nothing True
                where
                    go1 = abix a s
                    go = Just $ level
                    level = effectivespelllevel mi s
                    go2
                        | temporary (view (#team) s) == Ally = Just 1
                        | temporary (view (#team) s) == Enemy = Just 2
                        | temporary (view (#team) s) == Enemy2 = Just 3
                        | temporary (view (#team) s) == Bystander = Just 4
                        | otherwise = Just 4

commandundead :: SpellTemplate
commandundead mi a s = Spell (SpellInfo ("Command Undead") 2 Enchantment go1 0 Nothing (SingleTarget 9)) emptyspellroll (True,False)
                Nothing Nothing (Just $ Effect Commanded (Present go [] [0]) go2 (Just $ Target Will 0)) Nothing Nothing True
                where
                    go1 = abix a s
                    go = Just $ level
                    level = effectivespelllevel mi s
                    go2
                        | temporary (view (#team) s) == Ally = Just 1
                        | temporary (view (#team) s) == Enemy = Just 2
                        | temporary (view (#team) s) == Enemy2 = Just 3
                        | temporary (view (#team) s) == Bystander = Just 4
                        | otherwise = Just 4

charmanimal :: SpellTemplate
charmanimal mi a s = Spell (SpellInfo ("Charm Animal") 2 Enchantment go1 0 Nothing (SingleTarget 9)) emptyspellroll (True,False)
                Nothing Nothing (Just $ Effect Dominated (Present go [] [0]) go2 (Just $ Target Will 0)) Nothing Nothing True
                where
                    go1 = abix a s
                    go = Just $ level * 10
                    level = effectivespelllevel mi s
                    go2
                        | temporary (view (#team) s) == Ally = Just 1
                        | temporary (view (#team) s) == Enemy = Just 2
                        | temporary (view (#team) s) == Enemy2 = Just 3
                        | temporary (view (#team) s) == Bystander = Just 4
                        | otherwise = Just 4

hideouslaughter :: SpellTemplate
hideouslaughter mi a s =  Spell (SpellInfo "Hideous Laughter" 2 Enchantment go1 0 Nothing (SingleTarget (close level))) emptyspellroll (False,False)
    Nothing Nothing (Just $ Effect Laughter (Present go [] [0]) Nothing (Just $ Target Will 0)) Nothing Nothing True
    where
        go1 = abix a s
        go = Just $ level
        level = effectivespelllevel mi s

confusion :: SpellTemplate
confusion mi a s =  Spell (SpellInfo "Confusion" 2 Enchantment go1 0 Nothing (AllInRange Bursty 15)) emptyspellroll (False,False)
    Nothing Nothing (Just $ Effect Confused (Present go [] [0]) Nothing (Just $ Target Will 0)) Nothing Nothing True
    where
        go1 = abix a s
        go = Just $ level
        level = effectivespelllevel mi s

hold :: SpellTemplate
hold mi a s =  Spell (SpellInfo "Hold Person" 2 Transmutation go1 0 Nothing (SingleTarget (medium level))) emptyspellroll (False,False)
    Nothing Nothing (Just $ Effect Held (Present go [] [0]) Nothing (Just $ Target Will 0)) Nothing Nothing True
    where
        go1 = abix a s
        go = Just $ level
        level = effectivespelllevel mi s

causefear :: SpellTemplate
causefear mi a s =  Spell (SpellInfo "Cause Fear" 2 Enchantment go1 0 Nothing (SingleTarget (close level))) emptyspellroll (False,False)
    Nothing Nothing (Just $ Effect Fear (Present go [] [0]) Nothing (Just $ Target Will 0)) Nothing Nothing True
    where
        go1 = abix a s
        go = Just 2
        level = effectivespelllevel mi s

doom :: SpellTemplate
doom mi a s =  Spell (SpellInfo "Doom" 2 Enchantment go1 0 Nothing (SingleTarget (medium level))) emptyspellroll (False,False)
    Nothing Nothing (Just $ Effect Shaken (Present go [] [0]) Nothing (Just $ Target Will 0)) Nothing Nothing True
    where
        go1 = abix a s
        go = Just $ level * 10
        level = effectivespelllevel mi s

curewounds :: Int -> SpellTemplate
curewounds i mi _ s = Spell (SpellInfo ("Mystic Cure " ++ show i) 2 Evocation go3 0 Nothing (SingleTarget (close go2))) emptyspellroll (False,False)
    (Just $ D (go1) go2 (Just Positive) (Just $ Target Touch 0) Negates) Nothing Nothing Nothing Nothing True
    where
        go1 
            | i == 6 = 20 `đ` 8
            | i == 5 = 16 `đ` 8
            | i == 4 = 12 `đ` 8
            |otherwise = ((i + 1) `div` 2) `đ` 8
        go2 = effectivespelllevel mi s
        go3 = view (#dexterity) (abilitybonuses s) + (temporary $ view (#primclass . #bab) s) + (temporary . sum . view (#miscclass . #bab) . gatherbonuses $ s)

mindthrust :: Int -> SpellTemplate
mindthrust i mi a s = Spell (SpellInfo ("Mind Thrust " ++ show i) 2 Enchantment (abix a s) 0 Nothing (SingleTarget (close go2))) emptyspellroll (False,False)
    (Just $ D (go1) go2 Nothing (Just $ Target Will 0) Half) Nothing Nothing Nothing Nothing True
    where
        go1
            | i == 1 = 2 `đ` 10
            | i == 2 = 4 `đ` 10
            | i == 3 = 7 `đ` 10
            | i == 4 = 10 `đ` 10
            | i == 5 = 15 `đ` 10
            | i == 6 = 17 `đ` 10
            | otherwise = 17 `đ` 10
        go2 = effectivespelllevel mi s

babcheck :: Status -> Int
babcheck s = (temporary $ view (#primclass . #bab) s)

masscurewounds :: Int -> SpellTemplate
masscurewounds i mi _ s = Spell (SpellInfo ("Mass Cure " ++ go ++ " Wounds") 2 Evocation go3 0 Nothing (Allies (close go2))) emptyspellroll (False,False)
    (Just $ D (go1) go2 (Just Positive) (Just $ Target Touch 0) Negates) Nothing Nothing Nothing Nothing True
    where
        go
            | i == 1 = "Light"
            | i == 2 = "Moderate"
            | i == 3 = "Serious"
            | i == 4 = "Critical"
            | otherwise = "Light"
        go1
            | i == 1 = [8]
            | i == 2 = [8,8]
            | i == 3 = [8,8,8]
            | i == 4 = [8,8,8,8]
            | otherwise = [8]
        go2 = effectivespelllevel mi s
        go3 = view (#dexterity) (abilitybonuses s) + (temporary $ view (#primclass . #bab) s) + (temporary . sum . view (#miscclass . #bab) . gatherbonuses $ s)

catsgrace :: SpellTemplate
catsgrace mi _ s = Spell (SpellInfo ("Cats Grace") 2 Transmutation 0 0 Nothing (SingleTarget (close level))) emptyspellroll (False,False)
                Nothing Nothing (Just $ Effect CatsGrace (Present go [] [0]) Nothing Nothing) Nothing Nothing True
                where
                    go = Just $ level * 10
                    level = effectivespelllevel mi s

bullsendurance :: SpellTemplate
bullsendurance mi _ s = Spell (SpellInfo ("Bulls Endurance") 2 Transmutation 0 0 Nothing (SingleTarget (close level))) emptyspellroll (False,False)
                Nothing Nothing (Just $ Effect BullsEndurance (Present go [] [0]) Nothing Nothing) Nothing Nothing True
                where
                    go = Just $ level * 10
                    level = effectivespelllevel mi s

bearsstrength :: SpellTemplate
bearsstrength mi _ s = Spell (SpellInfo ("Bears Strength") 2 Transmutation 0 0 Nothing (SingleTarget (close level))) emptyspellroll (False,False)
                Nothing Nothing (Just $ Effect BearStrength (Present go [] [0]) Nothing Nothing) Nothing Nothing True
                where
                    go = Just $ level * 10
                    level = effectivespelllevel mi s

bullsendurancemass :: SpellTemplate
bullsendurancemass mi _ s = Spell (SpellInfo ("Mass Bulls Endurance") 2 Transmutation 0 0 Nothing (Allies (close level))) emptyspellroll (False,False)
                Nothing Nothing (Just $ Effect BullsEndurance (Present go [] [0]) Nothing Nothing) Nothing Nothing True
                where
                    go = Just $ level * 10
                    level = effectivespelllevel mi s

entangle :: SpellTemplate
entangle mi a s = Spell (SpellInfo ("Entangle") 2 Conjuration go1 0 Nothing (MapArea 40 (long level))) emptyspellroll (False,False)
                Nothing Nothing (Just $ Effect Entangled (Present go [] [0]) Nothing (Just $ Target Reflex 0)) Nothing Nothing True
                where
                    go = Just $ level * 10
                    level = effectivespelllevel mi s
                    go1 = abix a s

entangletrap :: SpellTemplate
entangletrap mi a s = Spell (SpellInfo ("Entangle") 2 Conjuration 5 0 Nothing (AllInRange (Sphere 10) 40)) emptyspellroll (False,False)
                Nothing Nothing (Just $ Effect Entangled (Present (Just 10) [] [0]) Nothing (Just $ Target Reflex 0)) Nothing Nothing False

flashpowder :: SpellTemplate
flashpowder mi a s = Spell (SpellInfo ("Flash Power") 2 Conjuration 5 0 Nothing (AllInRange (Sphere 10) 20)) emptyspellroll (False,False)
                Nothing Nothing (Just $ Effect Blind (Present (Just 0) [] [4]) Nothing (Just $ Target Reflex 0)) Nothing Nothing False

tnaglefootbag :: SpellTemplate
tnaglefootbag mi a s = Spell (SpellInfo ("Tangle Foot Bag") 2 Conjuration 5 0 Nothing (SingleTarget 20)) emptyspellroll (False,False)
                Nothing Nothing (Just $ Effect Entangled (Present (Just 0) [] [4]) Nothing (Just $ Target Reflex 0)) Nothing Nothing False

barkskin :: SpellTemplate
barkskin mi _ s = Spell (SpellInfo ("Bark Skin") 2 Transmutation 0 0 Nothing (SingleTarget 9)) emptyspellroll (False,False)
                Nothing Nothing (Just $ Effect BarkSkin (Present go [] [0]) go2 Nothing) Nothing Nothing True
                where
                    go = Just $ level * 100
                    level = effectivespelllevel mi s
                    go2 = Just $ min 5 (2 + ((level - 3) `div` 3))

flamestrike :: SpellTemplate
flamestrike mi a s = Spell
    (SpellInfo  "Flame Strike" 2 Evocation go1 0 Nothing (SingleTarget (medium level)))
    emptyspellroll (False,False)
    (Just $ D (go) 0 (Just Fire) (Just $ Target Reflex 0) Half)
    Nothing
    Nothing
    Nothing
    Nothing
    True
        where
            go1 = abix a s
            go = (min level 15) `đ` 6
            level = effectivespelllevel mi s

balefullpolymorth :: SpellTemplate
balefullpolymorth mi a s = Spell (SpellInfo ("Baleful Polymorth") 2 Transmutation go1 0 Nothing (SingleTarget (close level))) emptyspellroll (False,False)
                Nothing Nothing (Just $ Effect Polymorth (Present Nothing [] [0]) Nothing (Just $ Target Fortitude 0)) Nothing Nothing True
                where
                    go1 = abix a s
                    level = effectivespelllevel mi s

bless :: SpellTemplate
bless mi _ s = Spell (SpellInfo ("Bless") 2 Abjuration 0 0 Nothing (Allies 50)) emptyspellroll (False,False)
                Nothing Nothing (Just $ Effect Bless (Present go [] [0]) Nothing Nothing) Nothing Nothing True
                where
                    go = Just $ level * 10
                    level = effectivespelllevel mi s

haste :: SpellTemplate
haste mi _ s = Spell (SpellInfo ("Haste") 2 Abjuration 0 0 Nothing (Allies 50)) emptyspellroll (False,False)
                Nothing Nothing (Just $ Effect Haste (Present go [] [0]) Nothing Nothing) Nothing Nothing True
                where
                    go = Just $ level
                    level = effectivespelllevel mi s

hurry :: SpellTemplate
hurry mi _ s = Spell (SpellInfo ("Hurry") 2 Abjuration 0 0 Nothing (SingleTarget 60)) emptyspellroll (True,False)
                Nothing Nothing (Just $ Effect Haste (Present (Just 1) [] [0]) Nothing Nothing) Nothing Nothing False

improvedhurry :: SpellTemplate
improvedhurry mi _ s = Spell (SpellInfo ("Improved Hurry") 1 Abjuration 0 0 Nothing (SingleTarget 60)) emptyspellroll (True,False)
                Nothing Nothing (Just $ Effect Haste (Present (Just 1) [] [0]) Nothing Nothing) Nothing Nothing False

divinefavour :: SpellTemplate
divinefavour mi _ s = Spell (SpellInfo ("Divine Favour") 2 Abjuration 0 0 Nothing Caster) emptyspellroll (False,False)
                Nothing Nothing (Just $ Effect DivineFavour (Present go [] [0]) go2 Nothing) Nothing Nothing True
                where
                    go = Just 10
                    level = effectivespelllevel mi s
                    go2 = Just $ min 3 ((level + 2) `div` 3)

healpoisen :: SpellTemplate
healpoisen _ _ _ = Spell (SpellInfo ("Heal Poisen") 2 Abjuration 0 0 Nothing (SingleTarget 9)) emptyspellroll (False,False)
                Nothing Nothing (Just $ Effect Poisened (Absent Nothing [] [0]) Nothing Nothing) Nothing Nothing True

causewounds :: Int -> SpellTemplate
causewounds i mi _ s = Spell (SpellInfo ("Cause " ++ go ++ " Wounds") 2 Evocation go3 0 Nothing (SingleTarget (close go2))) emptyspellroll (True,False)
    (Just $ D (go1) go2 (Just Negative) (Just $ Target Touch 0) Negates) Nothing Nothing Nothing Nothing True
    where
        go
            | i == 1 = "Light"
            | i == 2 = "Moderate"
            | i == 3 = "Serious"
            | i == 4 = "Critical"
            | otherwise = "Light"
        go1
            | i == 1 = [8]
            | i == 2 = [8,8]
            | i == 3 = [8,8,8]
            | i == 4 = [8,8,8,8]
            | otherwise = [8]
        go2 = effectivespelllevel mi s
        go3 = view (#dexterity) (abilitybonuses s) + (temporary $ view (#primclass . #bab) s) + (temporary . sum . view (#miscclass . #bab) . gatherbonuses $ s)

darkheal :: SpellTemplate
darkheal _ _ s = Spell (SpellInfo ("Dark Heal") 2 Abjuration 0 0 Nothing (SingleTarget 9)) emptyspellroll (False,False)
                Nothing Nothing (Just $ Effect DarkHeal (Present (Just level) [] [0]) Nothing Nothing) Nothing Nothing True
                where
                    level = sum $ view (#playerclasses) s

sleep1 :: SpellTemplate
sleep1 mi a s = Spell (SpellInfo ("Sleep (4 HD total)") 2 Enchantment go1 0 Nothing (AllInRange (Sphere 10) (medium $ effectivespelllevel mi s))) emptyspellroll (False,False)
                Nothing Nothing (Just $ Effect Sleep (Present (Just go2) [] [0]) Nothing (Just $ Target Will 0)) Nothing Nothing True
                where
                    go1 = abix a s
                    go2 = 10 * effectivespelllevel mi s

firebolt :: SpellTemplate
firebolt _ _ s = Spell (SpellInfo ("Fire Bolt") 2 Evocation go3 0 Nothing (SingleTarget (close 0))) emptyspellroll (False,False)
    (Just $ D [12] 0 (Just Fire) (Just $ Target Touch 0) Negates) Nothing Nothing Nothing Nothing True
    where
        go3 = view (#dexterity) (abilitybonuses s) + (temporary $ view (#primclass . #bab) s) + (temporary . sum . view (#miscclass . #bab) . gatherbonuses $ s)

