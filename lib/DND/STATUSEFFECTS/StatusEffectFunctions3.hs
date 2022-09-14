{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}


module DND.STATUSEFFECTS.StatusEffectFunctions3
where

import Control.Lens (view, over, set)
import Data.Generics.Labels ()
import Control.Lens.Indexed (imap)
import Data.Maybe (fromJust)

import DND.CHARACTER.Status (Status)

import DND.DEFENSE.DefenseFunctions (defense2defensetype, ffdefensetype)

import DND.STATUSEFFECTS.Effect (Effect (Effect))
import DND.STATUSEFFECTS.Effects (StatusEffect (..), Temporal (), Effects, effix, temporaldurationreduction, removeemptydurations, removerepeatdurations, (&), isActive)
import DND.STATUSEFFECTS.StatusEffectFunctions
import DND.STATUSEFFECTS.StatusEffectFunctions1
import DND.STATUSEFFECTS.StatusEffectFunctions2
import DND.TemporaryValue (Temporary, temporary)


twtgwp :: Maybe StatusEffect -> Temporal (Maybe Int) -> Status -> Status
twtgwp Nothing = (\_ -> id)
twtgwp (Just mse)
    | mse == Undead = changestatus (#effects . #undead) undeadon undeadoff
    | mse == Invisible = changestatus (#effects . #invisible) invisibleon invisibleoff
    | mse == ImmunetoCriticals = changestatus (#effects . #immunetocriticals) (\_ -> id) (\_ -> id)
    | mse == MirrorImage = changestatus (#effects . #mirrorImage) (\_ -> id) (\_ -> id)
    | mse == TrueStrike = changestatus (#effects . #trueStrike) (\_ -> id) (\_ -> id)
    | mse == Evasion = changestatus (#effects . #evasion) (\_ -> id) (\_ -> id)
    | mse == GreaterEvasion = changestatus (#effects . #greaterEvasion) (\_ -> id) (\_ -> id)
    | mse == Sing = changestatus (#effects . #sing) singon singoff
    | mse == Rage = changestatus (#effects . #rage) rageon rageoff
    | mse == BearStrength = changestatus (#effects . #bearStrength) bearstrengthon bearstrengthoff
    | mse == StoneSkin = changestatus (#effects . #stoneSkin) (\_ -> id) (\_ -> id)
    | mse == Repelled = changestatus (#effects . #repelled) (\_ -> id) (\_ -> id)
    | mse == Blind = changestatus (#effects . #blind) blindon blindoff
    | mse == Stealth = changestatus (#effects . #stealth) (\_ -> id) (\_ -> id)
    | mse == Bleeding = changestatus (#effects . #bleeding) (\_ -> id) (\_ -> id)
    | mse == Prone = changestatus (#effects . #prone) proneon proneoff
    | mse == UncannyDodge = changestatus (#effects . #uncannydodge) uncannydodgeon uncannydodgeoff
    | mse == Disarmed = changestatus (#effects . #disarmed) disarmon (\_ -> id)
    | mse == Getem = changestatus (#effects . #getem) getemon getemoff
    | mse == ExpertAttack = changestatus (#effects . #expertattack) (\_ -> id) (\_ -> id)
    | mse == ImprovedGetem = changestatus (#effects . #improvedgetem) (\_ -> id) (\_ -> id)
    | mse == ForceField = changestatus (#effects . #forcefield) (\_ -> id) (\_ -> id)
    | mse == Mobility = changestatus (#effects . #mobility) (\_ -> id) (\_ -> id)
    | mse == ArcanStrike = changestatus (#effects . #arcanestrike) (\_ -> id) (\_ -> id)
    | mse == Fatigued = changestatus (#effects . #fatigued) fatigueon fatigueoff
    | mse == Exhausted = changestatus (#effects . #exhausted) exhaustedon exhaustedoff
    | mse == Shaken = changestatus (#effects . #shaken) shakenon shakenoff
    | mse == Fear = changestatus (#effects . #fear) fearon fearoff
    | mse == Sickened = changestatus (#effects . #sickened) sickenedon sickenedoff
    | mse == Nauseated = changestatus (#effects . #nauseated) (\_ -> id) (\_ -> id)
    | mse == ImmunetoDiseae = changestatus (#effects . #immunetodisease) (\_ -> id) (\_ -> id)
    | mse == ImmunetoMagic = changestatus (#effects . #immunetomagic) (\_ -> id) (\_ -> id)
    | mse == ImmunetoPoisen = changestatus (#effects . #immunetopoisen) (\_ -> id) (\_ -> id)
    | mse == Sleep = changestatus (#effects . #sleep) (\_ -> id) (\_ -> id)
    | mse == KeenSenses = changestatus (#effects . #keensenses) keensenseson keensensesoff
    | mse == ImmunetoSneakAttack = changestatus (#effects . #immunetosneakattack) (\_ -> id) (\_ -> id)
    | mse == SpellFocus = changestatus (#effects . #spellfocus) (\_ -> id) (\_ -> id)
    | mse == Burning = changestatus (#effects . #burning) (\_ -> id) (\_ -> id)
    | mse == Paralyzed = changestatus (#effects . #paralyzed) (\_ -> id) (\_ -> id)
    | mse == Confused = changestatus (#effects . #confused) (\_ -> id) (\_ -> id)
    | mse == Laughter = changestatus (#effects . #laughter) (\_ -> id) (\_ -> id)
    | mse == ImprovedInvisibility = changestatus (#effects . #improvedinvisibility) invisibleon invisibleoff
    | mse == ProtectiveWard = changestatus (#effects . #protectiveward) protectivewardon protectivewardoff
    | mse == Dazed = changestatus (#effects . #dazed) (\_ -> id) (\_ -> id)
    | mse == Despair = changestatus (#effects . #despair) sickenedon sickenedoff
    | mse == AuraofDespair = changestatus (#effects . #auraofdespair) (\_ -> id) (\_ -> id)
    | mse == SpellTurning = changestatus (#effects . #spellturning) (\_ -> id) (\_ -> id)
    | mse == Dominated = changestatus (#effects . #dominated) dominatedon dominatedoff
    | mse == BeastShape = changestatus (#effects . #beastshape) beastshapeon beastshapeoff
    | mse == ElementalBodyIII = changestatus (#effects . #elementalbodyiii) elementalbodyiiion elementalbodyiiioff
    | mse == ElementalBodyIV = changestatus (#effects . #elementalbodyiv) elementalbodyivon elementalbodyivoff
    | mse == SmiteEvil = changestatus (#effects . #smiteevil) (\_ -> id) (\_ -> id)
    | mse == DivineBond = changestatus (#effects . #divinebond) (\_ -> id) (\_ -> id)
    | mse == WeaponFinesse = changestatus (#effects . #weaponfinesse) (\_ -> id) (\_ -> id)
    | mse == FormoftheDragon = changestatus (#effects . #formofthedragon) formofthedragonon formofthedragonoff
    | mse == Stigmata = changestatus (#effects . #stigmata) stigmataon stigmataoff
    | mse == ChannelSmite = changestatus (#effects . #channelsmite) (\_ -> id) (\_ -> id)
    | mse == MagicFang = changestatus (#effects . #magicfang) (\_ -> id) (\_ -> id)
    | mse == ChannelShield = changestatus (#effects . #channelshield) channelshieldon channelshieldoff
    | mse == Aid = changestatus (#effects . #aid) aidon aidoff
    | mse == Bless = changestatus (#effects . #bless) blesson blessoff
    | mse == DeadlyAim = changestatus (#effects . #deadlyaim) (\_ -> id) (\_ -> id)
    | mse == WereRaptor = changestatus (#effects . #wereraptor) wereraptoron wereraptoroff
    | mse == Shield = changestatus (#effects . #shield) shieldon shieldoff
    | mse == Blur = changestatus (#effects . #blur) bluron bluroff
    | mse == Heroism = changestatus (#effects . #heroism) heroismon heroismoff
    | mse == FireShield = changestatus (#effects . #fireshield) (\_ -> id) (\_ -> id)
    | mse == CatsGrace = changestatus (#effects . #catsgrace) catsgraceon catsgraceoff
    | mse == Held = changestatus (#effects . #held) (\_ -> id) (\_ -> id)
    | mse == BullsEndurance = changestatus (#effects . #bullsendurance) bullsenduranceon bullsenduranceoff
    | mse == GrowClaws = changestatus (#effects . #growclaws) growclawson growclawsoff
    | mse == Entangled = changestatus (#effects . #entangled) entangledon entangledoff
    | mse == BarkSkin = changestatus (#effects . #barkskin) barkskinon barkskinoff
    | mse == Polymorth = changestatus (#effects . #polymorth) polymorthon polymorthoff
    | mse == DivineFavour = changestatus (#effects . #divinefavour) divinefavouron divinefavouroff
    | mse == CombatShot = changestatus (#effects . #combatShot) (\_ -> id) (\_ -> id)
    | mse == Poisened = changestatus (#effects . #poisened) (\_ -> id) poisenedoff
    | mse == ImmuneMindInfluencingEffects = changestatus (#effects . #immuneMindInfluencingEffects) immuneMindInfluencingEffectson immuneMindInfluencingEffectsoff
    | mse == Stunned = changestatus (#effects . #stunned) stunnedon stunnedoff
    | mse == StunningFist = changestatus (#effects . #stunningfist) (\_ -> id) (\_ -> id)
    | mse == Ki = changestatus (#effects . #ki) kion kioff
    | mse == Staggered = changestatus (#effects . #staggered) (\_ -> id) (\_ -> id)
    | mse == DarkHeal = changestatus (#effects . #darkheal) darkhealon darkhealoff
    | mse == Grappled = changestatus (#effects . #grappled) grappledon grappledoff
    | mse == Cover = changestatus (#effects . #cover) coveron coveroff
    | mse == Charged = changestatus (#effects . #charged) chargedon chargedoff
    | mse == Incorporal = changestatus (#effects . #incorporal) (\_ -> id) (\_ -> id)
    | mse == Pounce = changestatus (#effects . #pounce) (\_ -> id) (\_ -> id)
    | mse == Turned = changestatus (#effects . #turned) (\_ -> id) (\_ -> id)
    | mse == Commanded = changestatus (#effects . #commanded) commandedon commandedoff
    | mse == ChannelResistance = changestatus (#effects . #channelresistance) (\_ -> id) (\_ -> id)
    | mse == ShieldofFaith = changestatus (#effects . #shieldoffaith) shieldoffaithon shieldoffaithoff
    | mse == ProtectionFrom = changestatus (#effects . #protectionfrom) (\_ -> id) (\_ -> id)
    | mse == DefensiveStance = changestatus (#effects . #defensivestance) defensivestanceon defensivestanceoff
    | mse == ImprovedGrapple = changestatus (#effects . #improvedgrapple) (\_ -> id) (\_ -> id)
    | mse == ImprovedDisarm = changestatus (#effects . #improveddisarm) (\_ -> id) (\_ -> id)
    | mse == ImprovedTrip = changestatus (#effects . #improvedtrip) (\_ -> id) (\_ -> id)
    | mse == Haste = changestatus (#effects . #haste) (\_ -> id) (\_ -> id)
    | mse == Slow = changestatus (#effects . #slow) (\_ -> id) (\_ -> id)
    | mse == Cleave = changestatus (#effects . #cleave) (\_ -> id) (\_ -> id)
    | mse == ImprovedDoubleAttack = changestatus (#effects . #improveddoubleattack) (\_ -> id) (\_ -> id)
    | mse == TripleAttack = changestatus (#effects . #tripleattack) (\_ -> id) (\_ -> id)
    | mse == PhotonMode = changestatus (#effects . #photonmode) photonon photonoff
    | mse == GravitonMode = changestatus (#effects . #gravitonmode) gravitonon gravitonoff
    | mse == HIPS = changestatus (#effects . #hips) (\_ -> id) (\_ -> id)
    | mse == TrickAttack = changestatus (#effects . #trickattack) (\_ -> id) (\_ -> id)
    | mse == FlatFooted = changestatus (#effects . #flatfooted) (\_ -> id) (\_ -> id)
    | mse == DarkMatter = changestatus (#effects . #darkmatter) darkmatteron darkmatteroff
twtgwp _ = (\_ -> id)

effectlist :: Effects a -> [StatusEffect]
effectlist effs = foldr ((++) . pure) [] (imap const effs)

effectslist :: [StatusEffect]
effectslist = effectlist (pure 0 :: Effects Int)

effect2s2s :: Int -> Int -> Maybe (Effect (Temporal (Maybe Int))) -> Status -> Status
effect2s2s _ _ Nothing s = s
effect2s2s i i2 (Just (Effect se temp tmb mtar)) s
    | mtar == Nothing = t2ix se newtemp s
    | i2 == 1 = s
    | i2 == 20 = t2ix se newtemp s
    | (pure $ i + i2 + (view (#bonus) (fromJust mtar))) > target = t2ix se newtemp s
    | otherwise = s
      where
        newtemp = fmap (tmb &) temp
        ishelpless = 
          (isActive . temporary) (view (#effects . #held) s) || ((isActive . temporary) (view (#effects . #paralyzed) s)) || ((isActive . temporary) (view (#effects . #sleep) s))
        target
            | ishelpless = ffdefensetype (view (#defensetype) (fromJust mtar)) $
              (set (#abilityscores . #wisdom) (pure 10 :: Temporary Int) $ 
              set (#abilityscores . #dexterity) (pure 0 :: Temporary Int) $ 
              over (#bonuses . #abilityscores . #wisdom) (fmap (\_ -> pure 0 :: Temporary Int)) $
              over (#bonuses . #abilityscores . #dexterity) (fmap (\_ -> pure 0 :: Temporary Int)) s)
            | (isActive . temporary) (view (#effects . #prone) s) = ffdefensetype (view (#defensetype) (fromJust mtar)) s
            | (isActive . temporary) (view (#effects . #stunned) s) = ffdefensetype (view (#defensetype) (fromJust mtar)) s
            | (isActive . temporary) (view (#effects . #grappled) s) = ffdefensetype (view (#defensetype) (fromJust mtar)) s
            | (isActive . temporary) (view (#effects . #laughter) s) = ffdefensetype (view (#defensetype) (fromJust mtar)) s
            | (isActive . temporary) (view (#effects . #flatfooted) s) = ffdefensetype (view (#defensetype) (fromJust mtar)) s   
            | otherwise = defense2defensetype (view (#defensetype) (fromJust mtar)) s

reducet2durations :: Status -> Status
reducet2durations s = newstatus
    where
        effects = view (#effects) s
        neweffects = (removeemptydurations . (temporaldurationreduction <$>)) <$> effects
        presenteffects = temporary <$> neweffects
        newstatus = over (#effects) ((removerepeatdurations . removeemptydurations) <$>) $ foldr (.) id (imap go presenteffects) (over (#effects) ((temporaldurationreduction <$>) <$>) s)
        go :: StatusEffect -> Temporal (Maybe Int) -> Status -> Status
        go seff temp status
          | temporary (effix seff (view (#effects) s)) == temp = status
          | otherwise = t2ix seff temp status