{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module DND.ATTACKS.AttackFunctions
where

import Control.Monad.Trans.State.Strict (State, state)
import Control.Lens (set, over, view)
import Data.Generics.Labels ()
import Control.Applicative

import DND.Action (Action (Action, runF), action)
import DND.Roll (Roll (Roll), Dice, runRoll, (~>), roll2mint)
import DND.TemporaryValue (Temporary, temporary)

import DND.ATTACKS.Attack
import DND.ATTACKS.DamageReduction (Material (Reduc), DamageType (Bludgeoning), drix, dt2mat, b2mat, l2mat)
import DND.ATTACKS.Smite (Smite (Smite), Benificence (Evil, NeutraL, Good), Lawfulness (Neutral, Chaotic, Lawful), Alignment (Al), rix)
import DND.ATTACKS.Weapon

import DND.CHARACTER.Status (healthupdate, Status (effects, elementalresistance, damagereduction), damager, damager')

import DND.DAMAGEDICE.DamageDice (DamageDice (D), SaveProfile (Negates))
import DND.DAMAGEDICE.DamageDiceFunctions (dDEP2MI, mddstatus)
import DND.DAMAGEDICE.Elemental (Elemental (Fire, Ice, Acid))

import DND.DEFENSE.Defense (Defense (Defense, CMD, Will), Target (Target))
import DND.DEFENSE.DefenseFunctions (defense2defensetype, perception, ffdefensetype, ffdefensetype')

import DND.STATDAMAGE.StatDamageFunctions (statdamage)

import DND.STATUSEFFECTS.Effect (Effect(Effect))
import DND.STATUSEFFECTS.Effects (Effects (immunetocriticals, mirrorImage, forcefield), viewtempmodifyier, forcefielddamage2, (&), isActive, Temporal (Absent, Permanent, Off, Present, NA), settemporal,
    StatusEffect (MirrorImage, Sleep, Sickened, Bleeding, ChannelShield, Regeneration), setmodifyer, isOn)
import DND.STATUSEFFECTS.StatusEffectFunctions2 (t2ix)
import DND.STATUSEFFECTS.StatusEffectFunctions3 (effect2s2s)

type Attacking = State Status

regenrationendingattack :: Attack a b -> Bool
regenrationendingattack att = go1 maindice || foldr (\ x y -> go x || y) False secondarydie
    where
        go x = view (#elemental) x == Just Acid || view (#elemental) x == Just Fire
        maindice = ddice att
        secondarydie =  mddice att
        go1 x = case x of
            Nothing -> False
            Just y -> go y

isHit :: Result a -> Bool
isHit (Hit _) = True
isHit (CriticalHit _ _) = True
isHit _ = False

emptyAttack :: AttackRoll [Int]
emptyAttack = AttackRoll [20] [20] [100] [100] [100] [20] [20] [20]

combatmaneuver :: Attack [Int] [Int]
combatmaneuver = Attack (AInfo "Attack" Melee 0 Bludgeoning Reduc (Just [0]) CMD (C 21 1 0) Nothing (Al Neutral NeutraL) False (False,False)) 
                    emptyAttack (Just $ pure [0]) [] Nothing Nothing Nothing []

attack1 :: Attack [Int] [Int]
attack1 = Attack (AInfo "Attack 1" Melee 0 Bludgeoning Reduc (Just [17, 12, 7]) Defense (C 19 3 0) Nothing (Al Neutral NeutraL) False (False,False)) 
            emptyAttack (Just $ D [6,6] 20 Nothing Nothing Negates) 
            [(D (take 100 $ cycle [6]) 517 (Just Fire) Nothing Negates), (D [1] 0 (Just Acid) Nothing Negates), (D [6,6] 87 (Just Ice) Nothing Negates) ] 
            Nothing (Just (Effect MirrorImage (Permanent (Just 10) [] [20]) Nothing Nothing)) Nothing 
            [(Effect Sickened (Present Nothing [] [100]) (Just 50) Nothing), (Effect Bleeding (Permanent (Just 10) [] [10]) (Just 500) Nothing), (Effect Sleep (Absent Nothing [] [0]) Nothing (Just $ Target Will 15))]

action1 :: Action (Attack [Int]) [Int]
action1 = Action attack1

action2 :: [Action (Attack Int) Roll]
action2 = [Action $ Attack (AInfo "Attack 1" Melee 0 Bludgeoning Reduc (Just 25) Defense (C 19 3 0) Nothing (Al Neutral NeutraL) False (False,False)) 
    (AttackRoll (Roll 20) (Roll 20) (Roll 10) (Roll 50) (Roll 88) (Roll 20) (Roll 20) (Roll 20)) (Just $ D (Roll 1) 0 Nothing Nothing Negates) 
    [] Nothing Nothing Nothing [], Action $ Attack (AInfo "Attack 1" Melee 0 Bludgeoning Reduc (Just 25) Defense (C 19 3 0) Nothing (Al Neutral NeutraL) False (False,False)) 
    (AttackRoll (Roll 15) (Roll 20) (Roll 10) (Roll 50) (Roll 88) (Roll 20) (Roll 20) (Roll 20)) (Just $ D (Roll 10) 0 Nothing Nothing Negates) 
    [] Nothing Nothing Nothing [], Action $ Attack (AInfo "Attack 1" Melee 0 Bludgeoning Reduc (Just 25) Defense (C 19 3 0) Nothing (Al Neutral NeutraL) False (False,False)) 
    (AttackRoll (Roll 20) (Roll 20) (Roll 10) (Roll 50) (Roll 88) (Roll 20) (Roll 20) (Roll 20)) (Just $ D (Roll 1) 0 Nothing Nothing Negates) 
    [] Nothing Nothing Nothing []] 

attacksRolls :: [Action (Attack [Int]) [Int]] -> Dice [Action (Attack Int) Roll]
attacksRolls actions = concat <$> attackRolls ~> actions

attackRolls :: Action (Attack [Int]) [Int] -> Dice [Action (Attack Int) Roll]
attackRolls (Action (Attack (AInfo name range incr damtype mat Nothing def crit mabil al prep spproxy) ar dd mdd mstd meff msdd mseff)) = do
    only <- action (Action (Attack (AInfo name range incr damtype mat Nothing def crit mabil al prep spproxy) ar dd mdd mstd meff msdd mseff))
    return [only]
attackRolls (Action (Attack (AInfo _ _ _ _ _ (Just []) _ _ _ _ _ _) _ _ _ _ _ _ _)) = return []
attackRolls (Action (Attack (AInfo name range incr damtype mat (Just (att : atts)) def crit mabil al prep spproxy) ar dd mdd mstd meff msdd mseff))= do
    first <- action (Action (Attack (AInfo name range incr damtype mat (Just att) def crit mabil al prep spproxy) ar dd mdd mstd meff msdd mseff))
    second <- attackRolls (Action (Attack (AInfo name range incr damtype mat (Just atts) def crit mabil al prep spproxy) ar dd mdd mstd meff Nothing []))
    return ([first] ++ second)

attackNormal :: Action (Attack  Int) Roll -> Status -> (Result (Maybe Int), Status)
attackNormal a s
    | tohit == 1 = (CriticalMiss, s)
    | hitpercent <= conceal = (Miss Nothing target', s)
    | dd == Nothing && tohit >= crittarget && isMI && hitsImage = (Miss Nothing target', statusless1image)
    | dd == Nothing && tohittotal > finaltarget && isMI && hitsImage = (Miss Nothing target', statusless1image)
    | dd == Nothing && tohit >= crittarget = (Hit Nothing, finals)
    | dd == Nothing && tohittotal > finaltarget = (Hit Nothing, finals)
    | dd == Nothing = (Miss ((finaltarget) & (negate <$> tohittotal)) target', s)
    | isSS && vulnerable && (tohit >= crittarget) &&  (tocrittotal > finaltarget) = (CriticalHit ((*) <$> critmult <*> damagess) critmult, sscritdamagedstatus)
    | vulnerable && (tohit >= crittarget) && (tocrittotal > finaltarget) = (CriticalHit ((*) <$> critmult <*> damagetotal) critmult, critdamagedstatus)
    | tohit >= crittarget && isMI && hitsImage = (Miss Nothing target', statusless1image)
    | tohit >= crittarget && isSS = (Hit damagess, ssdamagedstatus)
    | tohit >= crittarget = (Hit damagetotal, damagedstatus)
    | tohittotal > finaltarget && isMI && hitsImage = (Miss Nothing target', statusless1image)
    | tohittotal > finaltarget && isSS = (Hit damagess, ssdamagedstatus)
    | tohittotal > finaltarget = (Hit damagetotal, damagedstatus)
    | ((+5) <$> tohittotal) > finaltarget && isMI = (Miss Nothing target', statusless1image)
    | otherwise = (Miss ((finaltarget) & (negate <$> tohittotal)) target', s)
    where
        (Action (Attack info ar dd mdd mstd meff _ _)) = a
        pronedefense
            | (range info == Missile) && ((isActive . temporary) (view (#effects . #prone) s)) = (Just 4)
            | ((isActive . temporary) (view (#effects . #prone) s)) = (Just (-4))
            | otherwise = Nothing
        helplessdefense
            | (range info == Missile) && ishelpless = Nothing
            | ishelpless = (Just (-4))
            | otherwise = Nothing
        align = view (#alignment) info
        isprotectedfrom = isActive . temporary $ view (#effects . #protectionfrom) s
        protectedfromwhat = viewtempmodifyier . temporary $ view (#effects . #protectionfrom) s
        protectedfromdefense
            | not isprotectedfrom = Nothing
            | protectedfromwhat == (Just 1) && (view (#benificence) align == Evil) = Just 2
            | protectedfromwhat == (Just 2) && (view (#benificence) align == Good) = Just 2
            | protectedfromwhat == (Just 3) && (view (#lawfullness) align == Lawful) = Just 2
            | protectedfromwhat == (Just 4) && (view (#lawfullness) align == Chaotic) = Just 2
            | otherwise = Nothing
        ishelpless = 
          (isActive . temporary) (view (#effects . #held) s) || ((isActive . temporary) (view (#effects . #paralyzed) s)) || ((isActive . temporary) (view (#effects . #sleep) s))
        hitpercent = runRoll $ hitpercentage ar
        conceal = temporary $ view (#concealment) s
        isMI = isActive $ (temporary . mirrorImage . effects) s
        isSS = isActive $ (temporary . forcefield . effects) s
        lawful = view (#lawfullness) . temporary $ view (#alignment) s
        rce = view (#race) s
        target
            | ishelpless = ffdefensetype (versus info) 
              (set (#abilityscores . #wisdom) (pure 10 :: Temporary Int) $ 
              set (#abilityscores . #dexterity) (pure 0 :: Temporary Int) $ 
              over (#bonuses . #abilityscores . #wisdom) (fmap (\_ -> pure 0 :: Temporary Int)) $
              over (#bonuses . #abilityscores . #dexterity) (fmap (\_ -> pure 0 :: Temporary Int)) s)
            | (isActive . temporary) (view (#effects . #prone) s) = ffdefensetype (versus info) s
            | (isActive . temporary) (view (#effects . #stunned) s) = ffdefensetype (versus info) s
            | (isActive . temporary) (view (#effects . #grappled) s) = ffdefensetype (versus info) s
            | (isActive . temporary) (view (#effects . #laughter) s) = ffdefensetype (versus info) s     
            | (isActive . temporary) (view (#effects . #flatfooted) s) = ffdefensetype (versus info) s            
            | otherwise = defense2defensetype (versus info) s
        target'
            | (isActive . temporary) (view (#effects . #prone) s) = ffdefensetype' (versus info) s
            | (isActive . temporary) (view (#effects . #stunned) s) = ffdefensetype' (versus info) s
            | (isActive . temporary) (view (#effects . #grappled) s) = ffdefensetype' (versus info) s
            | (isActive . temporary) (view (#effects . #laughter) s) = ffdefensetype' (versus info) s     
            | (isActive . temporary) (view (#effects . #flatfooted) s) = ffdefensetype' (versus info) s            
            | otherwise = (versus info)
        finaltarget = target & pronedefense & helplessdefense & protectedfromdefense
        crittarget = (threshold . critical) info
        critmult = pure $ (multiplier . critical) info
        tohit = runRoll $ tohitroll ar
        tohittotal = (+) <$> (attackbonus info) <*> (pure tohit)
        tocrit = pure $ (critbonus . critical) info + (runRoll $ tohitroll ar)
        tocrittotal = (attackbonus info) & tocrit
        hitsImage = (pure ((runRoll . mIpercentage) ar)) < ((viewtempmodifyier . temporary . mirrorImage . effects) s >>= (\x -> Just $ (100 * x) `div` (x + 1)))
        vulnerable = (not . isActive . temporary . immunetocriticals . effects) s
        ispreparedforcharge = view (#preparedagainstcharge) info && (isActive . temporary $ view (#effects . #charged) s)
        damage1 = (({-(set (#damagebonus) 0) .-} (runRoll <$>)) <$> dd) >>= (\x -> dDEP2MI x (elementalresistance s))
        damage2 = foldr (&) Nothing ((\x -> dDEP2MI (runRoll <$> x) $ elementalresistance s) <$> (function3 mdd))
        damagefinal
            | ispreparedforcharge = ((2 *) <$> damage1) & damage2  {-& (fmap (view (#damagebonus)) dd)-}
            | otherwise = damage1 & damage2 {- & (fmap (view (#damagebonus)) dd)-}
        (smitedam, smiteatt) = (Nothing, Nothing)
        reduction1 = view ((drix . dt2mat . damagetype) info) (temporary <$> (damagereduction s))
        reduction2 = view ((drix . material) info) (temporary <$> (damagereduction s))
        reduction3 = view ((drix . b2mat . view (#benificence) . alignment) info) (temporary <$> (damagereduction s))
        reduction4 = view ((drix . l2mat . view (#lawfullness) . alignment) info) (temporary <$> (damagereduction s))
        reductiontotal = min (reduction1 `min` reduction2) (reduction3 `min` reduction4)
        damagetotal'
            | damagefinal < (Just 1) = damagefinal
            | damagefinal <= reductiontotal = (Just 0)
            | otherwise = damagefinal & (negate <$> reductiontotal)
        damagetotal
            | isActive . temporary $ view (#effects . #incorporal) s = fmap (\x -> x `div` 2) damagetotal'
            | otherwise = damagetotal'
        (newss, damagess) = forcefielddamage2 damagetotal $ (temporary . forcefield . effects) s
        firsts = effect2s2s 0 (runRoll $ effectroll ar) (((roll2mint <$>) <$>) <$> meff) s
        seconds = statdamage 0 (runRoll $ statdamageroll ar) ((runRoll <$>) <$> mstd) firsts
        thirds = t2ix Sleep (Absent Nothing [] Nothing) seconds
        {-fourths = foldr (.) id ((effect2s2s 0 (runRoll $ effectroll ar)) <$> ((Just . ((roll2mint <$>) <$>)) <$> (function3 mseff))) thirds-}
        fifths
            | (isOn . temporary) (view (#effects . #channelshield) s) = t2ix ChannelShield (Off Nothing [] Nothing) thirds
            | otherwise = thirds
        finals  
            | (isActive . temporary) (view (#effects . #regeneration) s) && regenrationendingattack (runF a) = t2ix Regeneration (NA Nothing [] Nothing) fifths
            | otherwise = fifths
        {-sdddamage = (mddstatus 0 (runRoll $ msddroll ar) (fmap (fmap runRoll) msdd))
        sadamage
            | immunetosa = id
            | isaberrant && (sorcerorlevel >= 13) && fiftypercent = id
            | isaberrant && (sorcerorlevel >= 9) && twentyfivepercent = id
            | otherwise = (mddstatus 0 (runRoll $ stealthroll ar) (fmap (fmap runRoll) msa))
                where
                  percentagecheck = (runRoll (msddroll ar)) * 5
                  fiftypercent = percentagecheck <= 50
                  twentyfivepercent = percentagecheck <= 25
                  isaberrant = view (#classfeatures . #bloodline . #aberrant) s
                  sorcerorlevel = view (#playerclasses . #sorc) s
                  immunetosa = (isActive . temporary) $ view (#effects . #immunetosneakattack) s
        update = (healthupdate . sadamage . sdddamage)-}
        damagedstatus = {-update $-} damager damagetotal finals
        critdamagedstatus = {-update-} (damager' (#injury) critmult $ damager ((((*) . (\x -> x - 1)) <$> critmult <*> damage1) & damagetotal) finals)
        statusless1image
            | imagenumber == Just 1 = t2ix MirrorImage (Absent Nothing [] Nothing) s
            | otherwise = over (#effects . #mirrorImage) (settemporal newtemporal) s
                where
                    imagenumber = (viewtempmodifyier . temporary . mirrorImage . effects) s
                    temporal = (temporary . mirrorImage . effects) s
                    newtemporal = setmodifyer ((\x -> x - 1) <$> imagenumber) temporal
        ssdamagedstatus = {-update-} (over (#effects . #forcefield) (settemporal newss) $ damager damagess finals)
        news1 = damager' (#injury) critmult $ damager ((((*) . (\x -> x - 1)) <$> critmult <*> damage1) & damagess) finals
        sscritdamagedstatus = {-update $-} over (#effects . #forcefield) (settemporal newss) news1

singleeffectAttack :: Action (Attack  Int) Roll -> Status -> Status
singleeffectAttack (Action (Attack _ ar _ _ _ _ msdd mseff)) s = update . aftermseff $ s
    where
        aftermseff s' = foldr (.) id ((effect2s2s 0 (runRoll $ effectroll ar)) <$> ((Just . ((roll2mint <$>) <$>)) <$> (function3 mseff))) s'
        sdddamage = (mddstatus 0 (runRoll $ msddroll ar) (fmap (fmap runRoll) msdd))
        update = (healthupdate . sdddamage)

attacking :: Action (Attack  Int) Roll -> Attacking (Result (Maybe Int))
attacking a = state $ attackNormal a

attackings :: [Action (Attack  Int) Roll] -> Attacking [(Result (Maybe Int))]
attackings a = attacking ~> a
    
function :: Int -> [a] -> [[a]]
function _ [] = [[]]
function int as = [take int as] ++ (function int (drop int as))

function2 :: Int -> [[a]] -> [a]
function2 _ [[]] = []
function2 _ [] = []
function2 int (a : as) = (take 1 (drop int a)) ++ (function2 (int + 1) as)

function3 :: [a] -> [a]
function3 as = function2 0 $ function truelength as
  where
    truelength :: Int
    truelength = round $ sqrt ((fromIntegral . length) as :: Double)
