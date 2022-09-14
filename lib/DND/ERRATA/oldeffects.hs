undeader :: Temporal (Maybe Int) -> Status -> Status
undeader NA oldstatus
    | isPresent $ (view (#effects . #undead) oldstatus) = set (#effects . #undead) NA $ over (#elementalresistance) (removeprofile' <$> go <*>) oldstatus
    | otherwise = set (#effects . #undead) NA oldstatus
        where
            go = temporary <$> undeadelementalresistance
undeader Absent oldstatus
    | isPresent $ (view (#effects . #undead) oldstatus) = set (#effects . #undead) Absent $ over (#elementalresistance) (removeprofile' <$> go <*>) oldstatus
    | otherwise = oldstatus
        where
            go = temporary <$> undeadelementalresistance
undeader (LimitedOff a b) oldstatus
    | isPresent $ (view (#effects . #undead) oldstatus) = set (#effects . #undead) (LimitedOff a b) $ over (#elementalresistance) (removeprofile' <$> go <*>) oldstatus
    | otherwise = set (#effects . #undead) (LimitedOff a b) oldstatus
        where
            go = temporary <$> undeadelementalresistance
undeader (LimitedOn a b) oldstatus
    | isLimitedOff $ (view (#effects . #undead) oldstatus) = set (#effects . #undead) (LimitedOn a b) $ over (#elementalresistance) (modifyprofile' <$> go <*>) oldstatus
    | otherwise = oldstatus
        where
            go = temporary <$> undeadelementalresistance
undeader t oldstatus
    | view (#effects . #undead) oldstatus == NA = oldstatus
    | view (#effects . #undead) oldstatus == Absent = set (#effects . #undead) t $ over (#elementalresistance) (modifyprofile' <$> go <*>) oldstatus
    | view (#effects . #undead) oldstatus < t = set (#effects . #undead) t oldstatus
    | otherwise = oldstatus
        where
            go = temporary <$> undeadelementalresistance

invisibler :: Temporal (Maybe Int) -> Status -> Status
invisibler NA oldstatus
    | isPresent $ view (#effects . #invisible) oldstatus = gofinal
    | otherwise = set (#effects . #invisible) NA oldstatus
        where
            go1 = over (#concealment) (removetemporary 50) oldstatus
            go2 = over (#bonuses . #defense . #nonarmour . #attacks . #misc) (removetemporary 5) go1
            go3 = over (#bonuses . #miscclass . #bab . #misc) (removetemporary 3) go2
            go4 = over (#bonuses . #stealth . #misc) (removetemporary 20) go3
            go5 = stealther Absent go4
            gofinal = set (#effects . #invisible) NA go5
invisibler (LimitedOff a b) oldstatus
    | isPresent $ view (#effects . #invisible) oldstatus = gofinal
    | otherwise = set (#effects . #invisible) (LimitedOff a b) oldstatus
        where
            go1 = over (#concealment) (removetemporary 50) oldstatus
            go2 = over (#bonuses . #defense . #nonarmour . #attacks . #misc) (removetemporary 5) go1
            go3 = over (#bonuses . #miscclass . #bab . #misc) (removetemporary 3) go2
            go4 = over (#bonuses . #stealth . #misc) (removetemporary 20) go3
            go5 = stealther Absent go4
            gofinal = set (#effects . #invisible) (LimitedOff a b) go5
invisibler (LimitedOn a b) oldstatus
    | isLimitedOff $ view (#effects . #invisible) oldstatus = gofinal
    | otherwise = oldstatus
        where
            go1 = over (#concealment) (addtemporary 50) oldstatus
            go2 = over (#bonuses . #defense . #nonarmour . #attacks . #misc) (addtemporary 5) go1
            go3 = over (#bonuses . #miscclass . #bab . #misc) (addtemporary 3) go2
            go4 = over (#bonuses . #stealth . #misc) (addtemporary 20) go3
            go5 = stealther (Permanent Nothing) go4
            gofinal = set (#effects . #invisible) (LimitedOn a b) go5
invisibler Absent oldstatus
    | isPresent $ view (#effects . #invisible) oldstatus = gofinal
    | otherwise = oldstatus
        where
            go1 = over (#concealment) (removetemporary 50) oldstatus
            go2 = over (#bonuses . #defense . #nonarmour . #attacks . #misc) (removetemporary 5) go1
            go3 = over (#bonuses . #miscclass . #bab . #misc) (removetemporary 3) go2
            go4 = over (#bonuses . #stealth . #misc) (removetemporary 20) go3
            go5 = stealther Absent go4
            gofinal = set (#effects . #invisible) Absent go5
invisibler t oldstatus
    | view (#effects . #invisible) oldstatus == NA = oldstatus
    | view (#effects . #invisible) oldstatus == Absent = gofinal
    | view (#effects . #invisible) oldstatus < t = set (#effects . #invisible) t oldstatus
    | otherwise = oldstatus
        where
            go1 = over (#concealment) (addtemporary 50) oldstatus
            go2 = over (#bonuses . #defense . #nonarmour . #attacks . #misc) (addtemporary 5) go1
            go3 = over (#bonuses . #miscclass . #bab . #misc) (addtemporary 3) go2
            go4 = over (#bonuses . #stealth . #misc) (addtemporary 20) go3
            go5 = stealther (Permanent Nothing) go4
            gofinal = set (#effects . #invisible) t go5

immunetocriticalser :: Temporal (Maybe Int) -> Status -> Status
immunetocriticalser t oldstatus
    | view (#effects . #immunetocriticals) oldstatus == NA = oldstatus
    | otherwise = set (#effects . #immunetocriticals) t oldstatus

mirrorimager :: Temporal (Maybe Int) -> Status -> Status
mirrorimager t oldstatus
    | view (#effects . #mirrorImage) oldstatus == NA = oldstatus
    | otherwise = set (#effects . #mirrorImage) t oldstatus

truestriker :: Temporal (Maybe Int) -> Status -> Status
truestriker t oldstatus
    | view (#effects . #trueStrike) oldstatus == NA = oldstatus
    | otherwise = set (#effects . #trueStrike) t oldstatus

evasioner :: Temporal (Maybe Int) -> Status -> Status
evasioner t oldstatus
    | view (#effects . #evasion) oldstatus == NA = oldstatus
    | otherwise = set (#effects . #evasion) t oldstatus

greaterevasioner :: Temporal (Maybe Int) -> Status -> Status
greaterevasioner t oldstatus
    | view (#effects . #greaterEvasion) oldstatus == NA = oldstatus
    | otherwise = set (#effects . #greaterEvasion) t oldstatus

sing1er :: Temporal (Maybe Int) -> Status -> Status
sing1er NA oldstatus
    | isPresent $ view (#effects . #sing) oldstatus = set (#effects . #sing) NA $ go
    | otherwise = set (#effects . #sing) NA $ oldstatus
        where
            go  = over (#bonuses . #damage . #luck) (removetemporary 2) $ over (#bonuses . #miscclass . #bab . #luck) (removetemporary 2) oldstatus
sing1er Absent oldstatus
    | isPresent $ view (#effects . #sing) oldstatus = set (#effects . #sing) Absent $ go
    | otherwise = oldstatus
        where
            go  = over (#bonuses . #damage . #luck) (removetemporary 2) $ over (#bonuses . #miscclass . #bab . #luck) (removetemporary 2) oldstatus
sing1er (LimitedOff a b) oldstatus
    | isPresent $ view (#effects . #sing) oldstatus = set (#effects . #sing) (LimitedOff a b) $ go
    | otherwise = set (#effects . #sing) (LimitedOff a b) $ oldstatus
        where
            go  = over (#bonuses . #damage . #luck) (removetemporary 2) $ over (#bonuses . #miscclass . #bab . #luck) (removetemporary 2) oldstatus
sing1er (LimitedOn a b) oldstatus
    | isLimitedOff $ view (#effects . #sing) oldstatus = set (#effects . #sing) (LimitedOn a b) $ go
    | otherwise = oldstatus
        where
            go = over (#bonuses . #damage . #luck) (addtemporary 2) $ over (#bonuses . #miscclass . #bab . #luck) (addtemporary 2) oldstatus
sing1er t oldstatus
    | view (#effects . #sing) oldstatus == NA = oldstatus
    | view (#effects . #sing) oldstatus == Absent = set (#effects . #sing) t go
    | view (#effects . #sing) oldstatus < t = set (#effects . #sing) t oldstatus
    | otherwise = oldstatus
        where
            go :: Status
            go = over (#bonuses . #damage . #luck) (addtemporary 2) $ over (#bonuses . #miscclass . #bab . #luck) (addtemporary 2) oldstatus

rager :: Temporal (Maybe Int) -> Status -> Status
rager NA oldstatus
    | isPresent $ view (#effects . #rage) oldstatus = set (#effects . #rage) NA $ go
    | otherwise = set (#effects . #rage) NA $ oldstatus
        where
            go = over (#bonuses . #abilityscores . #strength . #deflection) (removetemporary $ 4) $ 
                penalty (#bonuses . #defense . #nonarmour . #attacks . #penalty) (-2) $
                over (#bonuses . #abilityscores . #constitution . #deflection) (removetemporary $ 4) $
                over (#bonuses . #miscclass . #wil . #deflection) (removetemporary 2) oldstatus
rager Absent oldstatus
    | isPresent $ view (#effects . #rage) oldstatus = set (#effects . #rage) Absent $ go
    | otherwise = oldstatus
        where
            go = over (#bonuses . #abilityscores . #strength . #deflection) (removetemporary $ 4) $ 
                penalty (#bonuses . #defense . #nonarmour . #attacks . #penalty) (-2) $
                over (#bonuses . #abilityscores . #constitution . #deflection) (removetemporary $ 4) $
                over (#bonuses . #miscclass . #wil . #deflection) (removetemporary 2) oldstatus
rager (LimitedOff a b) oldstatus
    | isPresent $ view (#effects . #rage) oldstatus = set (#effects . #rage) (LimitedOff a b) $ go
    | otherwise = set (#effects . #rage) (LimitedOff a b) $ oldstatus
        where
            go = over (#bonuses . #abilityscores . #strength . #deflection) (removetemporary $ 4) $ 
                penalty (#bonuses . #defense . #nonarmour . #attacks . #penalty) (-2) $
                over (#bonuses . #abilityscores . #constitution . #deflection) (removetemporary $ 4) $
                over (#bonuses . #miscclass . #wil . #deflection) (removetemporary 2) oldstatus
rager (LimitedOn a b) oldstatus
    | isLimitedOff $ view (#effects . #rage) oldstatus = set (#effects . #rage) (LimitedOn a b) $ go
    | otherwise = oldstatus
        where
            go = over (#bonuses . #abilityscores . #strength . #deflection) (addtemporary $ 4) $ 
                penalty (#bonuses . #defense . #nonarmour . #attacks . #penalty) 2 $
                over (#bonuses . #abilityscores . #constitution . #deflection) (addtemporary $ 4) $
                over (#bonuses . #miscclass . #wil . #deflection) (addtemporary 2) oldstatus
rager t oldstatus
    | view (#effects . #rage) oldstatus == NA = oldstatus
    | view (#effects . #rage) oldstatus == Absent = set (#effects . #rage) t go
    | view (#effects . #rage) oldstatus < t = set (#effects . #rage) t oldstatus
    | otherwise = oldstatus
        where
            go = over (#bonuses . #abilityscores . #strength . #deflection) (addtemporary 4) $ 
                penalty (#bonuses . #defense . #nonarmour . #attacks . #penalty) 2 $
                over (#bonuses . #abilityscores . #constitution . #deflection) (addtemporary 4) $
                over (#bonuses . #miscclass . #wil . #deflection) (addtemporary 2) oldstatus

bearstrengther :: Temporal (Maybe Int) -> Status -> Status
bearstrengther NA oldstatus
    | isPresent $ view (#effects . #bearStrength) oldstatus = set (#effects . #bearStrength) NA $ go
    | otherwise = set (#effects . #bearStrength) NA $ oldstatus
        where
            go = over (#bonuses . #abilityscores . #strength . #magic) (removetemporary $ 4) oldstatus
bearstrengther Absent oldstatus
    | isPresent $ view (#effects . #bearStrength) oldstatus = set (#effects . #bearStrength) Absent $ go
    | otherwise = oldstatus
        where
            go = over (#bonuses . #abilityscores . #strength . #magic) (removetemporary $ 4) oldstatus
bearstrengther (LimitedOff a b) oldstatus
    | isPresent $ view (#effects . #bearStrength) oldstatus = set (#effects . #bearStrength) (LimitedOff a b) $ go
    | otherwise = set (#effects . #bearStrength) (LimitedOff a b) $ oldstatus
        where
            go = over (#bonuses . #abilityscores . #strength . #magic) (removetemporary $ 4) oldstatus
bearstrengther (LimitedOn a b) oldstatus
    | isLimitedOff $ view (#effects . #bearStrength) oldstatus = set (#effects . #bearStrength) (LimitedOn a b) $ go
    | otherwise = oldstatus
        where
            go = over (#bonuses . #abilityscores . #strength . #magic) (addtemporary 4) oldstatus
bearstrengther t oldstatus
    | view (#effects . #bearStrength) oldstatus == NA = oldstatus
    | view (#effects . #bearStrength) oldstatus == Absent = set (#effects . #bearStrength) t go
    | view (#effects . #bearStrength) oldstatus < t = set (#effects . #bearStrength) t oldstatus
    | otherwise = oldstatus
        where
            go = over (#bonuses . #abilityscores . #strength . #magic) (addtemporary 4) oldstatus

stoneskiner :: Temporal (Maybe Int) -> Status -> Status
stoneskiner t oldstatus
    | view (#effects . #stoneSkin) oldstatus == NA = oldstatus
    | otherwise = set (#effects . #stoneSkin) t oldstatus

confusioner :: Temporal (Maybe Int) -> Status -> Status
confusioner t oldstatus
    | view (#effects . #confusion) oldstatus == NA = oldstatus
    | otherwise = set (#effects . #confusion) t oldstatus

blinder :: Temporal (Maybe Int) -> Status -> Status
blinder NA oldstatus
    | isPresent $ view (#effects . #blind) oldstatus = set (#effects . #blind) NA $ go
    | otherwise = set (#effects . #blind) NA $ oldstatus
        where
            go =  penalty (#bonuses . #defense . #nonarmour . #attacks . #penalty) (-2) oldstatus
blinder Absent oldstatus
    | isPresent $ view (#effects . #blind) oldstatus = set (#effects . #blind) Absent $ go
    | otherwise = oldstatus
        where
            go =  penalty (#bonuses . #defense . #nonarmour . #attacks . #penalty) (-2) oldstatus
blinder (LimitedOff a b) oldstatus
    | isPresent $ view (#effects . #blind) oldstatus = set (#effects . #blind) (LimitedOff a b) $ go
    | otherwise = set (#effects . #blind) (LimitedOff a b) $ oldstatus
        where
            go =  penalty (#bonuses . #defense . #nonarmour . #attacks . #penalty) (-2) oldstatus
blinder (LimitedOn a b) oldstatus
    | isLimitedOff $ view (#effects . #blind) oldstatus = set (#effects . #blind) (LimitedOn a b) $ go
    | otherwise = oldstatus
        where
            go =  penalty (#bonuses . #defense . #nonarmour . #attacks . #penalty) 2 oldstatus
blinder t oldstatus
    | view (#effects . #blind) oldstatus == NA = oldstatus
    | view (#effects . #blind) oldstatus == Absent = set (#effects . #blind) t go
    | view (#effects . #blind) oldstatus < t = set (#effects . #blind) t oldstatus
    | otherwise = oldstatus
        where
            go =  penalty (#bonuses . #defense . #nonarmour . #attacks . #penalty) 2 oldstatus

stealther :: Temporal (Maybe Int) -> Status -> Status
stealther t oldstatus
    | view (#effects . #stealth) oldstatus == NA = oldstatus
    | otherwise = set (#effects . #stealth) t oldstatus

bleedinger :: Temporal (Maybe Int) -> Status -> Status
bleedinger t oldstatus
    | view (#effects . #bleeding) oldstatus == NA = oldstatus
    | otherwise = set (#effects . #bleeding) t oldstatus

proner :: Temporal (Maybe Int) -> Status -> Status
proner NA oldstatus
    | isPresent $ view (#effects . #prone) oldstatus = set (#effects . #prone) NA $ go
    | otherwise = set (#effects . #prone) NA $ oldstatus
        where
            go =  penalty (#bonuses . #defense . #nonarmour . #attacks . #penalty) (-4) oldstatus
proner Absent oldstatus
    | isPresent $ view (#effects . #prone) oldstatus = set (#effects . #prone) Absent $ go
    | otherwise = oldstatus
        where
            go =  penalty (#bonuses . #defense . #nonarmour . #attacks . #penalty) (-4) oldstatus
proner (LimitedOff a b) oldstatus
    | isPresent $ view (#effects . #prone) oldstatus = set (#effects . #prone) (LimitedOff a b) $ go
    | otherwise = set (#effects . #prone) (LimitedOff a b) $ oldstatus
        where
            go =  penalty (#bonuses . #defense . #nonarmour . #attacks . #penalty) (-4) oldstatus
proner (LimitedOn a b) oldstatus
    | isLimitedOff $ view (#effects . #prone) oldstatus = set (#effects . #prone) (LimitedOn a b) $ go
    | otherwise = oldstatus
        where
            go =  penalty (#bonuses . #defense . #nonarmour . #attacks . #penalty) 4 oldstatus
proner t oldstatus
    | view (#effects . #prone) oldstatus == NA = oldstatus
    | view (#effects . #prone) oldstatus == Absent = set (#effects . #prone) t go
    | view (#effects . #prone) oldstatus < t = set (#effects . #prone) t oldstatus
    | otherwise = oldstatus
        where
            go =  penalty (#bonuses . #defense . #nonarmour . #attacks . #penalty) 4 oldstatus

uncannydodger :: Temporal (Maybe Int) -> Status -> Status
uncannydodger t oldstatus
    | view (#effects . #uncannydodge) oldstatus == NA = oldstatus
    | otherwise = set (#effects . #uncannydodge) t oldstatus

disarmer :: Temporal (Maybe Int) -> Status -> Status
disarmer NA oldstatus = set (#effects . #disarmed) NA $ oldstatus
disarmer Absent oldstatus
    | isPresent $ view (#effects . #disarmed) oldstatus = set (#effects . #disarmed) Absent $ oldstatus
    | otherwise = oldstatus
disarmer (LimitedOff a b) oldstatus
    | isPresent $ view (#effects . #disarmed) oldstatus = set (#effects . #disarmed) (LimitedOff a b) $ oldstatus
    | otherwise = oldstatus
disarmer (LimitedOn a b) oldstatus
    | isLimitedOff $ view (#effects . #disarmed) oldstatus = set (#effects . #disarmed) (LimitedOn a b) $ go
    | otherwise = oldstatus
        where
            go = set (#primaryhand) (Weapon Iron UnArmed (Just 0) []) $ over (#otherweapons) ([Right (view (#primaryhand) oldstatus)] ++) oldstatus
disarmer t oldstatus
    | view (#effects . #disarmed) oldstatus == NA = oldstatus
    | view (#effects . #disarmed) oldstatus == Absent = set (#effects . #disarmed) t go
    | view (#effects . #disarmed) oldstatus < t = set (#effects . #disarmed) t oldstatus
    | otherwise = oldstatus
        where
            go = set (#primaryhand) (Weapon Iron UnArmed (Just 0) []) $ over (#otherweapons) ([Right (view (#primaryhand) oldstatus)] ++) oldstatus

efix :: StatusEffect -> Temporal (Maybe Int) -> Status -> Status
efix Undead = undeader
efix Invisible = invisibler
efix ImmunetoCriticals = immunetocriticalser
efix MirrorImage = mirrorimager
efix TrueStrike = truestriker
efix GreaterEvasion = greaterevasioner
efix Evasion = evasioner
efix Sing = sing1er
efix Rage = rager
efix BearStrength = bearstrengther
efix StoneSkin = stoneskiner
efix Confusion = confusioner
efix Blind = blinder
efix Stealth = stealther
efix Bleeding = bleedinger
efix Prone = proner
efix UncannyDodge = uncannydodger
efix Disarmed = disarmer

effect2s2s :: Effect -> Status -> Status
effect2s2s (Effect s t) = efix s t

endtemporaryeffects :: Status -> Status
endtemporaryeffects s = over (#effects) durationreduction $ foldr (.) id (imap go (view (#effects) s)) s
  where
    go :: StatusEffect -> Temporal (Maybe Int) -> Status -> Status
    go se t
        | duration t == 0 = efix se Absent
        | otherwise = id
