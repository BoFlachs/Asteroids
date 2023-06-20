-- | This module defines the general structure of the back-end of the game
module Datastructure where

    import Graphics.Gloss
    import Data.Fixed
    import System.Random

    -- | Type that describes everything in the game
    data GameState  = GameState { world :: World
                                , gameInfo :: GameInfo 
                                , keyStates :: KeyStates
                                , randomG :: StdGen
                                , highScoreInfo :: HighScoreInfo}
                    deriving Show
    
    -- | Type that describes all the moving objects in the game
    data World      = World { friendly :: FShip
                            , enemies :: [EShip]
                            , asteroids :: [Asteroid]
                            , bullets :: [Bullet] 
                            , ebullets :: [EBullet]
                            }
                    deriving Show
        
    -- | Type that describes the general information about the current gamesession
    data GameInfo = GameInfo {score :: Score
                              , lives :: Lives
                              , level :: Level
                              , playername :: PlayerName
                              , gameModus :: GameModus
                              , status :: GameStatus}
                    deriving Show

    -- | Type that describes whether the arrows (and in multiplayer "a" and "d") are pressed down
    data KeyStates = KeyStates { uparrowDown :: Bool
                                , leftarrowDown :: Bool
                                , rightarrowDown :: Bool
                                , aDown :: Bool
                                , dDown :: Bool
                                }
                    deriving (Show, Eq)

    -- | Type that describes the current highscore-list and whether or not this list has to be written to the filesystem
    data HighScoreInfo = HighScoreInfo {highScores :: HighScore
                                        , doWrite :: DoWrite}
                    deriving Show

    -- | Type that describes the current status of the game
    data GameStatus = GameStart| Running | Paused | GameOver | ScoreScreen
                    deriving (Show, Eq)

    -- | Type that describes the current mode of the game
    data GameModus = SinglePlayer | MultiPlayer
                    deriving (Show, Eq)

    -- | Type that describes the number of lives left
    data Lives      = L0 | L1 | L2 | L3
                    deriving (Eq, Ord)

    -- | Type that tells the program whether or not to write the file to the system
    data DoWrite = GoWrite | NoWrite
                    deriving (Show, Eq)

    -- | These types are used quite often with built-in functions, so these are aliases for built-in types
    type Level = Float
    type PlayerName = String 
    type HighScore = [(Int, String)]

    -- | Player controlled friendly ship
    data FShip      = FShip {fsgeo :: GeoConfig
                            , fhitbox :: HitBox
                            , defence :: Defence
                            , fstatus :: Status
                            , fduration :: Duration
                            , ftimer :: AnimationTimer
                            , turret :: Turret}
                    deriving Show

    -- | Enemy ship that can shoot
    data EShip      = EShip {esgeo :: GeoConfig
                            , ehitbox :: HitBox
                            , estatus :: Status
                            , estimer :: AnimationTimer}
                    deriving Show

    -- | Asteroid that flies around
    data Asteroid   = Asteroid {ageo :: GeoConfig
                            , ahitbox :: HitBox
                            , astatus :: Status
                            , atimer :: AnimationTimer}
                    deriving Show

    -- | Bullet shot by the friendly ship
    data Bullet     = Bullet {bgeo :: GeoConfig
                            , bhitbox :: HitBox
                            , bstatus :: Status
                            , duration :: Duration}
                    deriving Show

    -- | Bullet shot by the enemy ship
    data EBullet    = EBullet {ebgeo :: GeoConfig
                            , ebhitbox :: HitBox
                            , ebstatus :: Status
                            , eduration :: Duration}
                    deriving Show

    -- | Turret on top of the friendly ship that shoots
    data Turret = Turret {tgeo :: GeoConfig}
                    deriving Show

    -- | Type that determines how long a bullet flies until it dies
    type Duration = Int

    -- | Type that determines how long an animation is run
    type AnimationTimer = Float

    -- | Type that id's the objects (used only when checking destruction)
    data Identifier = FShipID | EShipID | AsteroidID | BulletID | EBulletID
                    deriving Eq
    
    -- | The friendly ship can't die or shoot if it's invincible (right after spawning)
    data Defence    = Invincible | Normal
                    deriving (Show, Eq)   
        
    -- | All objects have a paramater that states whether they are dead
    data Status     = Alive | Dead
                    deriving (Show, Eq)  

    -- | Type that describes the geometrical configuration of an object
    data GeoConfig  = GeoConfig {location :: Location
                                , size :: Size
                                , speed :: Speed
                                , angle :: Angle}
                                deriving (Show, Eq)

    -- | Types for keeping track of the game's score and the size, speed and angle of objects
    newtype Score   = Score Int
        deriving (Show, Eq, Ord)
    newtype Size    = Size Float
        deriving (Show, Eq, Ord)
    newtype Speed   = Speed Float
        deriving (Show, Eq, Ord)
    newtype Angle   = Angle {unAngle ::Float}
        deriving (Show, Eq, Ord)

    -- | Type that describes the location of an object
    data Location   = Location Float Float
                    deriving (Show, Eq)

    -- | Type that describes a square in which an object can be shot down
    data HitBox     = HitBox Location Location
                    deriving (Show)

    -- | Typeclass for moving, turning and accelerating
    class Movable a where
        changeSpeed :: a -> Speed -> a
        changeDir :: a -> Angle -> a
        move :: a -> a
        changeSpeed x _ = x
        changeDir x _ = x
        move x = x
    
    {- Typeclasses -}
    -- | Typeclass for checking whether something is destructed and handle the consequences 
    class Destructible a where
        destructedBy :: HasHitbox b => a -> b -> Bool
        die :: a -> a
        getScore :: a -> Score
        getScore x = Score 0

    -- | Typeclass for getting useful information about the hitbox from an object with an hitbox
    class HasHitbox a where
        getHitbox :: a -> (HitBox, Identifier)
        hasHitBox :: a -> Bool
        getStatus :: a -> Status
        hasHitBox x = True

    -- | Typeclass for objects that can shoot a bullet
    class CanShoot a where
        shoot :: GameState -> a -> GameState
        
    -- | Typeclass for objects that are made into a picture
    class Renderable a where
        render :: a -> Picture

    -- | Helper typeclass to have easy access to the geometrical parameters of an object
    class HasGeoConfig a where
        getLocation :: a -> Location
        getAngle :: a -> Angle
        getSpeed :: a -> Speed
        getSize :: a -> Size
        wrapLocation :: a -> Location -> a

    -- | Helper typeclass to do some arithmetics on own defined datatypes
    class Arithmeticable a where
        add :: a -> a -> a
        mod_ :: a -> a -> a

    -- | Typeclass for things that can intersect with a hitbox
    class Intersectable a where
        intersect :: a -> HitBox -> Bool

{- Standard values for certain types -}
    bulletSpeed :: Speed
    bulletSpeed = Speed 8

    bulletSize :: Size
    bulletSize = Size 5

    bulletDuration :: Duration
    bulletDuration = 30

    ebulletDuration :: Duration
    ebulletDuration = 15

    bulletHitBox :: HitBox
    bulletHitBox = HitBox (Location 0 0) (Location 5 5)

    animationDurationF :: Float
    animationDurationF = 30

    animationDurationE :: Float
    animationDurationE = 6

    maxSpeed :: Speed
    maxSpeed = Speed 10

    minSpeed :: Speed 
    minSpeed = Speed 0 

    invincibleDuration :: Duration
    invincibleDuration = 30

    bigAsteroid :: Size
    bigAsteroid = Size 5

    medAsteroid :: Size
    medAsteroid = Size 3

    smallAsteroid :: Size
    smallAsteroid = Size 2

    {- Helper instances  -}
    instance Show Lives where
        show L0 = "Lives: 0"
        show L1 = "Lives: 1"
        show L2 = "Lives: 2"
        show L3 = "Lives: 3"

    instance Arithmeticable Speed where
        (Speed x) `add` (Speed y) = Speed (x + y)
        (Speed x) `mod_` (Speed y) = Speed (x `mod'` y)

    instance Arithmeticable Score where
        (Score x) `add` (Score y) = Score (x + y)
        (Score x) `mod_` (Score y) = Score (x `mod'` y)

    instance Arithmeticable Angle where 
        (Angle x) `add` (Angle y) = Angle((x + y) `mod'` 360)
        (Angle x) `mod_` (Angle y) = Angle(x `mod'` y)

    -- | Checks whether two hitboxes intersect
    instance Intersectable HitBox where
        intersect (HitBox (Location x1 y1) (Location u1 v1)) (HitBox (Location x2 y2) (Location u2 v2)) = ((y2 <= v1 && y2 >= y1) || 
                                                                                                                    (v2 <= v1 && v2 >= y1)) &&
                                                                                                                        ((x2 >= x1 && x2 <= u1) ||
                                                                                                                            (u2 >= x1 && u2 <= u1))

    {- Hulpmethodes voor instances -}
    -- | Increases or deacreses the speed as long as it stays between the minimum and maximum speed
    addSpeed :: Speed -> GeoConfig -> GeoConfig
    addSpeed x g  | speed g `add` x <= minSpeed = g {speed = minSpeed}
                  | speed g `add` x >= maxSpeed = g {speed = maxSpeed}
                  | otherwise = g {speed = speed g `add` x}

    addAngle :: Angle -> GeoConfig -> GeoConfig
    addAngle x g = g {angle = angle g `add` x}

    toRadian :: Float -> Float
    toRadian x = (pi /180) * x

    -- | Changes the location according to the current speed and angle
    changeLocation :: Location -> Speed -> Angle -> Location
    changeLocation (Location x y) (Speed s) (Angle a) = Location (x + s * sin a') (y + s * cos a')
        where a' = toRadian a

    -- | Changes the location of an hitbox given a new location
    changeHitLocation :: HitBox -> Location -> HitBox
    changeHitLocation (HitBox (Location x1 y1) (Location x2 y2)) (Location x y) = HitBox (Location a b) (Location c d)
            where   dx = x2 - x1
                    dy = y2 - y1
                    a = x - (dx/2)
                    b = y - (dy/2)
                    c = x + (dx/2)
                    d = y + (dy/2)

    -- | Helper functions to get easy acces to the x and y coordinate of an objects location 
    getx :: HasGeoConfig a => a -> Float
    getx obj = takex (getLocation obj)
        where takex (Location a _) = a
    
    gety :: HasGeoConfig a => a -> Float
    gety obj = takey (getLocation obj)
        where takey (Location _ b) = b

    {- Instances voor Turret -}
    instance HasGeoConfig Turret where
        getLocation (Turret gc) = location gc
        getAngle (Turret gc) = angle gc
        getSpeed (Turret gc) = speed gc
        getSize (Turret gc) = size gc
        wrapLocation t lc = t{tgeo = (tgeo t){location = lc}}

    instance Movable Turret where
        changeDir t a = t {tgeo = addAngle a (tgeo t)}

    {- Instances voor FShip -}
    instance HasGeoConfig FShip where
        getLocation (FShip gc _ _ _ _ _ _) = location gc 
        getAngle (FShip gc _ _ _ _ _ _) = angle gc 
        getSpeed (FShip gc _ _ _ _ _ _) = speed gc
        getSize (FShip gc _ _ _ _ _ _) = size gc
        wrapLocation fs lc = fs {fsgeo = (fsgeo fs){location = lc}}

    -- | When a friendly ship is invincible it can't shoot. Otherwise, a new bullet is added in to the gamestate using the angle and speed 
    -- | of the friendly ship
    instance CanShoot FShip where
        shoot gstate (FShip _ _ Invincible _ _ _ _) = gstate
        shoot gstate fs = gstate {world = addBullet b' (world gstate)}
            where addBullet b w = w { bullets = b : bullets w }
                  b' = Bullet (GeoConfig {location = getLocation fs, speed = newSpeed, 
                                    size = bulletSize, angle = getAngle (turret fs)}) (setHit (getLocation fs)) Alive bulletDuration
                  setHit = changeHitLocation bulletHitBox 
                  newSpeed | (getAngle fs) == (getAngle (turret fs)) = bulletSpeed `add` getSpeed fs
                           | otherwise = bulletSpeed

    instance HasHitbox FShip where
        hasHitBox (FShip _ _ Invincible _ _ _ _) = False
        hasHitBox (FShip _ _ Normal _ _ _ _) = True
        getHitbox fs = (fhitbox fs, FShipID)
        getStatus fs = fstatus fs

    -- | A friendly ship moves to the newlocation using the speed and angle. If the ship is Invincible the timer counts down until it's normal again.
    -- | If the ship is Normal remains normal during movement.
    instance Movable FShip where
        changeSpeed fs s = fs {fsgeo = addSpeed s (fsgeo fs)}
        changeDir fs a = fs {fsgeo = addAngle a (fsgeo fs)}
        move fs | fduration fs == 0 = fs {fsgeo = ((fsgeo fs) {location = newlocation})
                                            , fhitbox =  changeHitLocation (fhitbox fs) newlocation, defence = Normal
                                            , turret = newTurret (turret fs) newlocation}
                | otherwise = fs {fsgeo = ((fsgeo fs) {location = newlocation})
                                    , fhitbox =  changeHitLocation (fhitbox fs) newlocation, defence = Invincible, fduration = fduration fs - 1
                                    , turret = newTurret (turret fs) newlocation}
                where newlocation =  changeLocation (getLocation fs) (getSpeed fs) (getAngle fs)
                      newTurret t lc = t{tgeo = (tgeo t){location = newlocation}}

    -- | A friendly ship can only be destructed by something that is alive and not by it's own bullets.
    instance Destructible FShip where
        fs@(FShip _ _ Invincible _ _ _ _) `destructedBy` _ = False
        fs `destructedBy` x | (getStatus x) == Dead = False
                            | snd (getHitbox x) == BulletID = False
                            | otherwise = fhitbox fs `intersect` fst (getHitbox x)
        die fs = fs{fstatus = Dead}

        
    {- Instances voor EShip -}
    instance HasGeoConfig EShip where
        getLocation (EShip gc _ _ _) = location gc 
        getAngle (EShip gc _ _ _) = angle gc 
        getSpeed (EShip gc _ _ _) = speed gc
        getSize (EShip gc _ _ _) = size gc
        wrapLocation es lc = es {esgeo = (esgeo es){location = lc}}

    -- | When an enemy shoots an enemybullet (EBullet) is added using the enemy's angle and speed
    instance CanShoot EShip where
        shoot gstate es | (estimer es) == 0 = gstate {world = addBullet b' (world gstate)}
                        | otherwise = gstate
            where   addBullet b w = w { ebullets = b : ebullets w }
                    b' = EBullet (GeoConfig {location = getLocation es, speed = bulletSpeed, 
                                    size = bulletSize, angle = getAngle es}) (setHit (getLocation es)) Alive ebulletDuration
                    setHit = changeHitLocation bulletHitBox

    instance HasHitbox EShip where
        getHitbox es = (ehitbox es, EShipID)
        getStatus es = estatus es

    -- | When an enemy doesn't follow the friendly ship it moves in a continously changing direction
    instance Movable EShip where
        changeSpeed es s = es {esgeo = addSpeed s (esgeo es)}
        changeDir es a = es {esgeo = addAngle a (esgeo es)}
        move es = es {esgeo = ((esgeo es) {location = changeLocation (getLocation es) (getSpeed es) (getAngle es), angle = newangle})
                        , ehitbox = changeHitLocation (ehitbox es) newlocation}
                    where newlocation =  changeLocation (getLocation es) (getSpeed es) (getAngle es)
                          newangle = (getAngle es `add` Angle 1) `mod_` Angle 350

    -- | An enemy can only be destructed by a friendly bullet. When it dies an animation is started.
    instance Destructible EShip where
        es `destructedBy` x | (getStatus x) == Dead || (getStatus es) == Dead = False
                            | snd (getHitbox x) == BulletID = ehitbox es `intersect` fst (getHitbox x)
                            | otherwise = False
        getScore es | (estimer es) == 0 = Score 500
                    | otherwise = Score 0
        die es = es{estatus = Dead, estimer = animationDurationE}

    {- instances voor Asteroid -}       
    instance HasGeoConfig Asteroid where
        getLocation (Asteroid gc _ _ _) = location gc 
        getAngle (Asteroid gc _ _ _) = angle gc 
        getSpeed (Asteroid gc _ _ _) = speed gc
        getSize (Asteroid gc _ _ _) = size gc
        wrapLocation as lc = as {ageo = (ageo as){location = lc}}

    instance HasHitbox Asteroid where
        getHitbox as = (ahitbox as, AsteroidID)
        getStatus as = astatus as

    -- | An asteroid moves in a straight line 
    instance Movable Asteroid where
        move a = a {ageo = ((ageo a) {location = changeLocation (getLocation a) (getSpeed a) (getAngle a)})
                    , ahitbox = changeHitLocation (ahitbox a) newlocation}
                    where newlocation =  changeLocation (getLocation a) (getSpeed a) (getAngle a)

    -- | An asteroid can only be destructed by friendly bullets. When it dies an animation is started.
    instance Destructible Asteroid where
        as `destructedBy` x | (getStatus x) == Dead || (getStatus as) == Dead = False
                            | snd (getHitbox x) == BulletID = ahitbox as `intersect` fst (getHitbox x)
                            | otherwise = False
        getScore as | (atimer as) > 0 = Score 0
                    | size (ageo as) == bigAsteroid = Score 100
                    | size (ageo as) == medAsteroid = Score 50
                    | size (ageo as) == smallAsteroid = Score 20
        die as = as {astatus = Dead, atimer = animationDurationE}

    {- Instances voor Bullet -}
    instance HasGeoConfig Bullet where
        getLocation (Bullet gc _ _ _) = location gc 
        getAngle (Bullet gc _ _ _) = angle gc 
        getSpeed (Bullet gc _ _ _) = speed gc
        getSize (Bullet gc _ _ _) = size gc
        wrapLocation b lc = b {bgeo = (bgeo b){location = lc}}

    instance HasHitbox Bullet where
        getHitbox b = (bhitbox b, BulletID)
        getStatus b = bstatus b

    -- | A bullet moves as long as its duration hasn't ended. If it has, the bullet dies
    instance Movable Bullet where
        move b  | duration b == 0 = die b
                | otherwise =  b {bgeo = ((bgeo b) {location = changeLocation (getLocation b) (getSpeed b) (getAngle b)})
                                    , bhitbox = changeHitLocation (bhitbox b) newlocation, duration = duration b - 1}
                                    where newlocation =  changeLocation (getLocation b) (getSpeed b) (getAngle b)

    -- | A bullet dies if it hits anything that's not a friendly ship or another friendly bullit
    instance Destructible Bullet where
        b `destructedBy` x  | snd (getHitbox x) == FShipID || snd (getHitbox x) == BulletID = False
                            | otherwise = fst (getHitbox x) `intersect` bhitbox b
        die b = b {bstatus = Dead}

    {- instances voor EBullet -}
    instance HasGeoConfig EBullet where
        getLocation (EBullet gc _ _ _) = location gc 
        getAngle (EBullet gc _ _ _) = angle gc 
        getSpeed (EBullet gc _ _ _) = speed gc
        getSize (EBullet gc _ _ _) = size gc
        wrapLocation eb lc = eb {ebgeo = (ebgeo eb){location = lc}}

    instance HasHitbox EBullet where
        getHitbox eb = (ebhitbox eb, EBulletID)
        getStatus eb = ebstatus eb

    -- | An enemy bullet moves as long as its duration hasn't ended. If it has, the enemy bullet dies
    instance Movable EBullet where
        move eb | eduration eb == 0 = die eb
                | otherwise = eb {ebgeo = ((ebgeo eb) {location = changeLocation (getLocation eb) (getSpeed eb) (getAngle eb)})
                        , ebhitbox = changeHitLocation (ebhitbox eb) newlocation, eduration = eduration eb -1}
                        where newlocation =  changeLocation (getLocation eb) (getSpeed eb) (getAngle eb)

    -- | An enemy bullet dies if it hits the friendly ship
    instance Destructible EBullet where
        eb `destructedBy` x | (getStatus x) == Dead = False
                            | snd (getHitbox x) == FShipID = ebhitbox eb `intersect` fst (getHitbox x)
                            | otherwise = False
        die eb = eb {ebstatus = Dead}