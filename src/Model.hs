-- | This module makes an initialstate or a new state when called upon
module Model where

    import Datastructure 
    import System.Random
    import Highscore

    {- Standard declarations for an inital gamestate that is altered when making a new state-}
    fGeo :: GeoConfig
    fGeo = GeoConfig (Location 25 25 ) (Size 5) (Speed 0) (Angle 0)

    someName :: PlayerName
    someName = ""

    someHitbox :: HitBox
    someHitbox = HitBox (Location 0 0) (Location 35 35)

    someTurret :: Turret
    someTurret = Turret fGeo

    someFShip :: FShip
    someFShip = FShip fGeo someHitbox Invincible Alive invincibleDuration animationDurationF someTurret

    someGameInfo :: GameInfo
    someGameInfo = GameInfo (Score 0) L3 1 someName SinglePlayer GameStart

    someKeystate :: KeyStates
    someKeystate = KeyStates False False False False False

    someRandom :: StdGen
    someRandom = undefined

    initialState :: GameState
    initialState = GameState (World someFShip [] [] [] []) someGameInfo someKeystate someRandom (HighScoreInfo [] NoWrite)

    -- | Given the standardgenerator and the highscore-list in main this function creates a level 1 gamestate
    makeInitialState :: StdGen -> HighScore -> GameState
    makeInitialState r high = makeNewState initialState{randomG = r, highScoreInfo = HighScoreInfo high NoWrite} 1

    -- | Given a gamestate and a level this function creates increases the level, the number of enemies and asteroids
    makeNewState :: GameState -> Float -> GameState
    makeNewState gstate n = let (xs, g) = makeRandomList (randomG gstate) (300.0, 300.0) (n+2)
                                (ys, g1) = makeRandomList g (-300.0, 300.0) (n+2)
                                (an, g2) = makeRandomList g1 (0, 360) (n+2)
                                (xs', g3) = makeRandomList g2 (299.0, 300.0) n
                                (ys', g4) = makeRandomList g3 (-300.0, 300.0) n
                                (an', g5) = makeRandomList g4 (0, 360) n
                            in gstate{world = (world gstate){ enemies = makeEShipList xs' ys' an' n
                                  , asteroids = makeAsteroidList xs ys an (n+2)
                                        }, randomG = g4, gameInfo = (gameInfo gstate){level = n, playername = someName}}

    -- | Creates a list of random values together with a new generator
    makeRandomList :: StdGen -> (Float, Float) -> Float -> ([Float], StdGen)
    makeRandomList g _ 0 = ([], g)
    makeRandomList g (h,l) n = let (a, g') = randomR (h,l) g 
                                   (xs, gen) = makeRandomList g' (h,l) (n-1)
                                  in (a : xs, gen)

    -- | Given a list of x-coordinates, y-coordinates and angles and a number n this function creates n enemies
    makeEShipList :: [Float] -> [Float] -> [Float] -> Float -> [EShip]
    makeEShipList _ _ _ 0 = []
    makeEShipList (x:xs) (y:ys) (u:us) n = makeEShip x y u : makeEShipList xs ys us (n-1)
    makeEShipList _ _ _ n = []
 
    -- | Given a list of x-coordinates, y-coordinates and angles and a number n this function creates n asteroids
    makeAsteroidList :: [Float] -> [Float] -> [Float] -> Float -> [Asteroid]
    makeAsteroidList _ _ _ 0 = [] 
    makeAsteroidList (x:xs) (y:ys) (u:us) n = makeAsteroid x y u : makeAsteroidList xs ys us (n-1)
    makeAsteroidList _ _ _ n = []

    -- | Creates an asteroid using the random values given by makeRandomList through makeAsteroidList
    makeAsteroid :: Float -> Float -> Float -> Asteroid
    makeAsteroid x y ang = Asteroid (makeGeo x y bigAsteroid 3 ang) (makeHitBox x y 50 50) Alive 0

    -- | Creates an asteroid using the random values given by makeRandomList through makeAsteroidList
    makeEShip :: Float -> Float -> Float -> EShip
    makeEShip x y ang = EShip (makeGeo x y (Size 5) 1 ang) (makeHitBox x y 60 40) Alive 0
  
    -- | Helper function to create a new geometrical configuration
    makeGeo :: Float -> Float -> Size -> Float -> Float -> GeoConfig
    makeGeo x y si sp an = GeoConfig (Location x y) si (Speed sp) (Angle an)

    -- | Helper function to create a hitbox around the location of an object
    makeHitBox :: Float -> Float -> Float -> Float -> HitBox
    makeHitBox x y w h = HitBox (Location a b) (Location c d)
            where   a = x - (w/2)
                    b = y - (h/2)
                    c = x + (w/2)
                    d = y + (h/2)

