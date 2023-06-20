-- | This module defines how the state changes
--   in response to time and user input
module Controller where

    import Model
    
    import Graphics.Gloss
    import Graphics.Gloss.Interface.IO.Game
    import System.Random
    import Datastructure
    import Highscore
    import Data.Char
    
    -- | Function that changes the gamestate every step
    step :: Float -> GameState -> IO GameState

    -- | Don't do anything before the game has started, when the game is paused, when the highscores are shown or while the Playername is being given
    step secs gstate@(GameState _ (GameInfo _ _ _ _ _ GameStart) _ _ _) = return gstate
    step secs gstate@(GameState _ (GameInfo _ _ _ _ _ Paused) _ _ _) = return gstate
    step secs gstate@(GameState _ (GameInfo _ _ _ _ _ GameOver) _ _ (HighScoreInfo _ NoWrite)) = return gstate
    step secs gstate@(GameState _ (GameInfo _ _ _ _ _ ScoreScreen) _ _ _) = return gstate

    -- | Write the new highscore-file with the last score inserted. Then show the top 5 of highscores
    step secs gstate@(GameState _ (GameInfo s _ _ name _ GameOver) _ _ (HighScoreInfo h GoWrite)) = do writeHighScore (take 5 $ addHighScore h s name)                                                                                                 
                                                                                                       return gstate{highScoreInfo = 
                                                                                                            HighScoreInfo (take 5 $ addHighScore h s name) NoWrite, 
                                                                                                                gameInfo = (gameInfo gstate){status = ScoreScreen}}

    -- | If all enemies are defeated, create the next (and harder) level
    step secs gstate@(GameState (World _ [] [] _ _) (GameInfo _ _ n _ _ Running) _ _ _) = return (makeNewState gstate (n + 1))

    -- | Move all objects, check if anything died after this move and act accordingly if so
    step secs gstate
      = return $ randomFollowShoot(alterDead (gstate{world = 
                                    makeDead(world gstate){ 
                                                    friendly = keyboardInput gstate (move (checkLocation (friendly(world gstate)))), 
                                                    enemies = map move (map checkLocation (enemies(world gstate))), 
                                                    asteroids = map move (map checkLocation (asteroids(world gstate))), 
                                                    bullets = map move (map checkLocation (bullets(world gstate))),
                                                    ebullets = map move (map checkLocation (ebullets(world gstate)))
                                                    }}))

    -- | Function that moves the player's ship. While in singleplayer the turret that shoots is always pointing in the same direction as the ship
    keyboardInput :: GameState -> FShip -> FShip
    keyboardInput (GameState _ (GameInfo _ _ _ _ SinglePlayer _) (KeyStates up left right _ _) _ _) fs
                        | up && left = newT $ changeDir (changeSpeed fs (Speed 0.5)) (Angle (-10)) 
                        | up && right = newT $ changeDir (changeSpeed fs (Speed 0.5)) (Angle 10) 
                        | up = changeSpeed fs (Speed 0.5)
                        | left = newT $ changeDir (changeSpeed fs (Speed (-0.3))) (Angle (-10))
                        | right = newT $ changeDir (changeSpeed fs (Speed (-0.3))) (Angle 10)
                        | otherwise = changeSpeed fs (Speed (-0.3))
            where newT fs = fs{turret = (turret fs){tgeo = (tgeo (turret fs)){angle = getAngle fs}}}

    -- | While in multiplayer the turret turns independently of the rest of the ship
    keyboardInput (GameState _ (GameInfo _ _ _ _ MultiPlayer _) (KeyStates up left right a d) _ _) fs
                        | up && left = newTur a d $ changeDir (changeSpeed fs (Speed 0.5)) (Angle (-10)) 
                        | up && right = newTur a d $ changeDir (changeSpeed fs (Speed 0.5)) (Angle 10) 
                        | up = newTur a d $ changeSpeed fs (Speed 0.5)
                        | left = newTur a d $ changeDir (changeSpeed fs (Speed (-0.3))) (Angle (-10))
                        | right = newTur a d $ changeDir (changeSpeed fs (Speed (-0.3))) (Angle 10)
                        | otherwise = newTur a d $ changeSpeed fs (Speed (-0.3))
            where newTur l r fs | l && r = fs
                                | l = fs{turret = changeDir (turret fs) (Angle (-10))}         
                                | r = fs{turret = changeDir (turret fs) (Angle 10)}    
                                | otherwise = fs             

    -- | Using the random generator the enemyships shoot at a random moment. The rate in which they shoot is dependent of the current level
    -- | These ships move towards the friendly ship on random moments. This rate is also dependent of the current level
    randomFollowShoot :: GameState -> GameState
    randomFollowShoot gstate | fst $ odds 0.05 = move'((shoot' gstate (enemies (world gstate)) 
                                                            (snd $ odds 0.3)){randomG = (snd $ odds 0.3)}) (enemies (world gstate))
                             | otherwise = gstate{randomG = snd $ odds 0.3}
            where odds n = chance (n * level (gameInfo gstate)) (randomG gstate)
                  chance :: Float -> StdGen -> (Bool, StdGen)
                  chance n g = let (a, g') = randomR (0, 1) g 
                                    in (a < n, g')
                  shoot' :: GameState -> [EShip] -> StdGen -> GameState
                  shoot' gstate [] r = gstate
                  shoot' gstate xs r = shoot gstate (xs!! fst(randomR (0, length xs - 1) r))

    -- | Make all ships move towards the friendly ship
    move' :: GameState -> [EShip] -> GameState
    move' gstate [] = gstate
    move' gstate xs = gstate{world = (world gstate){enemies = map (\x -> x{esgeo = (esgeo x){angle = 
                                                                    newAngle (getLocation x) (getLocation(friendly(world gstate)))}})  xs}}
                where   newAngle (Location x y) (Location x' y') | y > y' && x > x' = Angle (180 + toAngle (atan (abs(x - x') / abs(y - y')) ))
                                                                 | y > y' = Angle (90 + toAngle (atan (abs(x - x') / abs(y - y')) ))
                                                                 | x > x' = Angle (270 + toAngle (atan (abs(x - x') / abs(y - y')) ))
                                                                 | otherwise = Angle(toAngle (atan (abs(x - x') / abs(y - y')) ))
                        toAngle x = (180/pi) * x
                       
    -- | For any object that is destructible, check if another object destroys it. If so turn it's status into Dead
    checkDead :: Destructible a => [EShip] -> [Asteroid] -> [Bullet] -> [EBullet] -> a -> a
    checkDead es as bs ebs obj | any f es || any g as || any h bs || any i ebs = die obj
                               | otherwise = obj
        where   f x = obj `destructedBy` x
                g x = obj `destructedBy` x
                h x = obj `destructedBy` x
                i x = obj `destructedBy` x

    -- | Check all objects for destruction and change the status accordingly
    makeDead :: World -> World
    makeDead w = w {friendly = checkDead ew asw bw ebw fs
                    , enemies = map (checkDead ew asw bw ebw) ew
                    , asteroids = map (checkDead ew asw bw ebw) asw
                    , bullets = map (checkDead ew asw bw ebw) bw
                    , ebullets = map (checkDead ew asw bw ebw) ebw
                    }
        where   fs = friendly w
                ew = enemies w
                asw = asteroids w
                bw = bullets w
                ebw = ebullets w

    -- | If the friendly ship has died. Respawn the ship and decrease the number of lives. If there are no lives left, go to the 'GameOver-sequence'
    -- | Remove all objects that have died and update the score accordingly
    alterDead :: GameState -> GameState
    alterDead gstate@(GameState _ (GameInfo s l _ _ _ _) _ _ _)
      | checkFShip gstate && l == L3 = gstate {world = (world gstate){friendly = someFShip},gameInfo = (gameInfo gstate){lives = L2}}
      | checkFShip gstate && l == L2 = gstate {world = (world gstate){friendly = someFShip},gameInfo = (gameInfo gstate){lives = L1}}
      | checkFShip gstate && l == L1 = gstate {gameInfo = (gameInfo gstate){status = GameOver}}
      | otherwise =  gstate {world =
                          (world gstate) {
                                          friendly = (friendly(world gstate)){ftimer = newtimer (ftimer(friendly(world gstate)))},
                                          enemies = killEShips(enemies(world gstate)), 
                                          asteroids = killAsteroids(asteroids(world gstate)), 
                                          bullets = killBullets(bullets(world gstate)),
                                          ebullets = killEBullets(ebullets(world gstate))
                          }, gameInfo = (gameInfo gstate){score = s `add` getScores gstate}}
            where newtimer 0 = 0
                  newtimer n = n -1

    -- | Function that gives scores when objects are killed
    getScores :: GameState -> Score
    getScores gstate = foldr f (Score 0) (enemies(world gstate)) `add` foldr h (Score 0) (asteroids(world gstate))
        where f x y = foo x `add` y
              foo x | estatus x == Dead = getScore x
                    | otherwise = Score 0
              h x y = hoo x `add` y
              hoo x | astatus x == Dead = getScore x
                    | otherwise = Score 0

    -- | Function that checks whether the fship is alive or not
    checkFShip :: GameState -> Bool
    checkFShip gstate
      | fstatus(friendly(world gstate)) == Alive = False
      | otherwise = True

    -- | Function that removes dead enemyships
    killEShips :: [EShip] -> [EShip]
    killEShips [] = []
    killEShips (x:xs)
      | estatus x == Dead && estimer x == 0 = killEShips xs
      | estimer x == 0 = x : killEShips xs
      | otherwise = x{estimer = estimer x- 1} : killEShips xs
    

      -- | Function that removes dead enemyships
    killBullets :: [Bullet] -> [Bullet]
    killBullets [] = []
    killBullets (x:xs)
      | bstatus x == Dead = killBullets xs
      | otherwise = x : killBullets xs

      -- | Function that removes dead enemyships
    killEBullets :: [EBullet] -> [EBullet]
    killEBullets [] = []
    killEBullets (x:xs)
      | ebstatus x == Dead = killEBullets xs
      | otherwise = x : killEBullets xs

    -- | Function that removes all dead asteroids and spawns smaller asteroids if needed
    killAsteroids :: [Asteroid] -> [Asteroid]
    killAsteroids [] = []
    killAsteroids (x:xs)
      | astatus x == Dead && atimer x > 0 = x{atimer = atimer x - 1} : killAsteroids xs
                      
      | astatus x == Dead && size(ageo x) == bigAsteroid =
                     [Asteroid (GeoConfig {location = getLocation x, speed = Speed 3, size = medAsteroid, 
                                angle = getAngle x `add` Angle 45}) (medHitbox x) Alive 0, 
                        Asteroid (GeoConfig {location = getLocation x, speed = Speed 3, size = medAsteroid, 
                                    angle = getAngle x `add` Angle (-55)}) (medHitbox x) Alive 0] ++ killAsteroids xs
                        
      | astatus x == Dead && size(ageo x) == medAsteroid =
                     [Asteroid (GeoConfig {location = getLocation x, speed = Speed 3, size = smallAsteroid, 
                                angle = getAngle x `add` Angle 65 }) (smallHitbox x) Alive 0, 
                      Asteroid (GeoConfig {location = getLocation x, speed = Speed 3, size = smallAsteroid, 
                                    angle = getAngle x `add` Angle (-30)}) (smallHitbox x) Alive 0] ++ killAsteroids xs
                      
      | astatus x == Dead && size(ageo x) == smallAsteroid = killAsteroids xs 
      | otherwise = x : killAsteroids xs
                    where medHitbox a = changeHitLocation (HitBox (Location 0 0) (Location 40 40)) (getLocation a)
                          smallHitbox a = changeHitLocation (HitBox (Location 0 0) (Location 30 30)) (getLocation a)

    -- | Helper function that wraps objects at the edge of the screen
    checkLocation :: HasGeoConfig a => a -> a
    checkLocation obj
      | getx obj > 300 = wrapLocation obj (Location (-300) (gety obj))
      | getx obj < (-300) = wrapLocation obj (Location 300 (gety obj))
      | gety obj > 300 = wrapLocation obj (Location (getx obj) (-300))
      | gety obj < (-300) = wrapLocation obj (Location (getx obj) 300)
      | otherwise = obj
    
    -- | Handle user input
    input :: Event -> GameState -> IO GameState
    input e gstate = return (inputKey e gstate)

    inputKey :: Event -> GameState -> GameState
    inputKey (EventKey (SpecialKey k) ks _ _) gstate@(GameState _ (GameInfo _ _ _ _ _ Running) _ _ _) 
      -- | Change gamestate with up arrow
      | SpecialKey KeyUp <- SpecialKey k
          , Down <- ks = gstate{keyStates = (keyStates gstate){uparrowDown = True}}
      | SpecialKey KeyUp <- SpecialKey k
          , Up <- ks = gstate{keyStates = (keyStates gstate){uparrowDown = False}}

      -- | Change gamestate with right arrow
      | SpecialKey KeyRight <- SpecialKey k
          , Down <- ks = gstate{keyStates = (keyStates gstate){rightarrowDown = True}}
      | SpecialKey KeyRight <- SpecialKey k
          , Up <- ks = gstate{keyStates = (keyStates gstate){rightarrowDown = False}}

      -- | Change gamestate with left arrow    
      | SpecialKey KeyLeft <- SpecialKey k
          , Down <- ks = gstate{keyStates = (keyStates gstate){leftarrowDown = True}}
      | SpecialKey KeyLeft <- SpecialKey k
          , Up <- ks = gstate{keyStates = (keyStates gstate){leftarrowDown = False}}

      -- | Shoot with space
      | SpecialKey KeySpace <- SpecialKey k
          , Down <- ks = shoot gstate (friendly(world gstate))

    -- | When in multiplayer, handle the controls for turning the turret
    inputKey (EventKey (Char k) ks _ _) gstate@(GameState _ (GameInfo _ _ _ _ MultiPlayer Running) _ _ _)
      | Char 'a' <- Char k
        , Down <- ks = gstate{keyStates = (keyStates gstate){aDown = True}}
      | Char 'a' <- Char k
        , Up <- ks = gstate{keyStates = (keyStates gstate){aDown = False}}
      | Char 'd' <- Char k
        , Down <- ks = gstate{keyStates = (keyStates gstate){dDown = True}}
      | Char 'd' <- Char k
        , Up <- ks = gstate{keyStates = (keyStates gstate){dDown = False}}

    -- | Start game
    inputKey (EventKey (Char k) ks _ _) gstate@(GameState _ (GameInfo _ _ _ _ _ GameStart) _ _ _)
        | Char 'm' <- Char k
            , Down <- ks = gstate{gameInfo = (gameInfo gstate){status = Running, gameModus = MultiPlayer}}
        | Char 's' <- Char k
            , Down <- ks = gstate{gameInfo = (gameInfo gstate){status = Running, gameModus = SinglePlayer}}
                 
    -- | If GameOver write playername for the highscore
    inputKey (EventKey (Char c) ks _ _) gstate@(GameState _ (GameInfo _ _ _ name _ GameOver) _ _ _)
        | Down <- ks , length name < 3 = gstate{gameInfo = (gameInfo gstate){playername = name ++ [toUpper c]}}

    -- | When the name is inserted and enter is pressed. Give the signal to write the new highscore-file
    inputKey (EventKey (SpecialKey k) ks _ _) gstate@(GameState _ (GameInfo _ _ _ name _ GameOver) _ _ _)
        | SpecialKey KeyEnter <- SpecialKey k
            , Down <- ks = gstate{highScoreInfo = (highScoreInfo gstate){doWrite = GoWrite}}
        | SpecialKey KeyDelete <- SpecialKey k
            , Down <- ks = gstate{gameInfo = (gameInfo gstate){playername = backSpacer name}}

    -- | When the highscores are shown, press enter to start a new game
    inputKey (EventKey (SpecialKey k) ks _ _) gstate@(GameState _ (GameInfo _ _ _ _ _ ScoreScreen) _ _ _)
        | SpecialKey KeyEnter <- SpecialKey k
            , Down <- ks = makeInitialState (randomG gstate) (highScores (highScoreInfo gstate))
            
    -- | Pause and unpause game          
    inputKey (EventKey (Char k) ks _ _) gstate@(GameState _ (GameInfo _ _ _ _ _ Running) _ _ _)
        | Char 'p' <- Char k
            , Down <- ks = gstate{gameInfo = (gameInfo gstate){status = Paused}}

    inputKey (EventKey (Char k) ks _ _) gstate@(GameState _ (GameInfo _ _ _ _ _ Paused) _ _ _)
        | Char 'p' <- Char k
            , Down <- ks = gstate{gameInfo = (gameInfo gstate){status = Running}, keyStates = KeyStates False False False False False}
    
    -- | Ignore all other input
    inputKey _ gstate = gstate

    -- | Helper function that ensures you can delete typos while you give your playername
    backSpacer :: PlayerName -> PlayerName
    backSpacer []     = []
    backSpacer [x]    = []
    backSpacer (x:xs) = x : backSpacer xs 
