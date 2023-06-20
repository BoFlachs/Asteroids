-- | This module defines how to turn
--   the game state into a picture
module View where

    import Graphics.Gloss
    import Model
    import Datastructure
    import Data.Fixed
    import Renderable
    import Highscore 
    
    view :: GameState -> IO Picture
    view = return . viewPure

    viewPure :: GameState -> Picture
    -- | At the beginning of the game only show the starting message
    viewPure gstate@(GameState _ (GameInfo _ _ _ _ _ GameStart) _ _ _) = writeStartMessage

    -- | When the game is over ask for the name, show the current name and show the final score 
    viewPure gstate@(GameState _ (GameInfo _ _ _ _ _ GameOver) _ _ _) = pictures[askName, writeName(playername(gameInfo gstate))
                                                                                , writeFinalScore(score(gameInfo gstate))]

    -- | After the name has been entered, show the highest 5 scores
    viewPure gstate@(GameState _ (GameInfo _ _ _ _ _ ScoreScreen) _ _ (HighScoreInfo h _)) = writeScoreScreen h (-70) 100

    -- | When the game is paused show all objects, lives, score and the explanation of the controls
    viewPure gstate@(GameState _ (GameInfo _ _ _ _ _ Paused) _ _ _) = pictures [render (friendly (world gstate)), showEShip (enemies (world gstate)), 
                                                                      showAsteroid (asteroids (world gstate)),
                                                                      showBullet (bullets (world gstate)), showEBullet (ebullets (world gstate))
                                                                      , writeLives (lives(gameInfo gstate)), writeScore (score(gameInfo gstate)), writeLegend]

    -- | When in single player, show all objects in the game except for the turret, which is locked.                                                              
    viewPure gstate@(GameState _ (GameInfo _ _ _ _ SinglePlayer _) _ _ _) = pictures [render (friendly (world gstate)), showEShip (enemies (world gstate)), 
                                                                            showAsteroid (asteroids (world gstate)),
                                                                            showBullet (bullets (world gstate)), showEBullet (ebullets (world gstate))
                                                                            , writeLives (lives(gameInfo gstate)), writeScore (score(gameInfo gstate)), writePause]

    -- | When in multiplayer, show all objects in the game including the turret
    viewPure gstate@(GameState _ (GameInfo _ _ _ _ MultiPlayer _) _ _ _) = pictures [render (friendly (world gstate)), showTurret (friendly (world gstate)), 
                                                                          showEShip (enemies (world gstate)), showAsteroid (asteroids (world gstate)),
                                                                          showBullet (bullets (world gstate)), showEBullet (ebullets (world gstate))
                                                                          , writeLives (lives(gameInfo gstate)), writeScore (score(gameInfo gstate)), writePause]

    -- | Ask the name of the player
    askName :: Picture
    askName = translate (-150) 0 (scale 0.15 0.15 (color white (text "What is your name?" )))
    
    -- | Shows the highscores
    writeScoreScreen :: HighScore -> Float -> Float -> Picture
    writeScoreScreen [] a b     = pictures [translate (-70) 140 (scale 0.15 0.15 (color white ( text "NAME SCORE"))),
                                            translate (-170) (-130) (scale 0.15 0.15 (color white (text "Press \"Enter\" to start a new game!" )))]
    writeScoreScreen (x:xs) a b = pictures (translate a b (scale 0.15 0.15 (color white (text (viewScore x)))) : [writeScoreScreen xs a (b-40)])

    -- | Echos the name written by the player to the screen
    writeName :: PlayerName -> Picture
    writeName name = translate (-150) (-100) (scale 0.15 0.15 (color white (text name)))
                  
    -- | Message in the starting screen
    writeStartMessage :: Picture
    writeStartMessage = pictures [translate (-170) 20 (scale 0.15 0.15 (color white (text "Press \"S\" to start the single-player" ))),
                                    translate (-170) (-20) (scale 0.15 0.15 (color white (text "Press \"M\" to start the multi-player" ))),
                                    translate (-210) (-180) (scale 0.15 0.15 (color white (text "Controls:" ))),
                                    translate (-210) (-210) (scale 0.15 0.15 (color white (text "Arrows - Fly ship" ))),
                                    translate (-210) (-240) (scale 0.15 0.15 (color white (text "Space - Shoot" ))),
                                    translate (-210) (-270) (scale 0.15 0.15 (color white (text "a and d - Move turret (only in Multiplayer)" )))]

    -- | Writes an explanation of the controls on the screen
    writeLegend :: Picture
    writeLegend = pictures [translate 0 230 (scale 0.15 0.15 (color white (text "Press \"P\" to un-pause" ))),
                            translate (-210) (0) (scale 0.15 0.15 (color white (text "Controls:" ))),
                            translate (-210) (-30) (scale 0.15 0.15 (color white (text "Arrows - Fly ship" ))),
                            translate (-210) (-60) (scale 0.15 0.15 (color white (text "Space - Shoot" ))),
                            translate (-210) (-90) (scale 0.15 0.15 (color white (text "a and d - Move turret (only in Multiplayer)" )))]

    -- | Writes information about pausing to the screen
    writePause :: Picture
    writePause =  translate 0 230 (scale 0.15 0.15 (color white (text "Press \"P\" to pause" )))

    -- | Writes the final score in the screen after death
    writeFinalScore :: Score -> Picture
    writeFinalScore s = translate 100 260 (scale 0.25 0.25 (color white (text (show s))))

    -- | Writes the score on the screen
    writeScore :: Score -> Picture
    writeScore s = translate 0 270 (scale 0.15 0.15 (color white (text (show s))))
    
    -- | Function that uses Gloss' text function to show amount of lives
    writeLives :: Lives -> Picture
    writeLives x = translate 200 270 (scale 0.15 0.15 (color white (text (show x))))

    -- | Function to display a turret. Turret can't be an instance of Renderable, because the data from the friendlyship is needed for the picture
    showTurret :: FShip -> Picture
    showTurret fs@(FShip _ _ _ _ _ _ t) | defence fs == Invincible = pictures ([(translate x y  (rotate (unAngle (angle(tgeo t))) (translate (-x) (-y) ((color green (line [
                                                                          (x+3, y), (x-3,y), (x-3,y-3), (x+3,y-3),(x+3,y+15), (x-3,y+15),(x-3,y),(x,y),(x,y+15)
                                                                          ]))))))])
                                        | otherwise = pictures ([(translate x y  (rotate (unAngle (angle(tgeo t))) (translate (-x) (-y) ((color white (line [
                                                                (x+3, y), (x-3,y), (x-3,y-3), (x+3,y-3),(x+3,y+15), (x-3,y+15),(x-3,y),(x,y),(x,y+15)
                                                                ]))))))])
                                                                  where x = getx t
                                                                        y = gety t

    -- | Function to display all enemy ships          
    showEShip :: [EShip] -> Picture
    showEShip [] = blank
    showEShip (e:es) | (estatus e) == Dead = pictures ((drawExplosion (getLocation e) (estimer e) (getSize e)) : [showEShip es])
                     | otherwise = pictures (render e : [showEShip es])

    -- | Function to display all asteroids
    showAsteroid :: [Asteroid] -> Picture
    showAsteroid [] = blank
    showAsteroid (a:as)
      | astatus a == Dead = pictures ((drawExplosion (getLocation a) (atimer a) (getSize a)) : [showAsteroid as])
      | otherwise = pictures (render   a : [showAsteroid as])

    -- | Function to display a bullet
    showBullet :: [Bullet] -> Picture
    showBullet [] = blank 
    showBullet (b:bs) = pictures (render b : [showBullet bs])

    -- | Function to display an enemy bullet
    showEBullet :: [EBullet] -> Picture
    showEBullet [] = blank 
    showEBullet (eb:ebs) = pictures (render eb :[showEBullet ebs])

    -- | Function to display a hitbox
    drawHitbox :: HitBox -> Picture
    drawHitbox (HitBox (Location a b) (Location x y)) = color white (line [
      (a, b), (a, y), (x, y), (x,b), (a,b) ])

    -- | Function to display an explosion animation
    drawExplosion :: Location -> Float -> Size ->  Picture
    drawExplosion (Location x y) n' s@(Size s') | s == medAsteroid = pictures[(color white (line [(x+3,y),(x+3 +n, y + n)])), (color white (line [(x+6,y-4),(x+6+n,y-4 - n)]))
                                                                      , (color white (line [(x-3,y+2),(x-3-n,y+2+n)])), (color white (line [(x-3,y-4),(x-3-n,y-4-n)]))
                                                                      , (color white (line [(x-3,y),(x-3-n,y+2)])), (color white (line [(x+10,y-4),(x+10+n,y-4)]))
                                                                      , (translate n (n-10) (color white (line [(x-3,y+1),(x+1,y+3),(x+3,y-1),(x-1,y-3)])))
                                                                      , (translate (-n + 15) n (color white (line [(x-3,y+1),(x+1,y+3),(x+3,y-1),(x-1,y-3)])))
                                                                      , (translate 0 (-n-15) (color white (line [(x-3,y+1),(x+1,y+3),(x+3,y-1),(x-1,y-3)])))
                                                                      , (translate (n+15) (-n) (color white (line [(x-3,y+1),(x+1,y+3),(x+3,y-1),(x-1,y-3)])))
                                                                      ]
                                                | s == smallAsteroid = pictures[(color white (line [(x+3,y),(x+3 +n, y + n)])), (color white (line [(x+6,y-4),(x+6+n,y-4 - n)]))
                                                                      , (color white (line [(x-3,y+2),(x-3-n,y+2+n)])), (color white (line [(x-3,y-4),(x-3-n,y-4-n)]))
                                                                      , (color white (line [(x-3,y),(x-3-n,y+2)])), (color white (line [(x+10,y-4),(x+10+n,y-4)]))
                                                                      , (translate (-n + 15) n (color white (line [(x-3,y+1),(x+1,y+3),(x+3,y-1),(x-1,y-3)])))
                                                                      , (translate 0 (-n-15) (color white (line [(x-3,y+1),(x+1,y+3),(x+3,y-1),(x-1,y-3)])))                                                                   
                                                                      , (translate (-n) 0 (color white (line [(x-3,y+1),(x+1,y+3),(x+3,y-1),(x-1,y-3)])))
                                                                      ]
                                                | otherwise = pictures[(color white (line [(x+3,y),(x+3 +n, y + n)])), (color white (line [(x+6,y-4),(x+6+n,y-4 - n)]))
                                                                      , (color white (line [(x-3,y+2),(x-3-n,y+2+n)])), (color white (line [(x-3,y-4),(x-3-n,y-4-n)]))
                                                                      , (color white (line [(x-3,y),(x-3-n,y+2)])), (color white (line [(x+10,y-4),(x+10+n,y-4)]))
                                                                      , (translate n (n-10) (color white (line [(x-3,y+1),(x+1,y+3),(x+3,y-1),(x-1,y-3)])))
                                                                      , (translate (-n + 15) n (color white (line [(x-3,y+1),(x+1,y+3),(x+3,y-1),(x-1,y-3)])))
                                                                      , (translate 0 (-n-15) (color white (line [(x-3,y+1),(x+1,y+3),(x+3,y-1),(x-1,y-3)])))
                                                                      , (translate (n+15) (-n) (color white (line [(x-3,y+1),(x+1,y+3),(x+3,y-1),(x-1,y-3)])))
                                                                      , (translate (-n) 0 (color white (line [(x-3,y+1),(x+1,y+3),(x+3,y-1),(x-1,y-3)])))
                                                                      ]
                  where n = (animationDurationE - n') * s'  