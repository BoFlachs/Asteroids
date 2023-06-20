module Main where

    import Controller
    import Model
    import View
    import Datastructure
    import Highscore
    
    import Graphics.Gloss.Interface.IO.Game
    import System.IO.Error 
    import Control.Exception
    import System.Random
    
    main :: IO ()
    main = do   highscore <- (readFile "src/highscore.txt") `catch` errorcheck -- Read the highscore file, if it doesn't exist report that you will create a new one
                closeF highscore   -- Force evaluate the file, so that the handle is closed
                rndm <- getStdGen  -- Get a random generator
                playIO (InWindow "Asteroids" (600, 600) (0, 0)) -- Screensize 
                    black                                       -- Background color
                    30                                          -- Frames per second
                    (makeInitialState rndm (tuples (words highscore)))  -- Create an initial state using the random generator and the highscores
                    view                                        -- View function
                    input                                       -- Event function
                    step                                        -- Step function

