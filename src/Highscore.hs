-- | This module handles the highscore
module Highscore where

    import System.IO
    import System.IO.Error 
    import Control.Exception
    import Data.List
    import Datastructure

    -- | Sorts the highscore list in reverse order
    sortHS :: HighScore -> HighScore
    sortHS hs = reverse (sort hs)

    -- | Makes a HighScore by casting each number to an Int alongside its corresponding string
    tuples :: [String] -> HighScore
    tuples [] = []
    tuples (x:y:xs) = (read x,y) : tuples xs

    -- | Converts a HighScore to a string to write to highscore.txt
    showScore :: HighScore -> String 
    showScore []     = ""
    showScore [x]    = show(fst(x)) ++ " " ++ snd(x)
    showScore (x:xs) = show(fst(x)) ++ " " ++ snd(x) ++ " " ++ showScore xs

    -- | Function to display the score given a single score tuple
    viewScore :: (Int, String) -> String
    viewScore (a,b) = b ++ " - " ++ show(a)

    -- | Does the writing to highscore.txt with a sorted HighScore
    writeHighScore :: HighScore -> IO ()
    writeHighScore h = writeFile "src/highscore.txt" (showScore (sortHS h))

    -- | Recursive function that inserts a new score in its right position and returns the updated HighScore 
    addHighScore :: HighScore -> Score -> PlayerName -> HighScore
    addHighScore [] (Score s) name = [(s, name)]
    addHighScore (x:xs) (Score s) name
      | s > fst x = [(s, name)] ++ x:xs
      | otherwise = x : (addHighScore xs (Score s) name)

    -- | Checks whether the highscore file actually exists: if not, it returns an empty list and the file will
    -- | be created later.
    errorcheck :: IOError -> IO String
    errorcheck e
      | isDoesNotExistError e = do putStrLn "Highscore file does not exist - will create one" ; return []
      | otherwise = do putStrLn "Something went wrong with the highscore file" ; return []

    -- | Function to force read an entire file to close the handle
    closeF :: String -> IO ()
    closeF h = length h `seq` return ()