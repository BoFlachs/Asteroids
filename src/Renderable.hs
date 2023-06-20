-- | This module defines the instance of Renderable for the main objects of the game
module Renderable where

    import Graphics.Gloss
    import Model
    import Datastructure
    import Data.Fixed

    -- | When the ship spawns it flickers as long as it's invincible
    -- | If the ship is moving an additional flame is drawn. If the ship is invincible it is green instead of white
    instance Renderable FShip where
        render fs   | ((ftimer fs) `mod'` 4) == 1 = pictures []
                    | getSpeed fs > (Speed 2) && defence fs == Invincible = pictures ([(translate x y  (rotate (unAngle (angle(fsgeo fs))) 
                                                                (translate (-x) (-y) ((color green (line [(x-10, y-18), (x, y+18), (x+10,  y-18), (x+8, y-13),
                                                                    (x-3, y-13),(x, y-19),(x+3, y-13), (x-8, y-13) ]))))))])
                    | defence fs == Invincible = pictures ([(translate x y  (rotate (unAngle (angle(fsgeo fs))) (translate (-x) (-y) ((color green (line [
                                                    (x-10, y-18), (x, y+18), (x+10,  y-18), (x+8, y-13), (x-8, y-13)
                                                    ]))))))])
                    | getSpeed fs > (Speed 2) = pictures ([(translate x y  (rotate (unAngle (angle(fsgeo fs))) (translate (-x) (-y) ((color white (line [
                                            (x-10, y-18), (x, y+18), (x+10,  y-18), (x+8, y-13),(x-3, y-13),(x, y-19),(x+3, y-13),(x-8, y-13)
                                            ]))))))])
                    | otherwise = pictures ([(translate x y  (rotate (unAngle (angle(fsgeo fs))) (translate (-x) (-y) ((color white (line [
                                    (x-10, y-18), (x, y+18), (x+10,  y-18), (x+8, y-13), (x-8, y-13)
                                    ]))))))])
                                    where x = getx fs
                                          y = gety fs
    
    instance Renderable EShip where
      render ship  = color white (line [
                          (x-30, y-4), (x+30, y-4), (x+10, y-18), (x-10, y-18), (x-30, y-4), (x-10, y+10), (x+10, y+10), 
                          (x+30,y-4), (x+10, y+10), (x+4, y+18), (x-4, y+18), (x-10, y+10) 
                          ])
              where x = getx ship
                    y = gety ship

    -- | Asteroids have different shapes according to their size
    instance Renderable Asteroid where
      render a  | size (ageo a) == bigAsteroid   = color white (line [
                                                  (x-32, y - 10), (x-32, y-15), (x-17, y-25), (x-12, y-30), (x+13, y-30), (x+33, y-10), (x+33, y+10)
                                                                ,(x+22, y+12),(x+13, y+30), (x-12, y+30), (x-32, y+10), (x-12, y+5), (x-32, y-10)])
                | size (ageo a) == medAsteroid   = color white (line [
                                                  (x-18, y-7), (x-18, y-9), (x-10, y-14), (x-8, y-17), (x+8, y-17), (x+18, y-7), (x+18, y+7) ,(x+14, y+14), 
                                                              (x+8, y+5),(x+6, y+16), (x-8, y+17), (x-18, y+7), (x-8, y+3), (x-18, y-7)])
                | size (ageo a) == smallAsteroid = color white (line [
                                                  (x-15, y-4), (x-15, y-6), (x-7, y-11), (x-5, y-14), (x+5, y-14), (x+15, y-3), (x+15, y+4) ,(x+11, y+11), 
                                                              (x+3, y+2),(x+3, y+12), (x-5, y+14), (x-15, y+4), (x-15, y-4)])
                          where x = getx a
                                y = gety a
          
    instance Renderable Bullet where
      render b'  = translate x y  (rotate (unAngle (angle(bgeo b'))) (translate (-x) (-y) ((color white (polygon [(x,y+1), (x, y+6), (x+1, y+6), (x+1, y+1)])))))
          where x = getx b'
                y = gety b'

    instance Renderable EBullet where
      render eb'  = translate x y  (rotate (unAngle (angle(ebgeo eb'))) (translate (-x) (-y) ((color white (polygon [(x,y+1), (x, y+6), (x+1, y+6), (x+1, y+1)])))))
          where x = getx eb'
                y = gety eb'