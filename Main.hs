{-# LANGUAGE Arrows #-}

module Main where

import FRP.Yampa
import FRP.Yampa.Switches
import Control.Lens
import Graphics.Gloss
import Graphics.Gloss.Interface.FRP.Yampa

import Types
import Graphics

initWorld  = World {
  _level   = [singleBall, secondBall, thirdBall, fourthBall],
  _borders = [Wall 200.0 H,  Wall (-200.0) H, Wall (-300.0) V, Wall 300.0 V],
  _walls   = [] }
  where singleBall = Ball { _pos = (0.0, 60.0) , _vel = (-1.0*modulo,  1.5*modulo) }
        secondBall = Ball { _pos = (50.0, 0.0) , _vel = ( 1.0*modulo,  1.5*modulo) }
        thirdBall  = Ball { _pos = (30.0, 20.0), _vel = (-1.0*modulo, -1.5*modulo) }
        fourthBall = Ball { _pos = (0.0, 20.0) , _vel = ( 1.5*modulo, -1.0*modulo) }
        modulo     = 150.0

mainSF = bouncingOffWalls initWorld >>^ draw 

main :: IO ()
main = do
  playYampa
    (InWindow "Walls" (600,400) (200,200))
    (greyN 0.8)
    3000
    mainSF
       
move :: [Ball] -> SF a [Ball]
move bs = proc input -> do
  nxs <- (zipWith (+) pxs) ^<< parZ (repeat integral) -< vxs
  nys <- (zipWith (+) pys) ^<< parZ (repeat integral) -< vys
  returnA -< constructBalls nxs nys vxs vys
  where vxs = map (\b -> b^.vel^._1) bs
        vys = map (\b -> b^.vel^._2) bs
        pxs = map (\b -> b^.pos^._1) bs
        pys = map (\b -> b^.pos^._2) bs
        constructBalls px py vx vy = zipWith Ball (zipWith (,) px py) (zipWith (,) vx vy)

bouncingOffWalls :: World -> SF a World
bouncingOffWalls w =
  switch (move' w) bouncingOffWalls
  where move' :: World -> SF a (World, Event World)
        move' w@(World bs bords ws) = proc input -> do
          nbs <- move bs -< input
          ev <- edge -< or $ map (or . outsideBoundaries bords) nbs
          let nw = set level nbs w
          returnA -< (nw, ev `tag` bounceBalls nw)

outsideBoundaries :: [Wall] -> Ball -> [Bool]
outsideBoundaries [t, b, l, r] ball = [y >= coord t, y <= coord b, x <= coord l, x >= coord r]
  where (x, y) = ball^.pos

bounceBalls :: World -> World
bounceBalls w = over level (map (bounceBall (w^.borders))) w
  where
    bounceBall :: [Wall] -> Ball -> Ball
    bounceBall bords b = foldr applyRot b $ map fst $ filter ((==True).snd) (zip handlers (outsideBoundaries bords b))
    handlers = [hitHWall, hitHWall, hitVWall, hitVWall]
    hitVWall = Rot $ over (vel._1) (*(-1))
    hitHWall = Rot $ over (vel._2) (*(-1))


