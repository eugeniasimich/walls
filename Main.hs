{-# LANGUAGE Arrows #-}

module Main where

import FRP.Yampa
import FRP.Yampa.Vector3
import FRP.Yampa.Utilities
import FRP.Yampa.Switches 
import Data.IORef
import Graphics.UI.GLUT hiding (Level,Vector3(..),normalize)
import qualified Graphics.UI.GLUT as G(Vector3(..))
import Control.Lens

import Types
import Graphics

initBall :: [Ball]
initBall = [singleBall, secondBall, thirdBall]
  where singleBall = Ball { _pos = P2D 0.0 0.0, _vel = P2D (-14.0) 12.0 }
        secondBall = Ball { _pos = P2D 0.0 0.0, _vel = P2D 12.0 14.0 }
        thirdBall  = Ball { _pos = P2D 1.0 2.0, _vel = P2D (-12.0) (-14.0) }
        
leftWall = (-9)
rightWall = 9
topWall = 8
bottomWall = (-8)

outsideBoundaries :: Ball -> [Bool]
outsideBoundaries b = [p^.x <= leftWall, p^.x >= rightWall, p^.y >= topWall, p^.y <= bottomWall]
  where p = b^.pos

bounceBalls :: [Ball] -> [Ball]
bounceBalls bs = map bounceBall bs

bounceBall :: Ball -> Ball
bounceBall b = foldr applyRot b $ map fst $ filter ((==True).snd) (zip handlers (outsideBoundaries b))
  where handlers = [hitVWall, hitVWall, hitHWall, hitHWall]


hitVWall, hitHWall :: Rotation
hitVWall = Rot $ over (vel.x) (*(-1))
hitHWall = Rot $ over (vel.y) (*(-1))

move :: [Ball] -> SF () [Ball]
move bs = proc () -> do
  nxs <- (zipWith (+) pxs) ^<< parZ (repeat integral) -< vxs
  nys <- (zipWith (+) pys) ^<< parZ (repeat integral) -< vys
  returnA -< constructBalls nxs nys vxs vys
  where vxs = map (\b -> b^.vel^.x) bs
        vys = map (\b -> b^.vel^.y) bs
        pxs = map (\b -> b^.pos^.x) bs
        pys = map (\b -> b^.pos^.y) bs
        constructBalls px py vx vy = zipWith Ball (zipWith P2D px py) (zipWith P2D vx vy)
       
bouncingOffWalls :: [Ball] -> SF () [Ball]
bouncingOffWalls bs =
  switch (move' bs) bouncingOffWalls
  where move' bs' = proc () -> do
          nbs <- move bs' -< ()
          ev <- edge -< or $ map (or . outsideBoundaries) nbs
          returnA -< (nbs, ev `tag` bounceBalls nbs)
          

mainSF = bouncingOffWalls initBall >>^ (\ bs -> draw (map (\b->b^.pos) bs))

-- | Main, initializes Yampa and sets up reactimation loop
main :: IO ()
main = do
    oldTime <- newIORef (0 :: Int)
    rh <- reactInit initGL (\_ _ b -> b >> return False) mainSF
    displayCallback $= return ()
    idleCallback $= Just (idle oldTime rh)
    oldTime' <- get elapsedTime
    writeIORef oldTime oldTime' 
    mainLoop

-- | Reactimation iteration, supplying the input
idle :: IORef Int -> 
        ReactHandle () (IO ()) -> IO ()
idle oldTime rh = do
    newTime'  <- get elapsedTime
    oldTime'  <- get oldTime
    let dt = (fromIntegral $ newTime' - oldTime')/1000
    react rh (dt, Nothing ) 
    writeIORef oldTime newTime'
    return ()


