{-# LANGUAGE Arrows #-}

module Main where

import FRP.Yampa
--import FRP.Yampa
import FRP.Yampa.Switches
import Control.Lens
import Graphics.Gloss
import Graphics.Gloss.Interface.FRP.Yampa
import qualified Graphics.Gloss.Interface.IO.Game as G

import Types 
import Graphics

initWorld  = World {
  _level   = [singleBall, secondBall, thirdBall, fourthBall],
  _borders = [Wall 200.0 H,  Wall (-200.0) H, Wall (-300.0) V, Wall 300.0 V],
  _hwalls   = [],
  _vwalls   = [],
  _ballRad = 10.0}
  where singleBall = Ball { _pos = (0.0, 60.0) , _vel = (-1.0*modulo,  1.5*modulo) }
        secondBall = Ball { _pos = (50.0, 0.0) , _vel = ( 1.0*modulo,  1.5*modulo) }
        thirdBall  = Ball { _pos = (30.0, 20.0), _vel = (-1.0*modulo, -1.5*modulo) }
        fourthBall = Ball { _pos = (0.0, 20.0) , _vel = ( 1.5*modulo, -1.0*modulo) }
        modulo     = 150.0

mainSF = parseInput >>> bouncingOffWalls initWorld >>^ draw 

main :: IO ()
main = do
  playYampa
    (InWindow "Walls" (620,420) (300,600))
    (greyN 0.8)
    3000
    mainSF
       
move :: [Ball] -> SF ()  [Ball]
move bs = proc init -> do
  nxs <- (zipWith (+) pxs) ^<< parZ (repeat integral) -< vxs
  nys <- (zipWith (+) pys) ^<< parZ (repeat integral) -< vys
  returnA -< constructBalls nxs nys vxs vys
  where vxs = map (\b -> b^.vel^._1) bs
        vys = map (\b -> b^.vel^._2) bs
        pxs = map (\b -> b^.pos^._1) bs
        pys = map (\b -> b^.pos^._2) bs
        constructBalls px py vx vy = zipWith Ball (zipWith (,) px py) (zipWith (,) vx vy)

bouncingOffWalls :: World -> SF (Event MouseClick) World
bouncingOffWalls w = 
  switch (moveUntilWalls w) bouncingOffWalls

moveUntilWalls :: World -> SF (Event MouseClick) (World, Event World)
moveUntilWalls w@(World bs bords _ _ rad) =
  proc input -> do
    rec
      currentW <- dHold w -< gameUpdated
      gameUpdated <- arr update -< (currentW, input)
      nbs <- move bs -< ()
      let nw = set level nbs currentW
      ev <- edge -< isOutsideBorders w nbs
    returnA -< (nw, ev `tag` bounceBalls nw)


update :: (World, Event MouseClick) -> Event World
update (w , input) =
    case input of
      NoEvent -> NoEvent
      Event m@(Mouse _ V) -> Event $ over vwalls ((:) (makeWall m)) w
      Event m@(Mouse _ H) -> Event $ over hwalls ((:) (makeWall m)) w
        {-TODO: check if it's an impossible place for wall-}


-- | Run the game, keeping the internal state using dHold, updating the
-- game state based on user's input (if any)
runGame :: World -> SF (Event MouseClick) World
runGame state = proc input -> do
  rec currentState <- dHold state -< gameUpdated -- gameUpdated :: Event World
      gameUpdated <- arr update -< (currentState, input)
  returnA -< currentState

isOutsideBorders :: World -> [Ball] -> Bool
isOutsideBorders w = or . map (or . outsideBorders (w^.borders) (w^.ballRad)) 

outsideBorders :: [Wall] -> Float -> Ball -> [Bool]
outsideBorders [t, b, l, r] delta ball = [y + delta >= coord t, y -delta <= coord b, x-delta <= coord l, x+delta >= coord r]
  where (x, y) = ball^.pos

bounceBalls :: World -> World
bounceBalls w = over level (map (bounceBall (w^.borders) (w^.ballRad))) w
  where
    bounceBall :: [Wall] -> Float -> Ball -> Ball
    bounceBall bords delta b = foldr applyRot b $ map fst $ filter ((==True).snd) (zip handlers (outsideBorders bords delta b))
    handlers = [hitHWall, hitHWall, hitVWall, hitVWall]
    hitVWall = Rot $ over (vel._1) (*(-1))
    hitHWall = Rot $ over (vel._2) (*(-1))


parseInput :: SF (Event InputEvent) (Event MouseClick)
parseInput = arr $ \event ->
  case event of
    Event (G.EventKey (G.MouseButton G.LeftButton)  G.Down _ p) -> event `tag` Mouse { mPosition = p, mDirection = V }
    Event (G.EventKey (G.MouseButton G.RightButton) G.Down _ p) -> event `tag` Mouse { mPosition = p, mDirection = H }  
    _ -> noEvent




makeWall :: MouseClick -> Wall
makeWall (Mouse (x,_) V) = Wall x V
makeWall (Mouse (_,y) H) = Wall y H
