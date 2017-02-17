module Graphics (draw) where

import FRP.Yampa
import FRP.Yampa.Utilities
import Graphics.Gloss

import Types

draw :: World -> Picture
draw (World bs bords ws) =
  pictures (map drawBall bs ++ map (drawWall bords)(bords ++ ws) )
  where
    drawWall [_, _, (Wall l V), (Wall r V)] (Wall c H) = line [(l,c), (r,c)]
    drawWall [(Wall t H), (Wall b H), _ ,_] (Wall c V) = line [(c,t), (c,b)]
    drawBall (Ball (x,y) _) = translate x y ball
    ball = color red $ circle 10.0 
