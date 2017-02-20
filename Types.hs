{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}

module Types  where

import Graphics.Gloss (Point, Vector)
import Control.Lens
import qualified Graphics.Gloss.Interface.IO.Game as G

type InputEvent = G.Event


data MouseClick = Mouse { mPosition  :: Point,
                          mDirection :: Axis }

data Wall        = Wall { coord :: Float , axis :: Axis }

data Axis        = V | H

data Ball        = Ball   { _pos :: Point,
                            _vel :: Vector}
                   
data World       = World  { _level     :: [Ball],
                            _borders   :: [Wall],
                            _hwalls    :: [Wall],
                            _vwalls    :: [Wall],
                            _ballRad   :: Float }


newtype Rotation = Rot {applyRot :: Ball -> Ball}

makeLenses    ''Ball
makeLenses    ''World
