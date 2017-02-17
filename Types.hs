{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Graphics.Gloss (Point, Vector)
import Control.Lens

-- data Input = Mouse { coord       :: Point,
--                      button      :: MouseButton,
--                      buttonState :: KeyState }

data Wall        = Wall { coord :: Float , axis :: Axis }

data Axis        = V | H

data Ball        = Ball   { _pos :: Point,
                            _vel :: Vector }
data World       = World  { _level     :: [Ball],
                            _borders   :: [Wall],
                            _walls     :: [Wall]}

newtype Rotation = Rot {applyRot :: Ball -> Ball}

makeLenses    ''Ball
makeLenses    ''World
