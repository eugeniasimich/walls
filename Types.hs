{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import FRP.Yampa
import FRP.Yampa.Vector3

import Graphics.UI.GLUT hiding (Level,Vector3(..),normalize)
import qualified Graphics.UI.GLUT as G(Vector3(..))

import Control.Lens

data P2D = P2D { _x :: Double, _y :: Double } deriving Show
type R   = GLdouble
type Wall = Double
newtype Rotation = Rot {applyRot :: Ball -> Ball}

data Ball = Ball { _pos :: P2D,
                   _vel :: P2D } deriving Show


-- data Input = Keyboard { key       :: Key,
--                         keyState  :: KeyState,
--                         modifiers :: Modifiers }

data Input = Mouse { coord       :: P2D,
                     button      :: MouseButton,
                     buttonState :: KeyState }

data GameState = Game { _level     :: [Ball],
                        _vWalls    :: [Wall],
                        _hWalls    :: [Wall]}

makeLenses    ''Ball
makeLenses    ''P2D
makeLenses    ''GameState


-- data ParsedInput = 
--     ParsedInput { wCount :: Double, aCount :: Double, 
--                   sCount :: Double, dCount :: Double,
--                   upEvs  :: Event Input, downEvs :: Event Input, 
--                   rightEvs :: Event Input, leftEvs :: Event Input }

