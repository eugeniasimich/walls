module Graphics (initGL, draw) where

import FRP.Yampa
import FRP.Yampa.Vector3
import FRP.Yampa.Utilities

import Graphics.UI.GLUT hiding (Level,Vector3(..),normalize)
import qualified Graphics.UI.GLUT as G(Vector3(..))

import Unsafe.Coerce
import Control.Lens
import Types

zed = (-30.0)
leftWall, rightWall, topWall, bottomWall :: GLfloat
leftWall = (-9)
rightWall = 9
topWall = 8
bottomWall = (-8)

initGL :: IO ()
initGL = do
    getArgsAndInitialize
    initialWindowPosition $= Position 600 600
    initialWindowSize $= Size 800 600
    initialDisplayMode $= [ WithDepthBuffer ]
    createWindow "Balls"
    depthFunc          $= Just Less
    clearColor         $= Color4 0 0 0 0
--    light (Light 0)    $= Enabled
--    lighting           $= Enabled
--    lightModelAmbient  $= Color4 0.5 0.5 0.5 1
--    diffuse (Light 0)  $= Color4 1 1 1 1
--    blend              $= Enabled
--    blendFunc          $= (SrcAlpha, OneMinusSrcAlpha)
    colorMaterial      $= Just (FrontAndBack, AmbientAndDiffuse)
    reshapeCallback    $= Just resizeScene
    return ()

-- Copied from reactive-glut
resizeScene :: Size -> IO ()
resizeScene (Size w 0) = resizeScene (Size w 1) -- prevent divide by zero
resizeScene s@(Size width height) = do
  -- putStrLn "resizeScene"
  viewport   $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 50 (w2/h2) 1 1000
  matrixMode $= Modelview 0
 where
   w2 = half width
   h2 = half height
   half z = realToFrac z / 2

-- Rendering Code:

draw :: [P2D] -> IO ()
draw ps = do
    clear [ ColorBuffer, DepthBuffer ]
    loadIdentity
    mapM_ (\p -> renderPlayer $ vector3 (unsafeCoerce p^.x) (unsafeCoerce p^.y) (-30)) ps
    renderWalls
    flush
    where size2 :: R
          size2 = (fromInteger $ 6)/2
          red    = Color4 1.0 0.7 0.8 1.0 :: Color4 R
          green  = Color4 0.8 1.0 0.7 0.9 :: Color4 R
          
          renderShapeAt s p = preservingMatrix $ do
            translate $ G.Vector3 (0.5 - size2 + vector3X p)
                                  (0.5 - size2 + vector3Y p)
                                  (0.5 - size2 + vector3Z p)
            renderObject Solid s
          renderPlayer   = (color red >>) . (renderShapeAt $ Sphere' 0.5 20 20)
          renderWalls   = (renderPrimitive Lines $ do
                            vertex $ (Vertex3 (leftWall-2)  (bottomWall-2) zed :: Vertex3 GLfloat)
                            vertex $ (Vertex3 (leftWall-2)  (topWall-2)    zed :: Vertex3 GLfloat))
                       >> (renderPrimitive Lines $ do
                            vertex $ (Vertex3 (leftWall-2)  (bottomWall-2) zed :: Vertex3 GLfloat)
                            vertex $ (Vertex3 (rightWall-2) (bottomWall-2) zed :: Vertex3 GLfloat))
                       >> (renderPrimitive Lines $ do
                            vertex $ (Vertex3 (leftWall-2)  (topWall-2)   zed :: Vertex3 GLfloat)
                            vertex $ (Vertex3 (rightWall-2) (topWall-2)   zed :: Vertex3 GLfloat))
                       >> (renderPrimitive Lines $ do
                            vertex $ (Vertex3 (rightWall-2) (topWall-2)   zed :: Vertex3 GLfloat)
                            vertex $ (Vertex3 (rightWall-2) (bottomWall-2) zed :: Vertex3 GLfloat))
