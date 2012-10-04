{-# OPTIONS_GHC -Wall #-}
{-# Language StandaloneDeriving #-}
{-# Language DeriveFunctor #-}

module Vis.VisObject ( VisObject(..)
                     , drawObjects
                     , setPerspectiveMode
                     ) where

import Control.Monad ( when )
import Data.Maybe ( fromJust, isJust )
import Graphics.Rendering.OpenGL.Raw
import qualified Graphics.Gloss.Data.Color as Gloss
import Graphics.UI.GLUT

import SpatialMath

glColorOfColor :: Gloss.Color -> Color4 GLfloat
glColorOfColor = (\(r,g,b,a) -> fmap realToFrac (Color4 r g b a)) . Gloss.rgbaOfColor

setColor :: Gloss.Color -> IO ()
setColor = color . glColorOfColor

setMaterialDiffuse :: Gloss.Color -> IO ()
setMaterialDiffuse col = materialDiffuse Front $= (glColorOfColor col)

data VisObject a = VisObjects [VisObject a]
                 | VisTranslate (Xyz a) (VisObject a)
                 | VisRotateQuat (Quat a) (VisObject a)
                 | VisRotateEulerRad (Euler a) (VisObject a)
                 | VisRotateEulerDeg (Euler a) (VisObject a) -- degrees more efficient
                 | VisScale (a,a,a) (VisObject a)
                 | VisCylinder (a,a) Gloss.Color
                 | VisBox (a,a,a) Flavour Gloss.Color
                 | VisCube Flavour Gloss.Color
                 | VisSphere a Flavour Gloss.Color
                 | VisEllipsoid (a,a,a) Flavour Gloss.Color
                 | VisLine [Xyz a] Gloss.Color
                 | VisLine' [(Xyz a,Gloss.Color)]
                 | VisArrow (a,a) (Xyz a) Gloss.Color
                 | VisAxes (a,a)
                 | VisPlane (Xyz a) Gloss.Color Gloss.Color
                 | VisTriangle (Xyz a) (Xyz a) (Xyz a) Gloss.Color
                 | VisQuad (Xyz a) (Xyz a) (Xyz a) (Xyz a) Gloss.Color
                 | Vis3dText String (Xyz a) BitmapFont Gloss.Color
                 | Vis2dText String (a,a) BitmapFont Gloss.Color
                 | VisPoints [Xyz a] (Maybe GLfloat) Gloss.Color
                 | VisCustom (IO ())

deriving instance Functor VisObject

setPerspectiveMode :: IO ()
setPerspectiveMode = do
  (_, Size w h) <- get viewport
  matrixMode $= Projection
  loadIdentity
  perspective 40 (fromIntegral w / fromIntegral h) 0.1 1000
  matrixMode $= Modelview 0

drawObjects :: VisObject GLdouble -> IO ()
drawObjects objects = do
  setPerspectiveMode
  drawObject objects

drawObject :: VisObject GLdouble -> IO ()
-- list of objects
drawObject (VisObjects xs) = mapM_ drawObject xs

-- list of objects
drawObject (VisTranslate (Xyz x y z) visobj) =
  preservingMatrix $ do
    translate (Vector3 x y z :: Vector3 GLdouble)
    drawObject visobj

drawObject (VisRotateQuat (Quat q0 q1 q2 q3) visobj) =
  preservingMatrix $ do
    rotate (2 * acos q0 *180/pi :: GLdouble) (Vector3 q1 q2 q3)
    drawObject visobj

drawObject (VisRotateEulerRad euler visobj) =
  drawObject $ VisRotateEulerDeg (fmap ((180/pi)*) euler) visobj

drawObject (VisRotateEulerDeg (Euler yaw pitch roll) visobj) =
  preservingMatrix $ do
    rotate yaw   (Vector3 0 0 1)
    rotate pitch (Vector3 0 1 0)
    rotate roll  (Vector3 1 0 0)
    drawObject visobj

drawObject (VisScale (sx,sy,sz) visobj) =
  preservingMatrix $ do
    normalize $= Enabled
    scale sx sy sz
    drawObject visobj
    normalize $= Disabled

-- triangle
drawObject (VisTriangle (Xyz x0 y0 z0) (Xyz x1 y1 z1) (Xyz x2 y2 z2) col) =
  preservingMatrix $ do
    setMaterialDiffuse col
    setColor col
    glBegin gl_TRIANGLES
    glVertex3d x0 y0 z0
    glVertex3d x1 y1 z1
    glVertex3d x2 y2 z2
    glEnd
   
-- quad
drawObject (VisQuad (Xyz x0 y0 z0) (Xyz x1 y1 z1) (Xyz x2 y2 z2) (Xyz x3 y3 z3) col) =
  preservingMatrix $ do
    lighting $= Disabled
    setColor col
    glBegin gl_QUADS
    glVertex3d x0 y0 z0
    glVertex3d x1 y1 z1
    glVertex3d x2 y2 z2
    glVertex3d x3 y3 z3
    glEnd
    lighting $= Enabled

-- cylinder
drawObject (VisCylinder (height,radius) col) =
  preservingMatrix $ do
    setMaterialDiffuse col
    setColor col
    
    -- translate (Vector3 0 0 (-height/2) :: Vector3 GLdouble)

    let nslices = 10 :: Int
        nstacks = 10 :: Int

        -- Pre-computed circle
        sinCosTable = map (\q -> (sin q, cos q)) angles
          where
            angle = 2*pi/(fromIntegral nslices)
            angles = reverse $ map ((angle*) . fromIntegral) [0..(nslices+1)]
            
    -- Cover the base and top
    glBegin gl_TRIANGLE_FAN
    glNormal3d 0 0 (-1)
    glVertex3d 0 0 0
    mapM_ (\(s,c) -> glVertex3d (c*radius) (s*radius) 0) sinCosTable
    glEnd

    glBegin gl_TRIANGLE_FAN
    glNormal3d 0 0 1
    glVertex3d 0 0 height
    mapM_ (\(s,c) -> glVertex3d (c*radius) (s*radius) height) (reverse sinCosTable)
    glEnd

    let -- Do the stacks
        -- Step in z and radius as stacks are drawn.
        zSteps = map (\k -> (fromIntegral k)*height/(fromIntegral nstacks)) [0..nstacks]
        drawSlice z0 z1 (s,c) = do
          glNormal3d  c          s         0
          glVertex3d (c*radius) (s*radius) z0
          glVertex3d (c*radius) (s*radius) z1

        drawSlices (z0,z1) = do
          glBegin gl_QUAD_STRIP
          mapM_ (drawSlice z0 z1) sinCosTable
          glEnd

    mapM_ drawSlices $ zip (init zSteps) (tail zSteps)

-- sphere
drawObject (VisSphere r flav col) =
  preservingMatrix $ do
    setMaterialDiffuse col
    setColor col
    renderObject flav (Sphere' (realToFrac r) 20 20)

-- ellipsoid
drawObject (VisEllipsoid (sx,sy,sz) flav col) = drawObject $ VisScale (sx,sy,sz) $ VisSphere 1 flav col

-- box
drawObject (VisBox (dx,dy,dz) flav col) = drawObject $ VisScale (dx,dy,dz) $ VisCube flav col

drawObject (VisCube flav col) =
  preservingMatrix $ do
    setMaterialDiffuse col
    setColor col
    renderObject flav (Cube 1)

-- line
drawObject (VisLine path col) =
  preservingMatrix $ do
    lighting $= Disabled
    setColor col
    renderPrimitive LineStrip $ mapM_ (\(Xyz x' y' z') -> vertex$Vertex3 x' y' z') path
    lighting $= Enabled

-- line where you set the color at each vertex
drawObject (VisLine' pathcols) =
  preservingMatrix $ do
    lighting $= Disabled
    
    glBegin gl_LINE_STRIP
    let f (xyz, col) = do
          let Xyz x y z = fmap realToFrac xyz
          setMaterialDiffuse col
          setColor col
          glVertex3f x y z
    mapM_ f pathcols
    glEnd
    lighting $= Enabled

-- plane
drawObject (VisPlane (Xyz x y z) col1 col2) =
  preservingMatrix $ do
    let normInv = 1/(sqrt $ x*x + y*y + z*z)
        x' = x*normInv
        y' = y*normInv
        z' = z*normInv
        r  = 10
        n  = 5
        eps = 0.01
    rotate ((acos z')*180/pi :: GLdouble) (Vector3 (-y') x' 0)

    glBegin gl_QUADS
    setColor col2

    let r' = realToFrac r
    glVertex3f   r'    r'  0
    glVertex3f (-r')   r'  0
    glVertex3f (-r')  (-r')  0
    glVertex3f   r'   (-r')  0
    glEnd

    glDisable gl_BLEND
    let drawWithEps eps' = do
          mapM_ drawObject $ concat [[ VisLine [ Xyz (-r) y0 eps'
                                               , Xyz r    y0 eps'
                                               ] col1
                                     , VisLine [ Xyz x0 (-r) eps',
                                                 Xyz x0 r    eps'
                                               ] col1
                                     ] | x0 <- [-r,-r+r/n..r], y0 <- [-r,-r+r/n..r]]
    drawWithEps eps
    drawWithEps (-eps)
    
    glEnable gl_BLEND


-- arrow
drawObject (VisArrow (size, aspectRatio) (Xyz x y z) col) =
  preservingMatrix $ do
    let numSlices = 8
        numStacks = 15
        cylinderRadius = 0.5*size/aspectRatio
        cylinderHeight = size
        coneRadius = 2*cylinderRadius
        coneHeight = 2*coneRadius

        rotAngle = acos(z/(sqrt(x*x + y*y + z*z) + 1e-15))*180/pi :: GLdouble
        rotAxis = Vector3 (-y) x 0
    
    rotate rotAngle rotAxis
    
    -- cylinder
    drawObject $ VisCylinder (cylinderHeight, cylinderRadius) col
    -- cone
    setMaterialDiffuse col
    setColor col
    translate (Vector3 0 0 cylinderHeight :: Vector3 GLdouble)
    renderObject Solid (Cone coneRadius coneHeight numSlices numStacks)

drawObject (VisAxes (size, aspectRatio)) = preservingMatrix $ do
  let xAxis = VisArrow (size, aspectRatio) (Xyz 1 0 0) (Gloss.makeColor 1 0 0 1)
      yAxis = VisArrow (size, aspectRatio) (Xyz 0 1 0) (Gloss.makeColor 0 1 0 1)
      zAxis = VisArrow (size, aspectRatio) (Xyz 0 0 1) (Gloss.makeColor 0 0 1 1)
  drawObject $ VisObjects [xAxis, yAxis, zAxis]

drawObject (VisCustom f) = preservingMatrix f

drawObject (Vis3dText string (Xyz x y z) font col) = preservingMatrix $ do
  lighting $= Disabled
  setColor col
  glRasterPos3d x y z
  renderString font string
  lighting $= Enabled

drawObject (Vis2dText string (x,y) font col) = preservingMatrix $ do
  lighting $= Disabled
  setColor col

  matrixMode $= Projection
  loadIdentity

  (_, Size w h) <- get viewport
  ortho2D 0 (fromIntegral w) 0 (fromIntegral h)
  matrixMode $= Modelview 0
  loadIdentity

  glRasterPos2d x y
  renderString font string

  setPerspectiveMode
  lighting $= Enabled

drawObject (VisPoints xyzs ps col) =
  preservingMatrix $ do
    lighting $= Disabled
    setColor col
    s' <- get pointSize
    when (isJust ps) $ pointSize $= (fromJust ps)
    renderPrimitive Points $ mapM_ (\(Xyz x' y' z') -> vertex$Vertex3 x' y' z') xyzs
    pointSize $= s'
    lighting $= Enabled

