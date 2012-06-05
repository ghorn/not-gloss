{-# OPTIONS_GHC -Wall #-}

module Vis.VisObject ( VisObject(..)
                     , drawObjects
                     , setPerspectiveMode
                     ) where

import Graphics.Rendering.OpenGL.Raw ( glBegin, glEnd, gl_QUADS, gl_QUAD_STRIP, gl_TRIANGLES, glVertex3f, glVertex3d, glNormal3d, gl_TRIANGLE_FAN, glRasterPos2d ,glRasterPos3d )
import qualified Graphics.Gloss.Data.Color as Gloss
import Graphics.UI.GLUT

import SpatialMath

glColorOfColor :: Gloss.Color -> Color4 GLfloat
glColorOfColor = (\(r,g,b,a) -> fmap realToFrac (Color4 r g b a)) . Gloss.rgbaOfColor

setColor :: Gloss.Color -> IO ()
setColor = color . glColorOfColor

setMaterialDiffuse :: Gloss.Color -> IO ()
setMaterialDiffuse col = materialDiffuse Front $= (glColorOfColor col)

data VisObject a = VisCylinder (a,a) (Xyz a) (Quat a) Gloss.Color
                 | VisBox (a,a,a) (Xyz a) (Quat a) Gloss.Color
                 | VisLine [Xyz a] Gloss.Color
                 | VisArrow (a,a) (Xyz a) (Xyz a) Gloss.Color
                 | VisAxes (a,a) (Xyz a) (Quat a)
                 | VisPlane (Xyz a) a Gloss.Color Gloss.Color
                 | VisTriangle (Xyz a) (Xyz a) (Xyz a) Gloss.Color
                 | VisQuad (Xyz a) (Xyz a) (Xyz a) (Xyz a) Gloss.Color
                 | VisCustom (IO ())
                 | Vis3dText String (Xyz a) BitmapFont Gloss.Color
                 | Vis2dText String (a,a) BitmapFont Gloss.Color

instance Functor VisObject where
  fmap f (VisCylinder (x,y) xyz quat col) = VisCylinder (f x, f y) (fmap f xyz) (fmap f quat) col
  fmap f (VisBox (x,y,z) xyz quat col) = VisBox (f x, f y, f z) (fmap f xyz) (fmap f quat) col
  fmap f (VisLine xyzs col) = VisLine (map (fmap f) xyzs) col
  fmap f (VisArrow (x,y) xyz0 xyz1 col) = VisArrow (f x, f y) (fmap f xyz0) (fmap f xyz1) col
  fmap f (VisAxes (x,y) xyz quat) = VisAxes (f x, f y) (fmap f xyz) (fmap f quat)
  fmap f (VisPlane xyz x col0 col1) = VisPlane (fmap f xyz) (f x) col0 col1
  fmap f (VisTriangle x0 x1 x2 col) = VisTriangle (fmap f x0) (fmap f x1) (fmap f x2) col
  fmap f (VisQuad x0 x1 x2 x3 col) = VisQuad (fmap f x0) (fmap f x1) (fmap f x2) (fmap f x3) col
  fmap f (Vis3dText t xyz bmf col) = Vis3dText t (fmap f xyz) bmf col
  fmap f (Vis2dText t (x,y) bmf col) = Vis2dText t (f x, f y) bmf col
  fmap _ (VisCustom f) = VisCustom f

setPerspectiveMode :: IO ()
setPerspectiveMode = do
  (_, Size w h) <- get viewport
  matrixMode $= Projection
  loadIdentity
  perspective 40 (fromIntegral w / fromIntegral h) 0.1 100
  matrixMode $= Modelview 0


drawObjects :: [VisObject GLdouble] -> IO ()
drawObjects objects = do
  setPerspectiveMode
  mapM_ drawObject objects

drawObject :: VisObject GLdouble -> IO ()
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
    setMaterialDiffuse col
    setColor col
    glBegin gl_QUADS
    glVertex3d x0 y0 z0
    glVertex3d x1 y1 z1
    glVertex3d x2 y2 z2
    glVertex3d x3 y3 z3
    glEnd

-- cylinder
drawObject (VisCylinder (height,radius) (Xyz x y z) (Quat q0 q1 q2 q3) col) =
  preservingMatrix $ do
    setMaterialDiffuse col
    setColor col
    
    translate (Vector3 x y z :: Vector3 GLdouble)
    rotate (2 * acos q0 *180/pi :: GLdouble) (Vector3 q1 q2 q3)
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

-- box
drawObject (VisBox (dx,dy,dz) (Xyz x y z) (Quat q0 q1 q2 q3) col) =
  preservingMatrix $ do
    setMaterialDiffuse col
    setColor col
    translate (Vector3 x y z :: Vector3 GLdouble)
    rotate (2 * acos q0 *180/pi :: GLdouble) (Vector3 q1 q2 q3)
    normalize $= Enabled
    scale dx dy dz
    renderObject Solid (Cube 1)
    normalize $= Disabled

-- line
drawObject (VisLine path col) =
  preservingMatrix $ do
    lighting $= Disabled
    setColor col
    renderPrimitive LineStrip $ mapM_ (\(Xyz x' y' z') -> vertex$Vertex3 x' y' z') path
    lighting $= Enabled

-- plane
drawObject (VisPlane (Xyz x y z) offset col1 col2) =
  preservingMatrix $ do
    let norm = 1/(sqrt $ x*x + y*y + z*z)
        x' = x*norm
        y' = y*norm
        z' = z*norm
        r  = 10
        n  = 5
        eps = 0.01
    translate (Vector3 (offset*x') (offset*y') (offset*z') :: Vector3 GLdouble)
    rotate ((acos z')*180/pi :: GLdouble) (Vector3 (-y') x' 0)
    mapM_ drawObject $ concat [[ VisLine [Xyz (-r) y0 eps, Xyz r y0 eps] col1
                               , VisLine [Xyz x0 (-r) eps, Xyz x0 r eps] col1
                               ] | x0 <- [-r,-r+r/n..r], y0 <- [-r,-r+r/n..r]]
    mapM_ drawObject $ concat [[ VisLine [Xyz (-r) y0 (-eps), Xyz r y0 (-eps)] col1
                               , VisLine [Xyz x0 (-r) (-eps), Xyz x0 r (-eps)] col1
                               ] | x0 <- [-r,-r+r/n..r], y0 <- [-r,-r+r/n..r]]
    glBegin gl_QUADS
    setColor col2

    let r' = realToFrac r
    glVertex3f   r'    r'  0
    glVertex3f (-r')   r'  0
    glVertex3f (-r')  (-r')  0
    glVertex3f   r'   (-r')  0
    glEnd

-- arrow
drawObject (VisArrow (size, aspectRatio) (Xyz x0 y0 z0) (Xyz x y z) col) =
  preservingMatrix $ do
    let numSlices = 8
        numStacks = 15
        cylinderRadius = 0.5*size/aspectRatio
        cylinderHeight = size
        coneRadius = 2*cylinderRadius
        coneHeight = 2*coneRadius

        rotAngle = acos(z/(sqrt(x*x + y*y + z*z) + 1e-15))*180/pi :: GLdouble
        rotAxis = Vector3 (-y) x 0
    
    translate (Vector3 x0 y0 z0 :: Vector3 GLdouble)
    rotate rotAngle rotAxis
    
    -- cylinder
    drawObject $ VisCylinder (cylinderHeight, cylinderRadius) (Xyz 0 0 0) (Quat 1 0 0 0) col
    -- cone
    setMaterialDiffuse col
    setColor col
    translate (Vector3 0 0 cylinderHeight :: Vector3 GLdouble)
    renderObject Solid (Cone coneRadius coneHeight numSlices numStacks)

drawObject (VisAxes (size, aspectRatio) (Xyz x0 y0 z0) (Quat q0 q1 q2 q3)) = preservingMatrix $ do
  translate (Vector3 x0 y0 z0 :: Vector3 GLdouble)
  rotate (2 * acos q0 *180/pi :: GLdouble) (Vector3 q1 q2 q3)
  
  let xAxis = VisArrow (size, aspectRatio) (Xyz 0 0 0) (Xyz 1 0 0) (Gloss.makeColor 1 0 0 1)
      yAxis = VisArrow (size, aspectRatio) (Xyz 0 0 0) (Xyz 0 1 0) (Gloss.makeColor 0 1 0 1)
      zAxis = VisArrow (size, aspectRatio) (Xyz 0 0 0) (Xyz 0 0 1) (Gloss.makeColor 0 0 1 1)
  drawObjects [xAxis, yAxis, zAxis]

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