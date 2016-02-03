{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# Language StandaloneDeriving #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}
{-# Language TypeSynonymInstances #-}

module Vis.VisObject ( VisObject(..)
                     , drawObjects
                     , LoadedObjModel
                     , loadObjModel
                     , setPerspectiveMode
                     ) where

import GHC.Generics ( Generic )

import Control.Monad ( when )
import qualified Data.Foldable as F
import Data.Maybe ( fromJust, isJust )
import Data.Word ( Word8 )
import qualified Data.Vector.Storable as VS
import qualified Data.Serialize as S
import qualified Data.Binary as B
import Data.Vector.Cereal ()
import Data.Vector.Binary ()
import Graphics.GL
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLUT as GLUT
import Graphics.UI.GLUT ( BitmapFont(..), Capability(..), Color4(..), Face(..)
                        , Flavour(..), MatrixMode(..), PrimitiveMode(..), Size(..)
                        , Vertex3(..), Vector3(..)
                        , ($=)
                        )

import SpatialMath

import qualified Vis.GlossColor as GlossColor

glColorOfColor :: GlossColor.Color -> Color4 GLfloat
glColorOfColor = (\(r,g,b,a) -> fmap realToFrac (Color4 r g b a)) . GlossColor.rgbaOfColor

setColor :: GlossColor.Color -> IO ()
setColor = GLUT.color . glColorOfColor

setMaterialDiffuse :: GlossColor.Color -> IO ()
setMaterialDiffuse col = GLUT.materialDiffuse Front $= (glColorOfColor col)

data VisObject a = VisObjects [VisObject a]
                 | Trans (V3 a) (VisObject a)
                 | RotQuat (Quaternion a) (VisObject a)
                 | RotDcm (M33 a) (VisObject a)
                 | RotEulerRad (Euler a) (VisObject a)
                 | RotEulerDeg (Euler a) (VisObject a) -- degrees more efficient
                 | Scale (a,a,a) (VisObject a)
                 | Cylinder (a,a) GlossColor.Color
                 | Box (a,a,a) Flavour GlossColor.Color
                 | Cube a Flavour GlossColor.Color
                 | Sphere a Flavour GlossColor.Color
                 | Ellipsoid (a,a,a) Flavour GlossColor.Color
                 | Line (Maybe a) [V3 a] GlossColor.Color
                 | Line' (Maybe a) [(V3 a,GlossColor.Color)]
                 | Arrow (a,a) (V3 a) GlossColor.Color
                 | Axes (a,a)
                 | Plane (V3 a) GlossColor.Color GlossColor.Color
                 | Triangle (V3 a) (V3 a) (V3 a) GlossColor.Color
                 | Quad (V3 a) (V3 a) (V3 a) (V3 a) GlossColor.Color
                 | Text3d String (V3 a) BitmapFont GlossColor.Color
                 | Text2d String (a,a) BitmapFont GlossColor.Color
                 | Points [V3 a] (Maybe GLfloat) GlossColor.Color
                 | ObjModel LoadedObjModel GlossColor.Color
                 deriving (Generic, Functor)

data LoadedObjModel = LoadedObjModel (VS.Vector Double) (VS.Vector Double) Int deriving (Generic)

instance S.Serialize LoadedObjModel
instance B.Binary LoadedObjModel

toFlavour :: Bool -> Flavour
toFlavour False = Solid
toFlavour True = Wireframe

fromFlavour :: Flavour -> Bool
fromFlavour Solid = False
fromFlavour Wireframe = True

instance S.Serialize Flavour where
  put = S.put . fromFlavour
  get = fmap toFlavour S.get
instance B.Binary Flavour where
  put = B.put . fromFlavour
  get = fmap toFlavour B.get


fromBitmapFont :: BitmapFont -> Word8
fromBitmapFont Fixed8By13   = 0 :: Word8
fromBitmapFont Fixed9By15   = 1 :: Word8
fromBitmapFont TimesRoman10 = 2 :: Word8
fromBitmapFont TimesRoman24 = 3 :: Word8
fromBitmapFont Helvetica10  = 4 :: Word8
fromBitmapFont Helvetica12  = 5 :: Word8
fromBitmapFont Helvetica18  = 6 :: Word8

toBitmapFont :: Word8 -> BitmapFont
toBitmapFont 0 = Fixed8By13
toBitmapFont 1 = Fixed9By15
toBitmapFont 2 = TimesRoman10
toBitmapFont 3 = TimesRoman24
toBitmapFont 4 = Helvetica10
toBitmapFont 5 = Helvetica12
toBitmapFont 6 = Helvetica18
toBitmapFont k = error $ "deserializing BitmapFont got bad value (" ++ show k ++ ")"

instance S.Serialize BitmapFont where
  put = S.put . fromBitmapFont
  get = fmap toBitmapFont S.get
instance B.Binary BitmapFont where
  put = B.put . fromBitmapFont
  get = fmap toBitmapFont B.get


fromColor :: GlossColor.Color -> (Float,Float,Float,Float)
fromColor = GlossColor.rgbaOfColor

toColor :: (Float,Float,Float,Float) -> GlossColor.Color
toColor (r,g,b,a) = GlossColor.makeColor r g b a

instance S.Serialize (GlossColor.Color) where
  put = S.put . fromColor
  get = fmap toColor S.get
instance B.Binary (GlossColor.Color) where
  put = B.put . fromColor
  get = fmap toColor B.get


instance S.Serialize a => S.Serialize (VisObject a)
instance B.Binary a => B.Binary (VisObject a)

setPerspectiveMode :: IO ()
setPerspectiveMode = do
  (_, Size w h) <- GLUT.get GLUT.viewport
  GLUT.matrixMode $= Projection
  GLUT.loadIdentity
  GLUT.perspective 40 (fromIntegral w / fromIntegral h) 0.1 1000
  GLUT.matrixMode $= Modelview 0

drawObjects :: VisObject GLdouble -> IO ()
drawObjects objects = do
  setPerspectiveMode
  drawObject objects

drawObject :: VisObject GLdouble -> IO ()
-- list of objects
drawObject (VisObjects xs) = mapM_ drawObject xs

-- list of objects
drawObject (Trans (V3 x y z) visobj) =
  GLUT.preservingMatrix $ do
    GLUT.translate (Vector3 x y z :: Vector3 GLdouble)
    drawObject visobj

drawObject (RotQuat (Quaternion q0 (V3 q1 q2 q3)) visobj) =
  GLUT.preservingMatrix $ do
    GLUT.rotate (2 * acos q0 *180/pi :: GLdouble) (Vector3 q1 q2 q3)
    drawObject visobj

drawObject (RotDcm dcm visobject) =
  drawObject (RotQuat (quatOfDcm dcm) visobject)

drawObject (RotEulerRad euler visobj) =
  drawObject $ RotEulerDeg (fmap ((180/pi)*) euler) visobj

drawObject (RotEulerDeg (Euler yaw pitch roll) visobj) =
  GLUT.preservingMatrix $ do
    GLUT.rotate yaw   (Vector3 0 0 1)
    GLUT.rotate pitch (Vector3 0 1 0)
    GLUT.rotate roll  (Vector3 1 0 0)
    drawObject visobj

drawObject (Scale (sx,sy,sz) visobj) =
  GLUT.preservingMatrix $ do
    GLUT.normalize $= Enabled
    GLUT.scale sx sy sz
    drawObject visobj
    GLUT.normalize $= Disabled

-- triangle
drawObject (Triangle (V3 x0 y0 z0) (V3 x1 y1 z1) (V3 x2 y2 z2) col) =
  GLUT.preservingMatrix $ do
    setMaterialDiffuse col
    setColor col
    glBegin GL_TRIANGLES
    glVertex3d x0 y0 z0
    glVertex3d x1 y1 z1
    glVertex3d x2 y2 z2
    glEnd

-- quad
drawObject (Quad (V3 x0 y0 z0) (V3 x1 y1 z1) (V3 x2 y2 z2) (V3 x3 y3 z3) col) =
  GLUT.preservingMatrix $ do
    GLUT.lighting $= Disabled
    setColor col
    glBegin GL_QUADS
    glVertex3d x0 y0 z0
    glVertex3d x1 y1 z1
    glVertex3d x2 y2 z2
    glVertex3d x3 y3 z3
    glEnd
    GLUT.lighting $= Enabled

-- cylinder
drawObject (Cylinder (height,radius) col) =
  GLUT.preservingMatrix $ do
    setMaterialDiffuse col
    setColor col

    -- GLUT.translate (Vector3 0 0 (-height/2) :: Vector3 GLdouble)

    let nslices = 10 :: Int
        nstacks = 10 :: Int

        -- Pre-computed circle
        sinCosTable = map (\q -> (sin q, cos q)) angles
          where
            angle = 2*pi/(fromIntegral nslices)
            angles = reverse $ map ((angle*) . fromIntegral) [0..(nslices+1)]

    -- Cover the base and top
    glBegin GL_TRIANGLE_FAN
    glNormal3d 0 0 (-1)
    glVertex3d 0 0 0
    mapM_ (\(s,c) -> glVertex3d (c*radius) (s*radius) 0) sinCosTable
    glEnd

    glBegin GL_TRIANGLE_FAN
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
          glBegin GL_QUAD_STRIP
          mapM_ (drawSlice z0 z1) sinCosTable
          glEnd

    mapM_ drawSlices $ zip (init zSteps) (tail zSteps)

-- sphere
drawObject (Sphere r flav col) =
  GLUT.preservingMatrix $ do
    setMaterialDiffuse col
    setColor col
    GLUT.renderObject flav (GLUT.Sphere' (realToFrac r) 20 20)

-- ellipsoid
drawObject (Ellipsoid (sx,sy,sz) flav col) = drawObject $ Scale (sx,sy,sz) $ Sphere 1 flav col

-- box
drawObject (Box (dx,dy,dz) flav col) = drawObject $ Scale (dx,dy,dz) $ Cube 1 flav col

drawObject (Cube r flav col) =
  GLUT.preservingMatrix $ do
    setMaterialDiffuse col
    setColor col
    GLUT.renderObject flav (GLUT.Cube (realToFrac r))

-- line
drawObject (Line width path col) =
  GLUT.preservingMatrix $ do
    GLUT.lighting $= Disabled
    setColor col
    lineWidth0 <- GLUT.get GLUT.lineWidth
    case width of
     Just w -> GLUT.lineWidth $= realToFrac w
     Nothing -> return ()

    GLUT.renderPrimitive LineStrip $ mapM_ (\(V3 x' y' z') -> GLUT.vertex $ Vertex3 x' y' z') path
    GLUT.lineWidth $= lineWidth0
    GLUT.lighting $= Enabled

-- line where you set the color at each vertex
drawObject (Line' width pathcols) =
  GLUT.preservingMatrix $ do
    GLUT.lighting $= Disabled

    lineWidth0 <- GLUT.get GLUT.lineWidth
    case width of
     Just w -> GLUT.lineWidth $= realToFrac w
     Nothing -> return ()

    glBegin GL_LINE_STRIP
    let f (xyz, col) = do
          let V3 x y z = fmap realToFrac xyz
          setMaterialDiffuse col
          setColor col
          glVertex3f x y z
    mapM_ f pathcols
    glEnd
    GLUT.lineWidth $= lineWidth0
    GLUT.lighting $= Enabled

-- plane
drawObject (Plane (V3 x y z) col1 col2) =
  GLUT.preservingMatrix $ do
    let normInv = 1/(sqrt $ x*x + y*y + z*z)
        x' = x*normInv
        y' = y*normInv
        z' = z*normInv
        r  = 10
        n  = 5
        eps = 0.01
    GLUT.rotate ((acos z')*180/pi :: GLdouble) (Vector3 (-y') x' 0)

    glBegin GL_QUADS
    setColor col2

    let r' = realToFrac r
    glVertex3f   r'    r'  0
    glVertex3f (-r')   r'  0
    glVertex3f (-r')  (-r')  0
    glVertex3f   r'   (-r')  0
    glEnd

    glDisable GL_BLEND
    let drawWithEps eps' = do
          mapM_ drawObject $ concat [[ Line Nothing
                                            [ V3 (-r) y0 eps'
                                            , V3 r    y0 eps'
                                            ] col1
                                     , Line Nothing
                                            [ V3 x0 (-r) eps',
                                              V3 x0 r    eps'
                                            ] col1
                                     ] | x0 <- [-r,-r+r/n..r], y0 <- [-r,-r+r/n..r]]
    drawWithEps eps
    drawWithEps (-eps)

    glEnable GL_BLEND


-- arrow
drawObject (Arrow (size, aspectRatio) (V3 x y z) col) =
  GLUT.preservingMatrix $ do
    let numSlices = 8
        numStacks = 15
        cylinderRadius = 0.5*size/aspectRatio
        cylinderHeight = size
        coneRadius = 2*cylinderRadius
        coneHeight = 2*coneRadius

        rotAngle = acos(z/(sqrt(x*x + y*y + z*z) + 1e-15))*180/pi :: GLdouble
        rotAxis = Vector3 (-y) x 0

    GLUT.rotate rotAngle rotAxis

    -- cylinder
    drawObject $ Cylinder (cylinderHeight, cylinderRadius) col
    -- cone
    setMaterialDiffuse col
    setColor col
    GLUT.translate (Vector3 0 0 cylinderHeight :: Vector3 GLdouble)
    GLUT.renderObject Solid (GLUT.Cone coneRadius coneHeight numSlices numStacks)

drawObject (Axes (size, aspectRatio)) = GLUT.preservingMatrix $ do
  let xAxis = Arrow (size, aspectRatio) (V3 1 0 0) (GlossColor.makeColor 1 0 0 1)
      yAxis = Arrow (size, aspectRatio) (V3 0 1 0) (GlossColor.makeColor 0 1 0 1)
      zAxis = Arrow (size, aspectRatio) (V3 0 0 1) (GlossColor.makeColor 0 0 1 1)
  drawObject $ VisObjects [xAxis, yAxis, zAxis]

drawObject (Text3d string (V3 x y z) font col) = GLUT.preservingMatrix $ do
  GLUT.lighting $= Disabled
  setColor col
  glRasterPos3d x y z
  GLUT.renderString font string
  GLUT.lighting $= Enabled

drawObject (Text2d string (x,y) font col) = GLUT.preservingMatrix $ do
  GLUT.lighting $= Disabled
  setColor col

  GLUT.matrixMode $= Projection
  GLUT.loadIdentity

  (_, Size w h) <- GLUT.get GLUT.viewport
  GLUT.ortho2D 0 (fromIntegral w) 0 (fromIntegral h)
  GLUT.matrixMode $= Modelview 0
  GLUT.loadIdentity

  glRasterPos2d x y
  GLUT.renderString font string

  setPerspectiveMode
  GLUT.lighting $= Enabled

drawObject (Vis.VisObject.Points xyzs ps col) =
  GLUT.preservingMatrix $ do
    GLUT.lighting $= Disabled
    setColor col
    s' <- GLUT.get GLUT.pointSize
    when (isJust ps) $ GLUT.pointSize $= (fromJust ps)
    GLUT.renderPrimitive GLUT.Points $
      mapM_ (\(V3 x' y' z') -> GLUT.vertex $ Vertex3 x' y' z') xyzs
    GLUT.pointSize $= s'
    GLUT.lighting $= Enabled

drawObject (Vis.VisObject.ObjModel (LoadedObjModel vvec nvec numVerts) col) =
  GLUT.preservingMatrix $ do
    setMaterialDiffuse col
    setColor col

    -- enable vertex/normal arrays
    -- todo: Should this be done every time?
    --       Either enable at the start, or push/pop to preserve user attributes
    GL.clientState GL.VertexArray $= GL.Enabled
    GL.clientState GL.NormalArray $= GL.Enabled

    -- set the vertex and normal arrays
    let va = GL.VertexArrayDescriptor 3 GL.Double 0
        na = GL.VertexArrayDescriptor 3 GL.Double 0
    VS.unsafeWith vvec $ \vptr -> GL.arrayPointer GL.VertexArray $= va vptr
    VS.unsafeWith nvec $ \nptr -> GL.arrayPointer GL.NormalArray $= na nptr
    -- draw the triangles
    GL.drawArrays GL.Triangles 0 (fromIntegral numVerts)

    -- disable vertex/normal arrays
    GL.clientState GL.VertexArray $= GL.Disabled
    GL.clientState GL.NormalArray $= GL.Disabled

-- | turn a list of vertex/normal tuples into vertex/normal arrays
loadObjModel :: F.Foldable f => f (V3 Double, V3 Double) -> LoadedObjModel
loadObjModel vns = LoadedObjModel (VS.fromList vs) (VS.fromList ns) n
  where
    vs = F.concatMap (\(V3 x y z) -> [x,y,z]) vs'
    ns = F.concatMap (\(V3 x y z) -> [x,y,z]) ns'
    (vs',ns') = unzip $ F.toList vns
    n = length vs'
