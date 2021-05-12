{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
import CodeWorld hiding ((<>))

-- import Data.Semigroup (Semigroup(..))

main :: IO ()
main = activityOf (initUniverse sampleStarSystem) handleUniverse drawUniverse

-- | A solar system with some planets.
sampleStarSystem :: [Body]
sampleStarSystem = [ sun, venus, earth, moon ]
  where
    sun = Body (-100, 0) (0, 10) 200000

    venus   = orbitingAt (-50, 120)  sun 5000
    earth   = orbitingAt (370, -50) sun 50000
    moon    = orbitingAt (0, 50) earth 500

    orbitingAt pos body mass = Body point (orbitVelocity body point) mass
      where
        point = vectorSum (bodyPosition body) pos

-- ==============================================
-- Model
-- ==============================================

-- | Universe model.
data Universe = Universe
  { universeBodies    :: [Body]   -- ^ Bodies in the universe.
  , universeField     :: Field    -- ^ Force fields (based on bodies).
  , universeBounds    :: Bounds   -- ^ Modelling bounds.
  , universeArrowSize :: Double   -- ^ Size for the vector arrows (for visualisation).
  }

-- | Body mass.
type Mass = Double

-- | A body (e.g. star or a planet).
data Body = Body
  { bodyPosition :: Point   -- ^ Location in space.
  , bodyVelocity :: Vector  -- ^ Velocity vector.
  , bodyMass     :: Mass    -- ^ Mass.
  }

-- | Vector field.
newtype Field = Field { getField :: Point -> Vector }

addVectors :: Vector -> Vector -> Vector
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

addFields :: Field -> Field -> Field
addFields f1 f2 = Field (\p -> (getField f1) p `addVectors` (getField f2) p)

instance Semigroup Field where
  (<>) = addFields

instance Monoid Field where
  mempty = Field (\_ -> (0, 0))

-- | Modelled universe bounds.
data Bounds
  = Unbounded                 -- ^ Unbounded universe.
  | Rectangular Point Point   -- ^ Rectangulare area.

-- | Determine bounds that capture all bodies.
bounds :: [Body] -> Bounds
bounds _ = Unbounded  -- TODO: implement yourself (hint: use monoids)

-- | Initialise a universe.
initUniverse :: [Body] -> Universe
initUniverse bodies = Universe
  { universeBodies = bodies
  , universeField  = systemField bodies
  , universeBounds = toScreenProps (bounds bodies)
  , universeArrowSize = 0
  }

-- ==============================================
-- Physics
-- ==============================================

-- | Gravity acceleration field for a single object.
bodyField :: Body -> Field
bodyField = Field . accel

-- | Gravity acceleration field for system of objects.
systemField :: [Body] -> Field
systemField = mconcat . map bodyField

-- | Compute gravity acceleration that a body generates
-- for another object at a given point.
accel :: Body -> Point -> Vector
accel body point
  | r == (0, 0) = (0, 0)  -- we ignore the point where the object is
  | otherwise   = scaledVector (bigG * m / d^2) n
  where
    m = bodyMass body
    r = vectorDifference (bodyPosition body) point
    n = scaledVector (1 / vectorLength r) r
    d = vectorLength r

-- | Velocity vector necessary for a circular orbit.
orbitVelocity :: Body -> Point -> Vector
orbitVelocity body point = bodyVelocity body `vectorSum` v
  where
    v = scaledVector
          (sqrt (vectorLength r * vectorLength a))
          (rotatedVector (pi/2) n)
    a = accel body point
    r = vectorDifference (bodyPosition body) point
    n = scaledVector (1 / vectorLength r) r

-- ==============================================
-- Rendering
-- ==============================================

-- | Render a universe.
drawUniverse :: Universe -> Picture
drawUniverse universe = scaled (1/unit) (1/unit) (
    scaleToBounds (universeBounds universe) (pictures
    [ drawField 30 (universeBounds universe)
                   (universeArrowSize universe)
                   (universeField universe)
    , pictures (fmap drawBody (universeBodies universe))
    ])
  )

-- | Render a body (as a circle).
drawBody :: Body -> Picture
drawBody body = translated x y (solidCircle r)
  where
    (x, y) = bodyPosition body
    r = bodyMass body ** 0.3

-- | Render a vector field.
drawField :: Double -> Bounds -> Double -> Field -> Picture
drawField cellSize boundary arrowSize field
  = pictures (map (drawFieldAtPoint (cellSize * arrowSize) field) points)
  where
    points =
      [ (x, y)
      | x <- [l, l + cellSize .. r]
      , y <- [b, b + cellSize .. t]
      ]
    ((l, b), (r, t)) = case boundary of
      Unbounded ->
        ((-screenWidth/2, -screenHeight/2), (screenWidth/2, screenHeight/2))
      Rectangular lb rt -> (lb, rt)

-- | Render a vector of a field at a single point.
drawFieldAtPoint :: Double -> Field -> Point -> Picture
drawFieldAtPoint arrowSize (Field f) (x, y)
  = colored c (translated x y (scaled s s (rotated theta arrow)))
  where
    c = RGBA m 0 (1 - m) 1
    s = arrowSize * m
    v = f (x, y)
    theta = angle (1, 0) v
    m = 1 - exp (log 0.5 * vectorLength v / mediumAccel)

-- | Angle between two vectors.
angle :: Vector -> Vector -> Double
angle v1 v2 = - atan2 y x
  where
    (x, y) = vectorDifference v2 v1

-- | A unit arrow (of length 1).
arrow :: Picture
arrow = pictures
  [ solidPolygon [ (0, w), (1 - hl, w), (1 - hl, -w), (0, -w) ]  -- body
  , solidPolygon [ (1 - hl, hw), (1, 0), (1 - hl, -hw) ]         -- head
  ]
  where
    w  = 0.05 -- half-width of an arrow
    hw = 0.2  -- half-width of an arrow head
    hl = 0.4  -- length of an arrow head

-- | Convert system bounds to screen bounds.
-- New area will be larger or stay the same.
toScreenProps :: Bounds -> Bounds
toScreenProps Unbounded = Unbounded
toScreenProps (Rectangular (l, b) (r, t))
  | tooWide   = Rectangular (l, b') (r, t')
  | otherwise = Rectangular (l', b) (r', t)
  where
    tooWide = (r - l) * screenHeight > (t - b) * screenWidth
    l' = (r + l - (t - b) * screenWidth / screenHeight) / 2
    r' = (r + l + (t - b) * screenWidth / screenHeight) / 2
    b' = (t + b - (r - l) * screenHeight / screenWidth) / 2
    t' = (t + b + (r - l) * screenHeight / screenWidth) / 2

-- | Scale image so that it spreads across the whole screen.
scaleToBounds :: Bounds -> Picture -> Picture
scaleToBounds Unbounded = id
scaleToBounds (Rectangular (l, b) (r, t))
  = scaled sx sy . translated dx dy
  where
    dx = - (l + r) / 2
    dy = - (b + t) / 2
    sx = screenWidth / (r - l)
    sy = screenHeight / (t - b)

-- ==============================================
-- Event handlers and updating
-- ==============================================

-- | Event handler.
handleUniverse :: Event -> Universe -> Universe
handleUniverse (TimePassing dt) = updateUniverse dt
handleUniverse (KeyPress " ") = toggleField
handleUniverse _ = id

-- | Toggle gravity field visualisation.
toggleField :: Universe -> Universe
toggleField universe = universe { universeArrowSize = newArrowSize }
  where
    newArrowSize
      | universeArrowSize universe == 0 = 0.01
      | otherwise = 0

-- | Update the whole universe.
updateUniverse :: Double -> Universe -> Universe
updateUniverse dt universe = (initUniverse newBodies)
  { universeArrowSize = newArrowSize }
  where
    newBodies = map (updateBody dt field) bodies
    newArrowSize
      | arrowSize == 0 = 0
      | otherwise      = min 0.7 (arrowSize + dt * 0.8)

    bodies    = universeBodies universe
    field     = universeField universe
    arrowSize = universeArrowSize universe

-- | Update a single body.
updateBody :: Double -> Field -> Body -> Body
updateBody dt (Field f) body = body
  { bodyPosition = bodyPosition body `vectorSum` scaledVector dt (bodyVelocity body)
  , bodyVelocity = bodyVelocity body `vectorSum` scaledVector dt (f (bodyPosition body))
  }

-- ==============================================
-- Constants and model parameters
-- ==============================================

-- | Gravity coefficient.
bigG :: Double
bigG = 4

-- | Absolute average acceleration.
-- Used to calibrate visualisation.
mediumAccel :: Double
mediumAccel = vectorLength (accel (Body (0, 0) (0, 0) 20000) (100, 0))

-- | Screen width in CodeWorld units.
screenWidth :: Double
screenWidth = 800

-- | Screen height in CodeWorld units.
screenHeight :: Double
screenHeight = 600

-- | One unit in pixels.
unit :: Double
unit = 50
