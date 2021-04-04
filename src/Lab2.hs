{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}

-- | N-body simulation.
module Lab2
  ( runSim,
  )
where

import CodeWorld

runSim :: IO ()
runSim = activityOf initialState handleSystem drawSystem

initialState :: System
initialState =
  System
    [ Body {mass = 5.972e24, pos = Position 2 3, v = (0, 0), color = RGB 1 0 0},
      Body {mass = 1.989e30, pos = Position 0 0, v = (0, 0), color = RGB 1 1 0}
    ]

-- * Types

-- | Mass (in kilograms).
type Mass = Double

-- | Position (coordinates in meters).
data Position = Position
  { x :: Double,
    y :: Double
  }

-- | Velocity vector (components in meters per second).
type Velocity = (Double, Double)

-- | An astronomical body (e.g. a planet or an asteroid).
data Body = Body
  { mass :: Mass,
    pos :: Position,
    v :: Velocity,
    color :: Color
  }

-- | A system of bodies.
newtype System = System
  { bodies :: [Body]
  }

-- * Rendering

-- | Render a single body, taking visualisation constants into account.
drawBody :: Body -> Picture
drawBody body =
  translated
    (x (pos body))
    (y (pos body))
    ( colored
        (color body)
        (solidCircle (massToRadius (mass body)))
    )

drawBodies :: [Body] -> Picture
drawBodies = foldr ((<>) . drawBody) blank

-- | Render a system of bodies on a black background.
drawSystem :: System -> Picture
drawSystem system =
  drawBodies (bodies system) <> background
  where
    background = solidRectangle 100 100

-- * Physics (updates)

-- | Update body's position based on its velocity.
moveBody :: Double -> Body -> Body
moveBody = _exercise

-- | Update body's position and velocity.
updateBody :: Double -> [Body] -> Body -> Body
updateBody dt bodies = moveBody dt . applyGravity dt bodies

-- | Update all bodies in a system.
updateBodies :: Double -> [Body] -> [Body]
updateBodies dt bodies =
  map (moveBody dt . applyGravity dt bodies) bodies

-- | Update entire system, taking 'timeScale' into account.
updateSystem :: Double -> System -> System
updateSystem dt = _exercise (dt * timeScale)

-- ** Gravity helpers

-- | Acceleration in a two body system.
--
-- NOTE: ignores gravitional effect of "small" objects
gravityAcc ::
  -- | Body whose gravitational field is used.
  Body ->
  -- | Body to compute acceleration for.
  Body ->
  Vector
gravityAcc other this
  | mass other < smallMassThreshold = (0, 0)
  | otherwise = (dx, dy)
  where
    f = bigG * mass other * mass this / dst2
    dx = f * cos ((y (pos other) - y (pos this)) / dst) / mass this
    dy = f * sin ((x (pos other) - x (pos this)) / dst) / mass this
    dst2 = (x (pos other) - x (pos this)) ^ 2 + (y (pos other) - y (pos this)) ^ 2
    dst = sqrt dst2

-- Hint: use vectorLength, vectorDifference and scaledVector

-- | Compute and apply acceleration to update body's velocity.
applyGravity :: Double -> [Body] -> Body -> Body
applyGravity dt [] body = body
applyGravity dt (other : bodies) body =
  -- Drop out the self-comparison
  if x (pos other) - x (pos body) < 0.000001 && y (pos other) - y (pos body) < 0.000001
    then body
    else body {v = v body `vectorSum` scaledVector dt (gravityAcc other body)}

-- * Controls

-- | Handle user input (e.g. mouse clicks).
handleSystem :: Event -> System -> System
-- handleSystem _ = _exercise
handleSystem _ = id -- ignore all events

-- * Helpers

-- | Convert pointer position into coordinates in a simulation.
fromPointer :: Point -> Point
fromPointer (x, y) = (x * viewportScale, y * viewportScale)

-- * Constants

-- ** Physical constants

-- | One astronomical unit (in meters).
au :: Double
au = 149597900000

-- | Gravitational constant.
bigG :: Double
bigG = 6.67430e-11

-- ** Visualisation parameters

-- | Viewport scaling factor.
--
-- For inner solar system: 1 unit = 0.2 'au'.
-- For Earth-Moon: 1 unit = 0.0005 'au'.
viewportScale :: Double
viewportScale = _exercise

-- | Time scale for the simulation.
--
-- For inner solar system: 1 second = 1 week
-- For Earth-Moon: 1 second = 1 day
timeScale :: Double
timeScale = _exercise

-- | Mass to visualisation radius mapping.
-- For nicer visualisation we use logarithmic scale.
massToRadius :: Mass -> Double
massToRadius m = 0.01 + 3e-7 * logBase 10 (m + 10) ^ 4

-- | Smallest mass to take gravity into account for.
smallMassThreshold :: Mass
smallMassThreshold = 1e21