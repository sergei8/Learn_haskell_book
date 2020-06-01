module Geometry
  ( sphereVolume
  , sphereArea
  , cubeVolume) where

    sphereVolume :: Float -> Float
    sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)

    sphereArea :: Float -> Float
    sphereArea radius = 4.0 * pi * (radius ^ 2)

    cubeVolume :: Float -> Float
    cubeVolume side = side ^ 3
