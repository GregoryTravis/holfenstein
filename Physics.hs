module Physics
( physics
) where

import Linear

import Map
import Math

physics :: World -> V2 Double -> V2 Double -> V2 Double
physics world oEye@(V2 ox oy) nEye@(V2 nx ny) = V2 cnx cny
  where (V2 ex ey) = floorV nEye
        nSolid = isSolid world ex ey
        lSolid = isSolid world (ex-1) ey
        rSolid = isSolid world (ex+1) ey
        dSolid = isSolid world ex (ey-1)
        uSolid = isSolid world ex (ey+1)
        margin = 0.25
        lMargin = ((fromIntegral ex) + margin)
        rMargin = ((fromIntegral (ex+1)) - margin)
        dMargin = ((fromIntegral ey) + margin)
        uMargin = ((fromIntegral (ey+1)) - margin)
        cnx = if lSolid && nx < lMargin
                then lMargin
                else if rSolid && nx > rMargin
                  then rMargin
                  else nx
        cny = if dSolid && ny < dMargin
                then dMargin
                else if uSolid && ny > uMargin
                  then uMargin
                  else ny
    

