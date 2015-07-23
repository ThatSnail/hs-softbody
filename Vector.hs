module Vector (
      Vector ((.+), (.-), (.*) , (./), mag)
    , Vector2D (Vector2D)
    , Vector2DP (Vector2DP)
    , rp2
    , pr2
    ) where

import Prelude hiding (negate)

class Vector v where
    (.+) :: v -> v -> v
    (.-) :: v -> v -> v
    v1 .- v2 = v1 .+ (negate v2)
    (.*) :: v -> Float -> v
    (./) :: v -> Float -> v
    v1 ./ c = v1 .* (1 / c)
    mag :: v -> Float
    negate :: v -> v

data Vector2D = Vector2D {
      x :: Float
    , y :: Float
} deriving (Eq, Show)

data Vector2DP = Vector2DP {
      radius :: Float
    , angle :: Float
} deriving (Eq, Show)

rp2 :: Vector2D -> Vector2DP
rp2 v@(Vector2D vx vy) = Vector2DP r a
    where
        r = mag v
        a = atan2 vy vx

pr2 :: Vector2DP -> Vector2D
pr2 v@(Vector2DP vr va) = Vector2D vx vy
    where
        vx = vr * (cos va)
        vy = vr * (sin va)

instance Vector Vector2D where
    v1 .+ v2 = Vector2D (x v1 + x v2) (y v1 + y v2)
    v .* a = Vector2D (x v * a) (y v * a)
    mag v = sqrt $ (x v) ** 2 + (y v) ** 2
    negate (Vector2D x y) = Vector2D (-x) (-y)

instance Vector Vector2DP where
    v1 .+ v2 = rp2 $ pr2 v1 .+ pr2 v2
    v .* a = Vector2DP (radius v * a) (angle v)
    mag = radius
    negate (Vector2DP r a) = Vector2DP r (a + pi)
