module MathUtil exposing (vecToTuple, tupleToVec)

import Math.Vector2 exposing (Vec2, vec2, getX, getY)

vecToTuple : Vec2 -> (Float, Float)
vecToTuple vec =
    (getX vec, getY vec)


tupleToVec : (Float, Float) -> Vec2
tupleToVec (x, y) =
    vec2 x y
