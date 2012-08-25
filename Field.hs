module Field (Field, field, set, get, empty) where

import Point as P

-- Element mapping:
-- 0 1
-- 2 3
data Field a = Field {fDepth :: !Int, fRoot :: !(Node a)}
               deriving (Show)

data Node a = Tile { f0 :: !(Node a)
                   , f1 :: !(Node a)
                   , f2 :: !(Node a)
                   , f3 :: !(Node a)
                   }
            | Leaf a
            | Empty
              deriving (Show)


field :: a -> Int -> Int -> Field a
field def w h = Field (depth w h) (makeF w h)
  where
    depth w h = head $ filter (\n -> 2^n >= max w h) [0..]
    half w h = 2^(depth w h - 1)
    makeF 0 _ = Empty
    makeF _ 0 = Empty
    makeF 1 1 = Leaf def
    makeF w h = let d = half w h
                    (w0, w1) = (min w d, max 0 (w - d))
                    (h0, h1) = (min h d, max 0 (h - d))
                in Tile (makeF w0 h0) (makeF w1 h0) (makeF w0 h1) (makeF w1 h1)

dec = P.add $ P.pack (-1) (-1)

get :: Point -> Field a -> a
get p (Field depth root) = helper root (depth-1)
  where
    p0 = dec p
    helper (Leaf a) _ = a
    helper t@(Tile _ _ _ _) d =
        let next = case getBits p0 d of
                     (False,False) -> f0
                     (True, False) -> f1
                     (False,True)  -> f2
                     (True, True)  -> f3
        in helper (next t) (d-1)

set :: Point -> a -> Field a -> Field a
set p a f@(Field depth root) = f {fRoot = helper root (depth - 1)}
  where
    p0 = dec p
    helper (Leaf _) d = Leaf a
    helper (Tile f0 f1 f2 f3) d =
        let d' = d - 1
        in case getBits p0 d of
               (False,False) -> Tile (helper f0 d') f1 f2 f3
               (True, False) -> Tile f0 (helper f1 d') f2 f3
               (False,True)  -> Tile f0 f1 (helper f2 d') f3
               (True, True)  -> Tile f0 f1 f2 (helper f3 d')

empty :: Field a
empty = Field 0 Empty

