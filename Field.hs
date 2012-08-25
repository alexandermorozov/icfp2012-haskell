module Field ( Point(..), toPoint, fromPoint, addPoints
             , Field, field, set, get, empty
             ) where

import Data.Bits


newtype Point = Point Int deriving (Eq, Ord, Show)

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

pBits, pMask, sMask :: Int
pBits = 14
pMask = 2^(pBits-1) - 1
sMask = complement $ shift 1 (pBits-1) + shift 1 (pBits*2-1)

toPoint :: Int -> Int -> Point
toPoint x y = Point (shift (y .&. pMask) pBits + (x .&. pMask))

addPoints :: Point -> Point -> Point
addPoints (Point a) (Point b) = let s = a + b
                               in Point (s .&. sMask)

fromPoint :: Point -> (Int, Int)
fromPoint (Point a) = (a .&. pMask, shift a (-pBits) .&. pMask)


getBits' :: Int -> Point -> (Bool, Bool)
getBits' shift (Point a) = (testBit a shift, testBit a (pBits + shift))

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

mOne = toPoint (-1) (-1)

get :: Point -> Field a -> a
get p (Field depth root) = helper root (depth-1)
  where
    p0 = (addPoints p mOne)
    helper (Leaf a) _ = a
    helper t@(Tile _ _ _ _) d =
        let (rx, ry) = getBits' d p0
            next = case (rx, ry) of
                     (False,False) -> f0
                     (True, False) -> f1
                     (False,True)  -> f2
                     (True, True)  -> f3
        in helper (next t) (d-1)

set :: Point -> a -> Field a -> Field a
set p a f@(Field depth root) = f {fRoot = helper root (depth - 1)}
  where
    p0 = (addPoints p mOne)
    helper (Leaf _) d = Leaf a
    helper (Tile f0 f1 f2 f3) d =
        let (rx, ry) = getBits' d p0
            d' = d - 1
        in case (rx, ry) of
               (False,False) -> Tile (helper f0 d') f1 f2 f3
               (True, False) -> Tile f0 (helper f1 d') f2 f3
               (False,True)  -> Tile f0 f1 (helper f2 d') f3
               (True, True)  -> Tile f0 f1 f2 (helper f3 d')

empty :: Field a
empty = Field 0 Empty

