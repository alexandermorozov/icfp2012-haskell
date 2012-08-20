module Field (Point(..), packPoint, addPoint, pointX, pointY, unpackPoint,
              Field, fieldGet, fieldSet, newField)
              where

import Data.Bits


newtype Point = Point Int deriving (Eq, Ord, Show)

-- Element mapping:
-- 0 1
-- 2 3
data Field a = Node a
             | Tile !(Field a) !(Field a) !(Field a) !(Field a)
               deriving (Show)

pBits, pMask, sMask :: Int
pBits = 14
pMask = 2^(pBits-1) - 1
sMask = complement $ shift 1 (pBits-1) + shift 1 (pBits*2-1)

packPoint :: Int -> Int -> Point
packPoint x y = Point (shift (y .&. pMask) pBits + (x .&. pMask))

addPoint :: Point -> Point -> Point
addPoint (Point a) (Point b) = let s = a + b
                               in Point (s .&. sMask)

-- unpacks only as positive integers
pointX, pointY :: Point -> Int
pointX (Point a) = a .&. pMask
pointY (Point a) = shift a (-pBits) .&. pMask

unpackPoint :: Point -> (Int, Int)
unpackPoint p = (pointX p, pointY p)


split :: Point -> (Point, Bool, Bool)
split (Point a) = (Point $ shift a (-1), testBit a 0, testBit a pBits)


fieldGet :: Point -> Field a -> a
fieldGet _ (Node a) = a 
fieldGet p (Tile f0 f1 f2 f3) =
    let (p', rx, ry) = split p
        next = case (rx, ry) of
                 (False,False) -> f0
                 (True, False) -> f1
                 (False,True)  -> f2
                 (True, True)  -> f3
    in fieldGet p' next

fieldSet :: Point -> a -> Field a -> Field a
fieldSet p a (Node _) = Node a
fieldSet p a (Tile f0 f1 f2 f3) =
    let (p', rx, ry) = split p
    in case (rx, ry) of
           (False,False) -> Tile (fieldSet p' a f0) f1 f2 f3
           (True, False) -> Tile f0 (fieldSet p' a f1) f2 f3 
           (False,True)  -> Tile f0 f1 (fieldSet p' a f2) f3
           (True, True)  -> Tile f0 f1 f2 (fieldSet p' a f3)

newField :: Int -> a -> Field a
newField 0 a = Node a
newField d a = let d' = d - 1 
          in  Tile (newField d' a) (newField d' a) (newField d' a) (newField d' a)
