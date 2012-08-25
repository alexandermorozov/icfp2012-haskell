module Point (Point, pack, unpack, add, getBits) where

import Data.Bits
import Data.Word
import Data.Digest.Murmur64

newtype Point = Point Word32 deriving (Eq, Ord, Show)

instance Hashable64 Point where
    hash64Add (Point a) = hash64AddInt (fromIntegral a)

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

pBits :: Int
pMask, sMask :: Word32
pBits = 14
pMask = 2^(pBits-1) - 1
sMask = complement $ shift 1 (pBits-1) + shift 1 (pBits*2-1)

pack :: Int -> Int -> Point
pack x y = Point (shift (fromIntegral y .&. pMask) pBits +
                           (fromIntegral x .&. pMask))

unpack :: Point -> (Int, Int)
unpack (Point a) = (fromIntegral $ a .&. pMask,
                       fromIntegral $ shift a (-pBits) .&. pMask)

add :: Point -> Point -> Point
add (Point a) (Point b) = let s = a + b
                               in Point (s .&. sMask)

getBits :: Point -> Int -> (Bool, Bool)
getBits (Point a) shift = (testBit a shift, testBit a (pBits + shift))

