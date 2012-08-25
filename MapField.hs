module MapField (Field, field, set, get, empty) where

import qualified Data.Map as M
import Debug.Trace

import Point as P

newtype Field a = Field (M.Map P.Point a) deriving (Show)

field :: a -> Int -> Int -> Field a
field a w h = Field $ M.fromList [(P.pack x y, a) | x <- [0..w], y <- [1..h]]

get :: Point -> Field a -> a
get p (Field m) = (M.!) m p

set :: Point -> a -> Field a -> Field a
set p a (Field m) = Field $ M.insert p a m

empty :: Field a
empty = Field M.empty

