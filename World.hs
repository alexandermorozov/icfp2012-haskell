{-# LANGUAGE TemplateHaskell #-}
module World (World, emptyWorld, parseWorld) where

import Control.Arrow (second)
import Control.Monad (liftM)
import Data.List (span)
import Data.Lens.Lazy ((^$), (^.), (^=), (^%=))
import Data.Lens.Template (makeLenses)
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace (trace)
import System.IO (stdin, Handle, hGetContents)

data Cell = Empty   | Earth  | Rock    | HoRock | Wall  | Robot | OLift | CLift
          | TrEntry | TrExit | Unknown | Beard  | Razor | Lambda
          deriving (Eq, Ord, Show)

newtype Point = Point (Int, Int) deriving (Eq, Show)

instance Ord Point where
    compare (Point (x1, y1)) (Point (x2, y2)) = compare (y1, x1) (y2, x2)

data World = World { _field        :: M.Map Point Cell
                   , _sets         :: M.Map Cell Point
                   , _trampEntries :: M.Map Point Point
                   , _trampExits   :: M.Map Point Point
                   , _flooding     :: Int
                   , _water        :: Int
                   , _waterproof   :: Int
                   , _growth       :: Int
                   , _razors       :: Int
                   , _turn         :: Int
                   } deriving (Show)

$(makeLenses [''World])

emptyWorld :: World
emptyWorld =
    World { _field = M.empty
          , _sets = M.empty
          , _trampEntries = M.empty
          , _trampExits = M.empty
          , _flooding = 0
          , _water = -1
          , _waterproof = 10
          , _growth = 0
          , _razors = 0
          , _turn = 0
          }

parseWorld :: String -> World
parseWorld rawData =
    let (fLines, vars, trampPairs) = splitConf rawData
        s  = foldr setVar emptyWorld vars
        s' = field ^= (parseField fLines) $ s
    in s'

  where
    splitConf cdata = (fieldLines, vars, tramps)
        where ls = lines cdata
              (fieldLines, rest) = break (== "") ls
              extras = map (break (== ' ')) (tail rest)
              (varLines, trampLines) = span ((/= "Trampoline") . fst) extras
              vars = map (second read) varLines :: [(String, Int)]
              tramps = M.fromList $ map (parseTramp . snd) trampLines
    parseTramp l = let parts = words l
                   in (parts !! 0, parts !! 0)

    optionList = [ ("Flooding",   flooding)
                 , ("Water",      water)
                 , ("Waterproof", waterproof)
                 , ("Growth",     growth)
                 , ("Razors",     razors)
                 ]
    setVar (k, v) = case lookup k optionList of
        Just setter -> setter ^= v
        Nothing     -> id
    cellList fLines = helper 0 0 fLines
        where helper x y [[]] = []
              helper x y ([]:lines) = helper 0 (y+1) lines
              helper x y ((c:rest):ls) = (Point (x, y), c):(helper (x+1) y (rest:ls))
    parseField fLines = M.fromList $ map (second toCell) (cellList fLines)
    toCell c =
         case c of
             '.' -> Earth
             ' ' -> Empty
             '*' -> Rock
             '@' -> HoRock
             '#' -> Wall
             'R' -> Robot
             'O' -> OLift
             'L' -> CLift
             '\\'-> Lambda
             'W' -> Beard
             '!' -> Razor
             x | x >= 'A' && x <= 'I' -> TrEntry
             x | x >= '0' && x <= '9' -> TrExit
             otherwise -> Unknown

