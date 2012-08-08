

{-# LANGUAGE TemplateHaskell #-}
module World (World, emptyWorld, parseWorld, drawWorld) where

import Control.Arrow (second)
import Control.Monad (liftM)
import Control.Monad.State (State, execState, modify, get)
import Data.List (groupBy, span)
import Data.Lens.Lazy ((^$), (^.), (^=), (^%=))
import Data.Lens.Template (makeLenses)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tuple (swap)
import Debug.Trace (trace)
import System.IO (stdin, Handle, hGetContents)

data Cell = Empty   | Earth  | Rock    | HoRock | Wall  | Robot | OLift | CLift
          | TrEntry | TrExit | Unknown | Beard  | Razor | Lambda
          deriving (Eq, Ord, Show)

data Point = Point Int Int deriving (Eq, Show)

instance Ord Point where
    compare (Point x1 y1) (Point x2 y2) = compare (y1, x1) (y2, x2)

data World = World { _field         :: M.Map Point Cell
                   , _sets          :: M.Map Cell Point
                   , _trampForward  :: M.Map Point Point
                   , _trampBackward :: M.Map Point [Point]
                   , _flooding      :: Int
                   , _water         :: Int
                   , _waterproof    :: Int
                   , _growth        :: Int
                   , _razors        :: Int
                   , _turn          :: Int
                   } deriving (Show)

$(makeLenses [''World])

emptyWorld :: World
emptyWorld =
    World { _field = M.empty
          , _sets = M.empty
          , _trampForward = M.empty
          , _trampBackward = M.empty
          , _flooding = 0
          , _water = -1
          , _waterproof = 10
          , _growth = 0
          , _razors = 0
          , _turn = 0
          }

parseWorld :: String -> World
parseWorld rawData = (flip execState) emptyWorld $ do
        let fw = getForwardTramp fLines trampPairs
        modify $ \w -> (foldr setVar w vars)
        modify $ field ^= (parseField fLines)
        modify $ trampForward ^= fw
        modify $ trampBackward ^= compileBackwardTramp fw
  where
    (fLines, vars, trampPairs) = splitConf rawData
    splitConf cdata = (fieldLines, vars, tramps)
        where ls = lines cdata
              (fieldLines, rest) = break (== "") ls
              extras = map (break (== ' ')) (tail rest)
              (varLines, trampLines) = span ((/= "Trampoline") . fst) extras
              vars = map (second read) varLines :: [(String, Int)]
              tramps = map (parseTramp . snd) trampLines
    parseTramp l = let parts = words l
                   in (head $ parts !! 0, head $ parts !! 2)

    optionList = [ ("Flooding",   flooding)
                 , ("Water",      water)
                 , ("Waterproof", waterproof)
                 , ("Growth",     growth)
                 , ("Razors",     razors)
                 ]
    setVar (k, v) = case lookup k optionList of
        Just setter -> setter ^= v
        Nothing     -> id
    cellList fLines = helper 1 1 fLines
        where helper x y [[]] = []
              helper x y ([]:lines) = helper 1 (y+1) lines
              helper x y ((c:rest):ls) = (Point x y, c):(helper (x+1) y (rest:ls))
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
             x | isTrampEntry(x) -> TrEntry
             x | isTrampExit(x)  -> TrExit
             otherwise -> Unknown
    isTrampEntry c = c >= 'A' && c <= 'I'
    isTrampExit  c = c >= '0' && c <= '9'

    getForwardTramp :: [String] -> [(Char, Char)] -> M.Map Point Point
    getForwardTramp fLines routes = forward
        where cells = cellList fLines
              isTramp = \x -> isTrampEntry x || isTrampExit x
              all = map swap $ filter (isTramp . snd) cells
              get a = case lookup a all of
                           Just x -> x
                           Nothing -> Point (-1) (-1)
              getMapping (a, b) = (get a, get b)
              forward = M.fromList $ map getMapping routes

    compileBackwardTramp :: M.Map Point Point -> M.Map Point [Point]
    compileBackwardTramp fw = foldr ins M.empty (M.toList fw)
        where ins (src, dst) = M.insertWith (++) dst [src]

drawWorld :: World -> [String]
drawWorld w = map (renderLine "" 1) $ grouped $ M.toAscList (w ^. field)
  where grouped = groupBy sameLine
        sameLine (Point _ y1, _) (Point _ y2, _) = y1 == y2
        renderLine s i [] = reverse s
        renderLine s i axs@((Point x _, c):xs) | x == i = renderLine (toChar c:s)
                                                                       (x+1) xs
                                               | True   = renderLine (' ':s) (i+1) axs
        toChar c = case c of
                        Empty -> ' '
                        Earth -> '.'
                        Rock -> '*'
                        HoRock -> '@'
                        Wall -> '#'
                        Robot -> 'R'
                        OLift -> 'O'
                        CLift -> 'L'
                        TrEntry -> 'T'
                        TrExit -> 't'
                        Unknown -> '?'
                        Beard -> 'W'
                        Razor -> '!'
                        Lambda -> '\\'
