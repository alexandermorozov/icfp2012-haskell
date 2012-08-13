


{-# LANGUAGE TemplateHaskell #-}
module World (World, emptyWorld, parseWorld, drawWorld,
              step,
              Command (..)
             ) where

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
data Vector = Vector Int Int deriving (Eq, Show)

data Ending = Win | Abort | Fail deriving (Show)

instance Ord Point where
    compare (Point x1 y1) (Point x2 y2) = compare (y1, x1) (y2, x2)

data World = World { _field         :: M.Map Point Cell
                   , _sets          :: M.Map Cell (S.Set Point)
                   , _trampForward  :: M.Map Point Point
                   , _trampBackward :: M.Map Point [Point]
                   , _flooding      :: Int
                   , _water         :: Int
                   , _waterproof    :: Int
                   , _growth        :: Int
                   , _razors        :: Int
                   , _turn          :: Int
                   , _lambdas       :: Int
                   , _ending        :: Maybe Ending
                   } deriving (Show)

data Command = CLeft | CRight | CUp | CDown | CWait | CShave | CAbort

cachedCellTypes = S.fromList [Rock, HoRock, Robot, OLift, CLift, Beard, Razor, Lambda]

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
          , _lambdas = 0
          , _ending = Nothing
          }

parseWorld :: String -> World
parseWorld rawData = (flip execState) emptyWorld $ do
        let fw = getForwardTramp fLines trampPairs
        modify $ \w -> (foldr setVar w vars)
        modify $ field ^= (parseField fLines)
        modify $ trampForward ^= fw
        modify $ trampBackward ^= compileBackwardTramp fw
        modify $ recache
  where
    (fLines, vars, trampPairs) = splitConf rawData
    splitConf cdata = (fieldLines, vars, tramps)
        where ls = lines cdata
              (fieldLines, rest) = break (== "") ls
              extras = if null rest then [] else map (break (== ' ')) (tail rest)
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
    cellList fLines = helper 1 1 (reverse fLines)
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

recache :: World -> World
recache w = (sets ^= (foldr add initial $ M.toList $ w ^. field) ) w
  where add (p, c) = M.adjust (S.insert p) c
        initial = foldr (\k -> M.insert k S.empty) M.empty $
                                          S.toList cachedCellTypes

drawWorld :: World -> [String]
drawWorld w = reverse $ map (renderLine "" 1) $ grouped $ M.toAscList (w ^. field)
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

charToCommand :: Char -> Command
charToCommand ch = case ch of
                        'L' -> CLeft
                        'R' -> CRight
                        'D' -> CDown
                        'U' -> CUp
                        'W' -> CWait
                        'S' -> CShave
                        'A' -> CAbort
                        otherwise -> undefined -- input should already be validated...

commandToChar :: Command -> Char
commandToChar c = case c of
                        CLeft  -> 'L'
                        CRight -> 'R'
                        CDown  -> 'D'
                        CUp    -> 'U'
                        CWait  -> 'W'
                        CShave -> 'S'
                        CAbort -> 'A'

step c = execState (halfStep c)

-- returns True, if something useful happened: robot moved, beard was shaved
--         False, if moving failed or no beard was actually shaved
halfStep :: Command -> State World Bool
halfStep cmd = case cmd of
                      CLeft  -> move (-1)   0
                      CRight -> move   1    0
                      CUp    -> move   0    1
                      CDown  -> move   0  (-1)
                      CAbort -> modify (ending ^= (Just Abort)) >> return False
                      CWait  -> return True
                      CShave -> shave

shiftPoint :: Point -> Vector -> Point
shiftPoint (Point x y) (Vector dx dy) = Point (x + dx) (y + dy)

move :: Int -> Int -> State World Bool
move dx dy = do
    r <- getRobot
    let r' = shiftPoint r $ Vector dx dy
    c' <- liftM (getCell r') get
    case c' of
        Empty  -> moveBot r r'
        Earth  -> moveBot r r'
        Lambda -> modify (lambdas ^%= (+1)) >> moveBot r r'
        Razor  -> modify (razors  ^%= (+1)) >> moveBot r r'
        OLift  -> modify (ending ^= (Just Win)) >> moveBot r r'
        TrEntry -> teleport r r'
        Rock   -> return False -- FIXME
        HoRock -> return False -- FIXME
        otherwise -> return False
    --if c' `elem` [Earth, Empty, Lambda, OLift, TrampEntry, Razor]
    --    then][]]
  where moveBot :: Point -> Point -> State World Bool
        moveBot r r' = modify (setCell r' Robot . setCell r Empty) >> return True
        teleport r r' = return True

shave = return True


getRobot :: State World Point
getRobot = do
    Just s <- liftM (M.lookup Robot . (sets ^$)) get
    return $ head $ S.toList s

getCell :: Point -> World -> Cell
getCell p = M.findWithDefault Unknown p . (field ^$)

setCell :: Point -> Cell -> World -> World
setCell p c' w = w {_field = f', _sets = s''}
  where f'  = M.insert p c' (w ^. field)
        c   = getCell p w
        s'  = M.adjust (S.delete p) c (w ^. sets)
        s'' = M.adjust (S.insert p) c' s'
