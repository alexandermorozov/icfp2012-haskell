{-# LANGUAGE TemplateHaskell #-}
module World (World, emptyWorld, parseWorld, drawWorld,
              turn, ending, razors,
              step,
              Command (..)
             ) where

import Control.Arrow (second)
import Control.Monad (liftM, when)
import Control.Monad.State (State, execState, modify, get, gets)
import Data.Char (isDigit)
import Data.List (groupBy, span, foldl')
import Data.Lens.Lazy ((^$), (^.), (^=), (^%=))
import Data.Lens.Template (makeLenses)
import qualified Data.Map as M
import Data.Maybe (isNothing, fromJust, fromMaybe)
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
                   , _underwater    :: Int -- number of steps underwater
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
          , _underwater = 0
          , _growth = 0
          , _razors = 0
          , _turn = 0
          , _lambdas = 0
          , _ending = Nothing
          }

parseWorld :: String -> World
parseWorld rawData = flip execState emptyWorld $ do
        let fw = getForwardTramp fLines trampPairs
        modify $ \w -> foldr setVar w vars
        modify $ field ^= parseField fLines
        modify $ trampForward ^= fw
        modify $ trampBackward ^= compileBackwardTramp fw
        modify   recache
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
                   in (head $ head parts, head $ parts !! 2)

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
              helper x y ((c:rest):ls) = (Point x y, c) : helper (x+1) y (rest:ls)
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
             x | isTrampEntry x -> TrEntry
             x | isTrampExit  x -> TrExit
             otherwise -> Unknown
    isTrampEntry c = c >= 'A' && c <= 'I'
    isTrampExit    = isDigit

    getForwardTramp :: [String] -> [(Char, Char)] -> M.Map Point Point
    getForwardTramp fLines routes = forward
        where cells = cellList fLines
              isTramp x = isTrampEntry x || isTrampExit x
              all = map swap $ filter (isTramp . snd) cells
              get a = fromMaybe (Point (-1) (-1)) (lookup a all)
              getMapping (a, b) = (get a, get b)
              forward = M.fromList $ map getMapping routes

    compileBackwardTramp :: M.Map Point Point -> M.Map Point [Point]
    compileBackwardTramp fw = foldr ins M.empty (M.toList fw)
        where ins (src, dst) = M.insertWith (++) dst [src]

recache :: World -> World
recache w = (sets ^= foldr add initial (M.toList $ w ^. field)) w
  where add (p, c) = M.adjust (S.insert p) c
        initial = foldr (`M.insert` S.empty) M.empty $
                                          S.toList cachedCellTypes

drawWorld :: World -> [String]
drawWorld w = reverse $ map (renderLine "" 1) $ grouped $ M.toAscList (w ^. field)
  where grouped = groupBy sameLine
        sameLine (Point _ y1, _) (Point _ y2, _) = y1 == y2
        renderLine s i [] = reverse s
        renderLine s i axs@((Point x _, c):xs)
             | x == i = renderLine (toChar c:s) (x+1) xs
             | otherwise = renderLine (' ':s) (i+1) axs
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

step c = execState $ do
    notEnded <- gets $ isNothing . (ending ^$)
    when notEnded (halfStep c >> update)

-- returns True, if something useful happened: robot moved, beard was shaved
--         False, if moving failed or no beard was actually shaved
halfStep :: Command -> State World Bool
halfStep cmd = case cmd of
                      CLeft  -> move (-1)   0
                      CRight -> move   1    0
                      CUp    -> move   0    1
                      CDown  -> move   0  (-1)
                      CAbort -> endM Abort >> return False
                      CWait  -> return True
                      CShave -> shave
  where
    shavePoint :: Point -> State World Bool
    shavePoint p = do c <- getCellM p
                      if c == Beard
                         then setCellM p Empty >> return True
                         else return False
    shave = do
        rz <- gets (razors ^$)
        if rz > 0
           then do
               r <- gets getRobot
               changed <- mapM (shavePoint . shiftPoint r) closeArea
               modify $ razors ^%= flip (-) 1
               return $ or changed
           else
               return False

update :: State World ()
update = do
    s0 <- get
    modify $ turn ^%= (+1)
    s0' <- get
    setsMap <- gets (sets ^$)
    let tn = s0' ^. turn
        bd = s0' ^. growth
        doGrow = bd > 0 && (tn `mod` bd == 0) && tn > 1
        types = (if doGrow then (Beard:) else id) [Rock, HoRock, CLift]
        setsToCheck = map ((M.!) setsMap) types
        Point _ robotY = getRobot s0
        isUnderWater = waterLevel s0 >= robotY
        isUnderWater' = waterLevel s0' >= robotY
    let toCheck = foldl' S.union S.empty setsToCheck
    if isUnderWater || isUnderWater'
       then modify $ underwater ^%= (+1)
       else modify $ underwater ^= 0
    nUnder <- gets (underwater ^$)
    when (nUnder > (s0 ^. waterproof)) (endM Fail)
    mapM_ updateCell (S.toAscList toCheck)
  where
    updateCell p = do
        c <- getCellM p
        case c of
          Rock   -> updateRock p c
          HoRock -> updateRock p c
          Beard  -> updateBeard p
          CLift  -> updateCLift p
        return ()
    updateRock p c = do
        let rel dx dy = getCellM (shiftPoint p $ Vector dx dy)
        cR <-  rel   1    0
        cL <-  rel (-1)   0
        cD  <- rel   0  (-1)
        cDL <- rel (-1) (-1)
        cDR <- rel   1  (-1)
        case True of
            _ | cD == Empty                                 -> moveRock   0  (-1)
              | isRock cD    && cR == Empty && cDR == Empty -> moveRock   1  (-1)
              | isRock cD    && cL == Empty && cDL == Empty -> moveRock (-1) (-1)
              | cD == Lambda && cR == Empty && cDR == Empty -> moveRock   1  (-1)
              | otherwise                                   -> return ()
      where isRock c = c == Rock || c == HoRock
            --isRock = (||) <$> (== Rock) <*> (== HoRock)
            breakRock HoRock p' = do
                c <- getCellM (shiftPoint p' $ Vector 0 (-1))
                return $ if c == Empty then HoRock else Lambda
            breakRock Rock p' = return Rock
            moveRock dx dy = let p'  = shiftPoint p  $ Vector dx dy
                                 p'' = shiftPoint p' $ Vector 0 (-1)
                             in do c'  <- breakRock c p'
                                   c'' <- getCellM p''
                                   setCellM p Empty
                                   setCellM p' c'
                                   when (c'' == Robot) (endM Fail)
    updateBeard p = mapM_ (growBeard . shiftPoint p) closeArea
    growBeard p = do c <- getCellM p
                     when (c == Empty) $ setCellM p Beard

    updateCLift p = do
        m <- gets (sets ^$)
        let nl c = S.null $ (M.!) m c
        when (nl Lambda && nl HoRock && not (nl CLift)) $
           let p = head $ S.toList $ (M.!) m CLift
           in setCellM p OLift

waterLevel :: World -> Int
waterLevel w =
    if w ^. flooding > 0
       then (w ^. water) + (w ^. turn) `div` (w ^. flooding)
       else  w ^. water

closeArea = [Vector dx dy | dx <- [-1,0,1], dy <- [-1,0,1], dx /= 0 || dy /= 0]

shiftPoint :: Point -> Vector -> Point
shiftPoint (Point x y) (Vector dx dy) = Point (x + dx) (y + dy)

move :: Int -> Int -> State World Bool
move dx dy = do
    r <- gets getRobot
    let r' = shiftPoint r $ Vector dx dy
    c' <- getCellM r'
    case c' of
        Empty  -> moveBot r r'
        Earth  -> moveBot r r'
        Lambda -> modify (lambdas ^%= (+1)) >> moveBot r r'
        Razor  -> modify (razors  ^%= (+1)) >> moveBot r r'
        OLift  -> modify (ending ^= Just Win) >> moveBot r r'
        TrEntry -> teleport r r'
        Rock   -> moveRock r r' dx c'
        HoRock -> moveRock r r' dx c'
        otherwise -> return False
    --if c' `elem` [Earth, Empty, Lambda, OLift, TrampEntry, Razor]
    --    then][]]
  where moveBot :: Point -> Point -> State World Bool
        moveBot r r' = setCellM r Empty >> setCellM r' Robot >> return True
        moveRock r r' dx c'= do
            let r'' = shiftPoint r' $ Vector dx 0
            c'' <- getCellM r''
            if dx /= 0 && c'' == Empty
               then setCellM r Empty >> setCellM r' Robot >> setCellM r'' c' >>
                    return True
               else return False
        teleport r r' = do
            exitP <- gets $ flip (M.!) r' . (trampForward ^$)
            entryPs <- gets $ flip (M.!) exitP . (trampBackward ^$)
            mapM_ (`setCellM` Empty) entryPs
            moveBot r exitP


getRobot :: World -> Point
getRobot w = head $ S.toList $ fromJust $ M.lookup Robot (w ^. sets)

getCell :: Point -> World -> Cell
getCell p = M.findWithDefault Unknown p . (field ^$)

setCell :: Point -> Cell -> World -> World
setCell p c' w = w {_field = f', _sets = s''}
  where f'  = M.insert p c' (w ^. field)
        c   = getCell p w
        s'  = M.adjust (S.delete p) c (w ^. sets)
        s'' = M.adjust (S.insert p) c' s'

getCellM :: Point -> State World Cell
getCellM = gets . getCell

setCellM :: Point -> Cell -> State World ()
setCellM p c = modify $ setCell p c

endM :: Ending -> State World ()
endM e = modify (ending ^= Just e)

