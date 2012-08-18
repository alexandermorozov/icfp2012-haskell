{-# LANGUAGE TemplateHaskell #-}
module World (World, emptyWorld, parseWorld, drawWorld,
              turn, ending, razors,
              step, possibleCommands, score,
              packPoint, addPoint, Point (..),
              Command (..)
             ) where

import Control.Arrow (second)
import Control.Monad (liftM, when)
import Control.Monad.State (State, execState, evalState, modify, get, gets)
import Data.Bits ((.&.), shift, complement)
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

type Cell = Char

newtype Point = Point Int deriving (Eq, Ord, Show)

data Ending = Win | Abort | Fail deriving (Show)

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

type Command = Char

cachedCellTypes = "*@ROCW!\\"

$(makeLenses [''World])


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
              helper x y ((c:rest):ls) = (packPoint x y, c) : helper (x+1) y (rest:ls)
    parseField fLines = M.fromList $ map (second toCell) (cellList fLines)
    toCell c = case c of
                 _ | c `elem` " *@#ROL\\W!." -> c
                   | isTrampEntry c          -> 'T'
                   | isTrampExit c           -> 't'
    isTrampEntry c = c >= 'A' && c <= 'I'
    isTrampExit = isDigit
    getForwardTramp :: [String] -> [(Char, Char)] -> M.Map Point Point
    getForwardTramp fLines routes = forward
        where cells = cellList fLines
              isTramp x = isTrampEntry x || isTrampExit x
              all = map swap $ filter (isTramp . snd) cells
              get a = fromMaybe (packPoint (-1) (-1)) (lookup a all)
              getMapping (a, b) = (get a, get b)
              forward = M.fromList $ map getMapping routes

    compileBackwardTramp :: M.Map Point Point -> M.Map Point [Point]
    compileBackwardTramp fw = foldr ins M.empty (M.toList fw)
        where ins (src, dst) = M.insertWith (++) dst [src]

recache :: World -> World
recache w = (sets ^= foldr add initial (M.toList $ w ^. field)) w
  where add (p, c) = M.adjust (S.insert p) c
        initial = foldr (`M.insert` S.empty) M.empty cachedCellTypes

drawWorld :: World -> [String]
drawWorld w = reverse $ map (renderLine "" 1) $ grouped $ M.toAscList (w ^. field)
  where grouped = groupBy sameLine
        sameLine (a, _) (b, _) = pointY a == pointY b
        renderLine s i [] = reverse s
        renderLine s i axs@((p, c):xs)
             | pointX p == i = renderLine (c:s) (pointX p + 1) xs
             | otherwise = renderLine (' ':s) (i+1) axs

step :: World -> Command -> World
step w c = flip execState w $ do
    notEnded <- gets $ isNothing . (ending ^$)
    when notEnded (halfStep c >> update)

-- returns True, if something useful happened: robot moved, beard was shaved
--         False, if moving failed or no beard was actually shaved
halfStep :: Command -> State World Bool
halfStep cmd = case cmd of
                      'L' -> move (-1)   0
                      'R' -> move   1    0
                      'U' -> move   0    1
                      'D' -> move   0  (-1)
                      'A' -> endM Abort >> return False
                      'W' -> return True
                      'S' -> shave
  where
    shavePoint :: Point -> State World Bool
    shavePoint p = do c <- getCellM p
                      if c == 'W'
                         then setCellM p ' ' >> return True
                         else return False
    shave = do
        rz <- gets (razors ^$)
        if rz > 0
           then do
               r <- gets getRobot
               changed <- mapM (shavePoint . addPoint r) closeArea
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
        types = (if doGrow then ('W':) else id) "C*@"
        setsToCheck = map ((M.!) setsMap) types
        robotY = pointY $ getRobot s0
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
          '*' -> updateRock p c
          '@' -> updateRock p c
          'W' -> updateBeard p
          'C' -> updateCLift p
        return ()
    updateRock p c = do
        s <- get
        let rel dx dy = getCell (addPoint p $ packPoint dx dy) s
            cR  = rel   1    0
            cL  = rel (-1)   0
            cD  = rel   0  (-1)
            cDL = rel (-1) (-1)
            cDR = rel   1  (-1)
        case True of
            _ | cD == ' '                                 -> moveRock   0  (-1)
              | isRock cD  && cR == ' ' && cDR == ' ' -> moveRock   1  (-1)
              | isRock cD  && cL == ' ' && cDL == ' ' -> moveRock (-1) (-1)
              | cD == '\\' && cR == ' ' && cDR == ' ' -> moveRock   1  (-1)
              | otherwise                                   -> return ()
      where isRock c = c `elem` "@*"
            breakRock '@' p' = do
                c <- getCellM (addPoint p' $ packPoint 0 (-1))
                return $ if c == ' ' then '@' else '\\'
            breakRock '*' p' = return '*'
            moveRock dx dy = let p'  = addPoint p  $ packPoint dx dy
                                 p'' = addPoint p' $ packPoint 0 (-1)
                             in do c'  <- breakRock c p'
                                   c'' <- getCellM p''
                                   setCellM p ' '
                                   setCellM p' c'
                                   when (c'' == 'R') (endM Fail)
    updateBeard p = mapM_ (growBeard . addPoint p) closeArea
    growBeard p = do c <- getCellM p
                     when (c == ' ') $ setCellM p 'W'

    updateCLift p = do
        m <- gets (sets ^$)
        let nl c = S.null $ (M.!) m c
        when (nl '\\' && nl '@' && not (nl 'C')) $
           let p = head $ S.toList $ (M.!) m 'C'
           in setCellM p 'O'

possibleCommands :: World -> [Command]
possibleCommands w = filter (\c -> evalState (halfStep c) w) "DULRSW"

waterLevel :: World -> Int
waterLevel w =
    if w ^. flooding > 0
       then (w ^. water) + (w ^. turn) `div` (w ^. flooding)
       else  w ^. water

closeArea = [packPoint dx dy | dx <- [-1,0,1], dy <- [-1,0,1], dx /= 0 || dy /= 0]


move :: Int -> Int -> State World Bool
move dx dy = do
    r <- gets getRobot
    let r' = addPoint r $ packPoint dx dy
    c' <- getCellM r'
    case c' of
        ' '  -> moveBot r r'
        '.'  -> moveBot r r'
        '\\' -> modify (lambdas ^%= (+1)) >> moveBot r r'
        '!'  -> modify (razors  ^%= (+1)) >> moveBot r r'
        'O'  -> modify (ending ^= Just Win) >> moveBot r r'
        'T' -> teleport r r'
        '*'   -> moveRock r r' dx c'
        '@' -> moveRock r r' dx c'
        otherwise -> return False
    --if c' `elem` [Earth, Empty, Lambda, OLift, TrampEntry, Razor]
    --    then][]]
  where moveBot :: Point -> Point -> State World Bool
        moveBot r r' = setCellM r ' ' >> setCellM r' 'R' >> return True
        moveRock r r' dx c'= do
            let r'' = addPoint r' $ packPoint dx 0
            c'' <- getCellM r''
            if dx /= 0 && c'' == ' '
               then setCellM r ' ' >> setCellM r' 'R' >> setCellM r'' c' >>
                    return True
               else return False
        teleport r r' = do
            exitP <- gets $ flip (M.!) r' . (trampForward ^$)
            entryPs <- gets $ flip (M.!) exitP . (trampBackward ^$)
            mapM_ (`setCellM` ' ') entryPs
            moveBot r exitP


getRobot :: World -> Point
getRobot w = head $ S.toList $ fromJust $ M.lookup 'R' (w ^. sets)

getCell :: Point -> World -> Cell
getCell p = M.findWithDefault '?' p . (field ^$)

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

score :: World -> Int
score w =
    let k = case ending ^$ w of
                Just Fail  -> 25
                Nothing    -> 50
                Just Abort -> 50
                Just Win   -> 75
    in (w ^. lambdas) * k - (w ^. turn)
