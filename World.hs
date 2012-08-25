{-# LANGUAGE TemplateHaskell #-}
module World ( World, emptyWorld, parseWorld, drawWorld
             , turn, ending, razors, fieldHash, rockFell
             , step, possibleCommands, score
             , commandToChar, cellToChar
             , Command (..)
             ) where

import Control.Arrow (second)
import Control.Monad (liftM, when)
import Control.Monad.State (State, execState, evalState, modify, get, gets)
import Data.Bits ((.&.), shift, complement, xor)
import Data.Char (isDigit)
import Data.Digest.Murmur64 (asWord64, hash64Add, hash64)
import Data.List (groupBy, span, foldl')
import Data.Lens.Lazy ((^$), (^.), (^=), (^%=))
import Data.Lens.Template (makeLenses)
import qualified Data.Map as M
import Data.Maybe (isNothing, isJust, fromJust, fromMaybe)
import qualified Data.Set as S
import Data.Tuple (swap)
import Data.Word (Word64)
import Debug.Trace (trace)
import System.IO (stdin, Handle, hGetContents)

import qualified Field as F
--import qualified MapField as F
import qualified Point as P

data Cell = Empty   | Earth  | Rock    | HoRock | Wall  | Robot | OLift | CLift
          | TrEntry | TrExit | Unknown | Beard  | Razor | Lambda | Undefined
          deriving (Eq, Ord, Show)

data Ending = Win | Abort | Fail deriving (Show)

data World = World { _field         :: F.Field Cell
                   , _fieldHash     :: !Word64
                   , _dimensions    :: (Int, Int)
                   , _sets          :: M.Map Cell (S.Set P.Point)
                   , _maybeFalling  :: S.Set P.Point -- stones that may fall on update
                   , _rockFell      :: Bool -- a stone fell in prev turn
                   , _trampForward  :: M.Map P.Point P.Point
                   , _trampBackward :: M.Map P.Point [P.Point]
                   , _flooding      :: !Int
                   , _water         :: !Int
                   , _waterproof    :: !Int
                   , _growth        :: !Int
                   , _razors        :: !Int
                   , _turn          :: !Int
                   , _underwater    :: !Int -- number of steps underwater
                   , _lambdas       :: !Int
                   , _ending        :: Maybe Ending
                   } deriving (Show)

data Command = CLeft | CRight | CUp | CDown | CWait | CShave | CAbort deriving (Show)

cachedCellTypes = [Rock, HoRock, Robot, OLift, CLift, Beard, Razor, Lambda]

$(makeLenses [''World])


emptyWorld :: World
emptyWorld =
    World { _field = F.empty
          , _fieldHash = 0
          , _dimensions = (0, 0)
          , _sets = M.fromList $ map (\x -> (x, S.empty)) cachedCellTypes
          , _maybeFalling = S.empty
          , _rockFell = True        -- really unknown
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
            width  = maximum $ map length fLines
            height = length fLines
        modify $ dimensions ^= (width, height)
        modify $ field ^= F.field Undefined width height
        modify $ \w -> foldr setVar w vars
        modify $ parseField fLines
        modify $ trampForward ^= fw
        modify $ trampBackward ^= compileBackwardTramp fw
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
              helper x y ((c:rest):ls) = (P.pack x y, c) : helper (x+1) y (rest:ls)
    parseField fLines w = foldl' (\w' (p,c) -> setCell p c w') w $ map (second toCell) (cellList fLines)
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

    getForwardTramp :: [String] -> [(Char, Char)] -> M.Map P.Point P.Point
    getForwardTramp fLines routes = forward
        where cells = cellList fLines
              isTramp x = isTrampEntry x || isTrampExit x
              all = map swap $ filter (isTramp . snd) cells
              get a = fromMaybe (P.pack (-1) (-1)) (lookup a all)
              getMapping (a, b) = (get a, get b)
              forward = M.fromList $ map getMapping routes

    compileBackwardTramp :: M.Map P.Point P.Point -> M.Map P.Point [P.Point]
    compileBackwardTramp fw = foldr ins M.empty (M.toList fw)
        where ins (src, dst) = M.insertWith (++) dst [src]

drawWorld :: World -> [String]
drawWorld w = map line ys
  where
    (xm,ym) = w ^. dimensions
    xs = [1..xm]
    ys = [1..ym]
    cells y = map (\x -> getCell (P.pack x y) w) xs
    line y = map cellToChar $ filter (/= Undefined) (cells y)

cellToChar :: Cell -> Char
cellToChar c = case c of
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
                Undefined -> '-'
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

step :: World -> Command -> World
step w c = flip execState w $ do
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
    shavePoint :: P.Point -> State World Bool
    shavePoint p = do c <- getCellM p
                      if c == Beard
                         then setCellM p Empty >> return True
                         else return False
    shave = do
        rz <- gets (razors ^$)
        if rz > 0
           then do
               r <- gets getRobot
               changed <- mapM (shavePoint . P.add r) closeArea
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
        types = (if doGrow then (Beard:) else id) [CLift]
        mergedSets ts = foldl' S.union S.empty $ map ((M.!) setsMap) ts
        -- falling = S.intersection (s0 ^. maybeFalling) (mergedSets [Rock, HoRock])
        -- this way is faster:
        falling = s0 ^. maybeFalling
        toCheck = S.union falling (mergedSets types)
        robotY = snd . P.unpack . getRobot $ s0
        isUnderWater = waterLevel s0 >= robotY
        isUnderWater' = waterLevel s0' >= robotY
    modify $ (maybeFalling ^= S.empty)
    if isUnderWater || isUnderWater'
       then modify $ underwater ^%= (+1)
       else modify $ underwater ^= 0
    nUnder <- gets (underwater ^$)
    when (nUnder > (s0 ^. waterproof)) (endM Fail)
    modify $ rockFell ^= False
    mapM_ (updateCell s0) (S.toAscList toCheck)
  where
    updateCell s0 p = do
        let c = getCell p s0
        case c of
          Rock   -> updateRock p c
          HoRock -> updateRock p c
          Beard  -> updateBeard p
          CLift  -> updateCLift p
          otherwise -> return () -- stones from some points may be already moved out
    updateRock p c = do
        s <- get
        let rel dx dy = getCell (P.add p $ P.pack dx dy) s
            cR  = rel   1    0
            cL  = rel (-1)   0
            cD  = rel   0  (-1)
            cDL = rel (-1) (-1)
            cDR = rel   1  (-1)
        case True of
            _ | cD == Empty                                 -> moveRock   0  (-1)
              | isRock cD    && cR == Empty && cDR == Empty -> moveRock   1  (-1)
              | isRock cD    && cL == Empty && cDL == Empty -> moveRock (-1) (-1)
              | cD == Lambda && cR == Empty && cDR == Empty -> moveRock   1  (-1)
              | otherwise                                   -> return ()
      where isRock c = c == Rock || c == HoRock
            breakRock HoRock p' = do
                c <- getCellM (P.add p' $ P.pack 0 (-1))
                return $ if c == Empty then HoRock else Lambda
            breakRock Rock p' = return Rock
            moveRock dx dy = let p'  = P.add p  $ P.pack dx dy
                                 p'' = P.add p' $ P.pack 0 (-1)
                             in do c'  <- breakRock c p'
                                   c'' <- getCellM p''
                                   setCellM p Empty
                                   setCellM p' c'
                                   modify $ rockFell ^= True
                                   when (c'' == Robot) (endM Fail)
    updateBeard p = mapM_ (growBeard . P.add p) closeArea
    growBeard p = do c <- getCellM p
                     when (c == Empty) $ setCellM p Beard

    updateCLift p = do
        m <- gets (sets ^$)
        let nl c = S.null $ (M.!) m c
        when (nl Lambda && nl HoRock && not (nl CLift)) $
           let p = head $ S.toList $ (M.!) m CLift
           in setCellM p OLift


-- Returns sensible moves to try in the next turn.
-- If no stone fell at prev turn and robot just waits, then nothing
--   good is expected to happen. This kind of prunning should reduce
--   branching factor in most cases by 1.
-- TODO: maybe change ordering
possibleCommands :: World -> [Command]
possibleCommands w =
    if isJust (w ^. ending)
       then []
       else let cmds = filter (\c -> evalState (halfStep c) w)
                           [CShave, CDown, CUp, CLeft, CRight]
            in if (w ^. rockFell)
                  then cmds ++ [CWait]
                  else cmds

waterLevel :: World -> Int
waterLevel w =
    if w ^. flooding > 0
       then (w ^. water) + (w ^. turn) `div` (w ^. flooding)
       else  w ^. water

closeArea = [P.pack dx dy | dx <- [-1,0,1], dy <- [-1,0,1], dx /= 0 || dy /= 0]


move :: Int -> Int -> State World Bool
move dx dy = do
    r <- gets getRobot
    let r' = P.add r $ P.pack dx dy
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
  where moveBot :: P.Point -> P.Point -> State World Bool
        moveBot r r' = setCellM r Empty >> setCellM r' Robot >> return True
        moveRock r r' dx c'= do
            let r'' = P.add r' $ P.pack dx 0
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


getRobot :: World -> P.Point
getRobot w = head $ S.toList $ fromMaybe (S.fromList [P.pack 0 0]) $ M.lookup Robot (w ^. sets)

getCell :: P.Point -> World -> Cell
getCell p w =
  let (x,y) = P.unpack p
      (xm,ym) = w ^. dimensions
  in if x > xm || x <= 0 || y > ym || y <= 0
        then Undefined
        else F.get p (w ^. field)

setCell :: P.Point -> Cell -> World -> World
setCell p c' w = w { _field = f'
                   , _fieldHash = hash'
                   , _sets = s''
                   , _maybeFalling = mFalling
                   }
  where f'  = F.set p c' (w ^. field)
        c   = getCell p w
        s'  = M.adjust (S.delete p) c (w ^. sets)
        s'' = M.adjust (S.insert p) c' s'
        -- hash' = (w ^. fieldHash) `xor` (cellHash p c) `xor` (cellHash p c')
        hash' = (w ^. fieldHash) - (cellHash p c) + (cellHash p c')
        packPs = map (P.add p . uncurry P.pack)
        fallArea = case c of -- here may be some extra points
            _ | c' == Empty -> packPs [(-1,0),(-1,1),(0,1),(1,1),(1,0)]
              | c' == Rock || c' == HoRock -> packPs [(0,0)]
              | c' == Lambda               -> packPs [(0,1)]
              | otherwise                  -> []
        mFalling = foldl' (flip S.insert) (w ^. maybeFalling) fallArea

getCellM :: P.Point -> State World Cell
getCellM = gets . getCell

setCellM :: P.Point -> Cell -> State World ()
setCellM p c = modify $ setCell p c

cellHash :: P.Point -> Cell -> Word64
cellHash p c = asWord64 $ hash64Add p (hash64 $ cellToChar c)

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
