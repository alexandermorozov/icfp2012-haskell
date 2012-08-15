


{-# LANGUAGE TemplateHaskell #-}
module World (World, emptyWorld, parseWorld, drawWorld,
              turn, ending, razors,
              step,
              Command (..)
             ) where

import Control.Arrow (second)
import Control.Monad (liftM)
import Control.Monad.State (State, execState, modify, get, gets)
import Data.List (groupBy, span, foldl')
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

step c = execState (halfStep c >> update)

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
  where
    shaveArea = [Vector dx dy | dx <- [-1,0,1], dy <- [-1,0,1], dx /= 0 || dy /= 0]
    shavePoint :: Point -> State World Bool
    shavePoint p = do c <- gets $ getCell p
                      if (c == Beard)
                         then modify (setCell p Empty) >> return True
                         else return False
    shave = do
        rz <- gets (razors ^$)
        if rz > 0
           then do
               r <- getRobot
               changed <- mapM (\v -> shavePoint (shiftPoint r v)) shaveArea
               modify $ (razors ^%= (flip (-) 1))
               return $ or changed
           else
               return False

update :: State World ()
update = do
    s0 <- get
    setsMap <- gets $ (sets ^$)
    let setsToCheck = map ((M.!) setsMap) [Rock, HoRock, Beard, CLift]
    let toCheck = foldl' S.union S.empty $ setsToCheck
    mapM_ updateCell (S.toAscList toCheck)
  where
    updateCell p = do
        c <- gets $ getCell p
        case c of
          Rock   -> updateRock p c
          HoRock -> updateRock p c
          Beard  -> updateBeard p
          CLift  -> updateCLift p
        return ()
    updateRock p c = do
        let rel dx dy = gets $ getCell (shiftPoint p $ Vector dx dy)
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
              | True                                        -> return ()
      where isRock c = c == Rock || c == HoRock
            --isRock = (||) <$> (== Rock) <*> (== HoRock)
            breakRock HoRock p' = do
                c <- gets $ getCell (shiftPoint p' $ Vector 0 (-1))
                return $ if c == Empty then HoRock else Lambda
            breakRock Rock p' = return Rock

            moveRock dx dy = let p'  = shiftPoint p  $ Vector dx dy
                             in do c' <- breakRock c p'
                                   modify (setCell p' c' . setCell p Empty)
    updateBeard p = return ()
    updateCLift p = return ()

shiftPoint :: Point -> Vector -> Point
shiftPoint (Point x y) (Vector dx dy) = Point (x + dx) (y + dy)

move :: Int -> Int -> State World Bool
move dx dy = do
    r <- getRobot
    let r' = shiftPoint r $ Vector dx dy
    c' <- gets $ getCell r'
    case c' of
        Empty  -> moveBot r r'
        Earth  -> moveBot r r'
        Lambda -> modify (lambdas ^%= (+1)) >> moveBot r r'
        Razor  -> modify (razors  ^%= (+1)) >> moveBot r r'
        OLift  -> modify (ending ^= (Just Win)) >> moveBot r r'
        TrEntry -> teleport r r'
        Rock   -> moveRock r r' dx c'
        HoRock -> moveRock r r' dx c'
        otherwise -> return False
    --if c' `elem` [Earth, Empty, Lambda, OLift, TrampEntry, Razor]
    --    then][]]
  where moveBot :: Point -> Point -> State World Bool
        moveBot r r' = modify (setCell r' Robot . setCell r Empty) >> return True
        moveRock r r' dx c'= do
            let r'' = shiftPoint r' $ Vector dx 0
            c'' <- gets $ getCell r''
            if dx /= 0 && c'' == Empty
               then modify (setCell r'' c' . setCell r' Robot . setCell r Empty) >>
                    return True
               else return False
        teleport r r' = do
            exitP <- gets $ flip (M.!) r' . (trampForward ^$)
            entryPs <- gets $ flip (M.!) exitP . (trampBackward ^$)
            mapM_ (\p -> modify $ setCell p Empty) entryPs
            moveBot r exitP


getRobot :: State World Point
getRobot = do
    Just s <- gets $ M.lookup Robot . (sets ^$)
    return $ head $ S.toList s

getCell :: Point -> World -> Cell
getCell p = M.findWithDefault Unknown p . (field ^$)

setCell :: Point -> Cell -> World -> World
setCell p c' w = w {_field = f', _sets = s''}
  where f'  = M.insert p c' (w ^. field)
        c   = getCell p w
        s'  = M.adjust (S.delete p) c (w ^. sets)
        s'' = M.adjust (S.insert p) c' s'
