import Control.Monad (forever)
import Data.Lens.Lazy ((^$), (^.), (^=), (^%=))
import Data.List (foldl', zipWith4)
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.PSQueue as PS
import Data.PSQueue (Binding((:->)))
import Data.Word (Word64)
import qualified Data.Dequeue as DQ
import qualified Data.Map as M
import Debug.Trace
import System.IO
import System.Environment (getArgs)
import System.CPUTime
import Text.Printf (printf)
import World

data Node = Node { nWorld :: World
                 , nRpath :: [Command]
                 , nScore :: Int
                 , nDepth :: Int
                 }

instance Eq Node where
    a == b = (fieldHash ^$ nWorld a) == (fieldHash ^$ nWorld b)

instance Ord Node where
    compare a b = compare (fieldHash ^$ nWorld a) (fieldHash ^$ nWorld b)

instance Show Node where
    show n = "{" ++ show (nScore n) ++ " " ++ (reverse $ map commandToChar $ nRpath n) ++ "}"


depthTreeSearch :: Int -> World -> [(Int, [Command])]
depthTreeSearch depth w = (0,[]) : helper depth w []
  where
    helper 0 _ _ = []
    helper depth w rpath =
        let cmds    = possibleCommands w
            ws      = map (step w) cmds
            scores  = map score ws
            rpaths  = map (:rpath) cmds
            future  = zipWith (helper (depth-1)) ws rpaths
        in zip scores rpaths ++ concat future


uninformedGraphSearch :: Int -> Bool -> World -> [(Int, [Command])]
uninformedGraphSearch depth inBreadth w =
  let n0 = Node w [] 0 0
  in (0,[]) : helper M.empty (DQ.fromList [n0]) depth
  where
    helper :: M.Map Word64 Int -> DQ.BankersDequeue Node -> Int -> [(Int, [Command])]
    helper seen toGo maxDepth | DQ.null toGo = []
                              | otherwise    =
        let (Just (Node w rpath sc depth), toGo') = DQ.popBack toGo
            cmds     = possibleCommands w
            ws       = map (step w) cmds
            scores   = map score ws
            rpaths   = map (:rpath) cmds
            children = zipWith4 Node ws rpaths scores (repeat $ depth + 1)
            futureNs = filter isGood children
            seen'    = foldl' addNode seen futureNs
            toGo''   = let push = if inBreadth then DQ.pushFront else DQ.pushBack
                       in foldl' push toGo' futureNs
        in zip scores rpaths ++ helper seen' toGo'' maxDepth
        where isGood (Node w' _ sc' depth') =
                  let h     = w' ^. fieldHash
                      found = M.lookup h seen
                  in depth' < maxDepth && fromMaybe True (found >>= Just . (< sc'))
              addNode smap (Node w' _ sc' _) = M.insert (w' ^. fieldHash) sc' smap

expand :: Node -> [Node]
expand (Node w rpath sc depth) = children
  where
    cmds     = possibleCommands w
    ws       = map (step w) cmds
    scores   = map score ws
    rpaths   = map (:rpath) cmds
    children = zipWith4 Node ws rpaths scores (repeat $ depth + 1)

astarLikeSearch :: (Node -> Int) -> World -> [(Int, [Command])]
astarLikeSearch costF w =
  let n0 = Node w [] 0 0
  in (0,[]) : helper M.empty (PS.fromList [bind n0])
  where
    bind :: Node -> Binding Node Int
    bind n = n :-> costF n
    helper :: M.Map Word64 Int -> PS.PSQ Node Int -> [(Int, [Command])]
    helper seen frontier | PS.null frontier = []
                         | otherwise        =
        let Just (b, frontier') = PS.minView frontier
            n          = PS.key b
            children   = filter isBetter (expand n)
            seen'      = foldr remember seen children
            frontier'' = foldr enqueue frontier' children
        in map toResult children ++ helper seen' frontier''
      where
        isBetter (Node w _ sc _) =
            let h     = w ^. fieldHash
                found = M.lookup h seen
            in fromMaybe True (found >>= Just . (< sc))
        remember (Node w _ sc _) = M.insert (w ^. fieldHash) sc
        toResult (Node w p sc _) = (score w,p)
        enqueue n = PS.insert n (costF n) . PS.delete n


simpleCost :: Node -> Int
simpleCost (Node w _ _ _) = (w ^. turn) - 50 * (w ^. lambdas)

printVerbose :: [(Int, [Command])] -> IO ()
printVerbose xs = do
    t0 <- getCPUTime
    helper t0 xs (-1) (0::Int)
  where helper t0 [] _ n = do
            t1 <- getCPUTime
            let ops = (fromIntegral n / fromIntegral (t1-t0) * 10^12)::Double
            printf "%.0f op/s, %d ops total" ops n
        helper t0 ((c', p'):xs) c n =
            if c' > c -- || True
                then do
                     t1 <- getCPUTime
                     printf "%d %s %.3f\n" c' (map commandToChar $ reverse p')
                                       (fromIntegral (t1-t0) / 10^12 ::Double)
                     helper t0 xs c' $! (n+1)
                else helper t0 xs c $! (n+1)

main = do
    [mapFile] <- getArgs
    fData <- readFile mapFile
    let s = parseWorld fData
    --printVerbose $ depthTreeSearch 12 s
    --printVerbose $ uninformedGraphSearch 18 True s
    printVerbose $ astarLikeSearch simpleCost s


