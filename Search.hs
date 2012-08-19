import Control.Monad (forever)
import Data.Lens.Lazy ((^$), (^.), (^=), (^%=))
import Data.List (foldl', zipWith4)
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import qualified Data.Map as M
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


depthGraphSearch :: Int -> World -> [(Int, [Command])]
depthGraphSearch depth w =
  let n0 = Node w [] 0 0
  in (0,[]) : helper M.empty [n0] depth
  where
    helper :: M.Map Word64 Int -> [Node] -> Int -> [(Int, [Command])]
    helper _ [] _ = []
    helper seen (Node w rpath sc depth : toGo) maxDepth =
        let cmds     = possibleCommands w
            ws       = map (step w) cmds
            scores   = map score ws
            rpaths   = map (:rpath) cmds
            children = zipWith4 Node ws rpaths scores (repeat $ depth + 1)
            futureNs = filter isGood children
            seen'    = foldl' addNode seen futureNs
        in zip scores rpaths ++ helper seen' (futureNs ++ toGo) maxDepth
        where isGood (Node w' _ sc' depth') =
                  let h     = w' ^. fieldHash
                      found = M.lookup h seen
                  in depth' < maxDepth && fromMaybe True (found >>= Just . (< sc'))
              addNode smap (Node w' _ sc' _) = M.insert (w' ^. fieldHash) sc' smap



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
                then putStrLn (show c' ++ " " ++ map commandToChar (reverse p')) >>
                     (helper t0 xs c' $! (n+1))
                else helper t0 xs c $! (n+1)

main = do
    [mapFile] <- getArgs
    fData <- readFile mapFile
    let s = parseWorld fData
    --printVerbose $ depthTreeSearch 14 s
    printVerbose $ depthGraphSearch 20 s
    --print $ limitedDepth 6 s


