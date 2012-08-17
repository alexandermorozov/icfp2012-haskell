import Control.Monad (forever)
import Data.Lens.Lazy ((^$), (^.), (^=), (^%=))
import Data.List (foldl')
import System.IO
import System.Environment (getArgs)
import System.CPUTime
import Text.Printf (printf)
import World

limitedDepth :: Int -> World -> [(Int, [Command])]
limitedDepth depth w = (0,[]) : helper depth w []
  where
    helper 0 _ _ = []
    helper depth w rpath =  
        let cmds    = possibleCommands w
            ws      = map (step w) cmds
            scores  = map score ws
            rpaths  = map (:rpath) cmds
            future  = zipWith (helper (depth-1)) ws rpaths
        in (zip scores rpaths) ++ concat future
         

printVerbose :: [(Int, [Command])] -> IO ()
printVerbose xs = do
    t0 <- getCPUTime
    helper t0 xs (-1) (0::Int)
  where helper t0 [] _ n = do
            t1 <- getCPUTime
            let ops = (fromIntegral n / (fromIntegral $ t1-t0) * 10^12)::Double 
            printf "%.0f Op/s, %d Ops" ops n
        helper t0 ((c', p'):xs) c n =
            if c' > c
                then putStrLn (show c' ++ " " ++ (map commandToChar $ reverse p')) >>
                     (helper t0 xs c' $! (n+1))
                else helper t0 xs c $! (n+1)

main = do
    [mapFile] <- getArgs
    fData <- readFile mapFile
    let s = parseWorld fData
    printVerbose $ limitedDepth 10 s
    --print $ limitedDepth 6 s


