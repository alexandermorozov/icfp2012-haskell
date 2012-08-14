import Control.Monad (forever)
import Data.Lens.Lazy ((^$), (^.), (^=), (^%=))
import Data.List (foldl')
import System.IO
import System.Environment (getArgs)
import World

main = do
    [mapFile, actions] <- getArgs
    fData <- readFile mapFile
    let s = parseWorld fData
    draw s
    let s' = foldl' (flip step) s $ take 1000000 $ cycle [CUp, CDown]
    draw s'

    hSetBuffering stdin NoBuffering

    --loop s
  where
    draw s = do
        putStrLn $ "Turn " ++ show (s ^. turn) ++ 
                   " / Razors " ++ show (s ^. razors) ++
                   " / Ending " ++ show (s ^. ending)
        putStrLn $ unlines $ drawWorld s
    loop s = do
        cmdChar <- hGetChar stdin
        hPutChar stdout '\r'
        if cmdChar `elem` "hjklaws"
           then let cmd = case cmdChar of
                      'h' -> CLeft
                      'j' -> CDown
                      'k' -> CUp
                      'l' -> CRight
                      'w' -> CWait
                      's' -> CShave
                      'a' -> CAbort
                    s' = step cmd s 
                in draw s' >> loop s'
           else loop s


