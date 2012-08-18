import Control.Monad (forever)
import Data.Lens.Lazy ((^$), (^.), (^=), (^%=))
import Data.List (foldl')
import System.IO
import System.Environment (getArgs)
import World

main = do
    [mapFile, actions] <- getArgs
    fData <- readFile mapFile
    hSetBuffering stdin NoBuffering

    let s = parseWorld fData
    draw s
    loop s
  where
    draw s = do
        putStrLn $ "Turn " ++ show (s ^. turn) ++ 
                   " / Razors " ++ show (s ^. razors) ++
                   " / Ending " ++ show (s ^. ending) ++
                   " / Comms "  ++ show (possibleCommands s)
        putStrLn $ unlines $ drawWorld s
    loop s = do
        cmdChar <- getChar
        putChar '\r'
        if cmdChar `elem` "hjklaws"
           then let cmd = case cmdChar of
                      'h' -> CLeft
                      'j' -> CDown
                      'k' -> CUp
                      'l' -> CRight
                      'w' -> CWait
                      's' -> CShave
                      'a' -> CAbort
                    s' = step s cmd
                in draw s' >> print s' >> loop s'
           else loop s


