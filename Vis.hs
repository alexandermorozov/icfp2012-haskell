import Control.Monad (forever)
import Data.Lens.Lazy ((^$), (^.), (^=), (^%=))
import Data.List (foldl')
import System.IO
import System.Environment (getArgs)
import Text.Printf

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
        printf "Turn %d, Score %d, Rz %d, End %s, RFall %s, Hash %016X, Comms %s\n"
                  (s ^. turn) (score s) (s ^. razors) (show $ s ^. ending)
                  (show $ s ^. rockFell)
                  (s ^. fieldHash) (map commandToChar $ possibleCommands s)
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
                in draw s' >> loop s'
           else loop s


