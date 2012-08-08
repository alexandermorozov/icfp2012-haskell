import World

main = do
    fData <- getContents
    let s = parseWorld fData
    putStrLn $ unlines $ drawWorld s
    return 0


