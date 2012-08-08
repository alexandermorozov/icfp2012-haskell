import World

main = do
    fData <- getContents
    let s = parseWorld fData
    print s
    putStrLn $ unlines $ drawWorld s
    return 0


