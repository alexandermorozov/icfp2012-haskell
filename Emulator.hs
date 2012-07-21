import World

main = do
    fData <- getContents
    let s = parseWorld fData
    print s
    return 0


