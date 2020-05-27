import System.IO        ( IOMode(WriteMode, ReadMode)
                        , openFile, hPutStrLn, hClose
                        , hGetLine, hIsEOF )
import System.Directory ( removeFile )

main :: IO ()
main =
  do
    h <- openFile "hello.txt" WriteMode

    hPutStrLn h "hello"
    hPutStrLn h "world"

    hClose h

    h <- openFile "hello.txt" ReadMode

    line <- hGetLine h
    putStrLn line

    atEnd <- hIsEOF h
    putStrLn $ show atEnd

    line <- hGetLine h
    putStrLn line

    atEnd <- hIsEOF h
    putStrLn $ show atEnd

    hClose h

    removeFile "hello.txt"
