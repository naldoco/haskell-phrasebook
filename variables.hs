main :: IO ()
main = do
  let
    x :: Integer
    x = 2
  putStrLn
    . show
    $ x + x

  let
    (b, c) = ("one", "two") :: (String, String)
  putStrLn b
  putStrLn c

  let
    d :: Bool
    d = True
    e :: [Integer]
    e = [1,2,3]
  putStrLn $ show d
  putStrLn $ show e
