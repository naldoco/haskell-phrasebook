import Data.Foldable  ( fold ) -- foldr, foldMap
import Prelude hiding ( sum )

sum :: [Integer] -> Integer
sum xs =
  foldr (+) 0 xs

commaList :: [String] -> String
commaList xs =
  foldr commaSep "" xs
  where
    commaSep :: String -> String -> String
    commaSep x ""     = x
    commaSep x phrase = x
                     ++ ", "
                     ++ phrase

bulletList :: [String] -> String
bulletList xs =
  foldMap bulletItem xs
  where
    bulletItem :: String -> String
    bulletItem x = "  - "
                ++ x
                ++ "\n"

smashTogether :: [String] -> String
smashTogether = fold

main =
  do
    let
      numbers :: [Integer]
      numbers = enumFromTo 1 5
    putStrLn $ show       numbers
    putStrLn $ show $ sum numbers

    let
      words :: [String]
      words =
        ["One", "Two", "Three", "Four", "Five"]
    putStrLn $ commaList     words
    putStr   $ bulletList    words
    putStrLn $ smashTogether words
