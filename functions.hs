next :: (Num a) => a -> a
next x =
  x + 1

hypotenuse :: (Floating a) => a -> a -> a
hypotenuse x y =
  sqrt $ x^2
       + y^2

greet :: String -> String
greet name =
     "hello"
  ++ " "
  ++ name

greet2 :: String -> String
greet2 name =
     "hello"
  ++ " "
  ++ name

greetNext :: (Num a, Show a) => a -> (a, String)
greetNext x =
  ( next x
  , greet $ show $ next x )

hello :: String -> String
hello "Olafur"     = "hello, Olafur!"
hello "Rocamadour" = "hey!"
hello x            = greet x

main = do
  putStrLn $ show $        next 4
  putStrLn $ show $ next $ next 4

  putStrLn $ show $ hypotenuse 3 4

  putStrLn $ greet  "world"
  putStrLn $ greet2 "world"

  putStrLn $ show $ greetNext 7

  let
    (x, y) = greetNext 7 :: (Integer, String)
  putStrLn $ show x
  putStrLn y

  putStrLn $ hello "Olafur"
  putStrLn $ hello "Rocamadour"
  putStrLn $ hello "Jane"
