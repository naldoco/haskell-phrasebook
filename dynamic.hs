import Data.Dynamic  ( Dynamic, toDyn, fromDynamic
                     , dynTypeRep )
import Data.Foldable ( for_, asum )

mixedList :: [Dynamic]
mixedList =
  [ toDyn True
  , toDyn (5 :: Integer)
  , toDyn "hey" ]

main :: IO ()
main =
  for_ mixedList $ \d ->
    putStrLn (message d)

recognizeType :: Dynamic -> Maybe String
recognizeType d =
  asum
    [ (fromDynamic d :: Maybe Integer) >>= \x ->
        Just $ show x
            ++ " is an integer"
    , (fromDynamic d :: Maybe Bool   ) >>= \x ->
        Just $ "The answer is "
            ++ if x
                 then "yes"
                 else "no"
    ]

message :: Dynamic -> String
message d =
  case (recognizeType d) of
    Just x  -> x
    Nothing -> "Unrecognized type: "
            ++ show (dynTypeRep d)
