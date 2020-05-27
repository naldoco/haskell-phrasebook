{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE ScopedTypeVariables #-}

import GHC.Generics                     ( Generic )
import Generics.Deriving.Enum           ( GEnum (genum) )
import qualified Data.Map.Strict as Map ( Map, singleton
                                        , lookup )

data Product
  = Basic
  | Standard
  | Pro
  deriving stock   (Generic, Show)
  deriving anyclass GEnum

data Frequency
  = Monthly
  | Annual
  deriving stock   (Generic, Show)
  deriving anyclass GEnum

data Bill
  = Bill Product Frequency
  deriving stock   (Generic, Show)
  deriving anyclass GEnum

encodeProduct :: Product -> String
encodeProduct = \case
  Basic    -> "p1"
  Standard -> "p2"
  Pro      -> "p3"

encodeBill :: Bill -> Integer
encodeBill = \case
  Bill Basic    Monthly -> 10
  Bill Basic    Annual  -> 11
  Bill Standard Monthly -> 20
  Bill Standard Annual  -> 21
  Bill Pro      Monthly -> 30
  Bill Pro      Annual  -> 31

invert :: forall a b. (GEnum a, Ord b)
       =>(a -> b) -> b -> Maybe a
invert f =
  \b -> Map.lookup b reverseMap
  where
  reverseMap :: Map.Map b a
  reverseMap =
    foldMap (\a -> Map.singleton (f a) a) genum

decodeProduct :: String -> Maybe Product
decodeProduct = invert encodeProduct

decodeBill :: Integer -> Maybe Bill
decodeBill    = invert encodeBill

main :: IO ()
main = do
  putStrLn $        encodeProduct Basic
  putStrLn $        encodeProduct Standard

  putStrLn $ show $ decodeProduct "p1"
  putStrLn $ show $ decodeProduct "xyz"

  putStrLn $ show $ encodeBill $ Bill Basic Annual
  putStrLn $ show $ encodeBill $ Bill Pro Monthly

  putStrLn $ show $ decodeBill 31
  putStrLn $ show $ decodeBill 50
