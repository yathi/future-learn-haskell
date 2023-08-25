{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Main where
import ShowParser ( parseShow )

data PersonRecord = MkPersonRecord {
  name :: String,
  address :: Address,
  id :: Integer,
  labels :: [Label]
} deriving (Show)

data Address = MkAddress {
  line1 :: String,
  number :: Integer,
  street :: String,
  town :: String,
  postcode :: String
} deriving (Show)

data Label = Green | Red | Blue | Yellow deriving (Show)

rec1 = MkPersonRecord
  "Yathi Manavalan"
  (MkAddress "School of computing science" 17 "Lilybank Gdns" "Glasgow" "G12 8QQ")
  23432
  [Green, Red]

rec2 = MkPersonRecord
  "Jessika Jose"
  (MkAddress "School of computing science" 17 "Lilybank Gdns" "Glasgow" "G88 4QQ")
  42
  [Blue, Yellow]

rec_str = show [rec1, rec2]
main = putStrLn $ parseShow rec_str