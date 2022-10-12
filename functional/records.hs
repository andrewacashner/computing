{-# LANGUAGE DuplicateRecordFields #-}
-- OverloadedRecordDot

import Prelude

data Person = Person {
    name :: String,
    age :: Int,
    nationality :: String
} deriving (Eq, Ord, Show)

data Pet = Pet {
    name :: String,
    age :: Int,
    species :: String
} deriving (Eq, Ord, Show)

data HouseholdMember = HumanMember Person | AnimalMember Pet
    deriving (Eq, Ord, Show)

type Family = [HouseholdMember]

main :: IO()
main = do
    let
        aac     = Person "Andrew" 41 "USA"
        susan   = Pet "Susan" 3 "cat"
        cashners = [HumanMember aac, AnimalMember susan]

    putStrLn $ show cashners


