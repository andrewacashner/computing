import Data.List
    (intercalate)

data Name = Name {
    firstname :: String,
    lastname :: String
} deriving (Eq, Ord)

instance Show Name where
    show n = unwords [firstname n, lastname n]

main :: IO()
main = do
    let
        names = [ ("Harry", "Potter")
                , ("Hermione", "Granger")
                , ("Ron", "Weasley")
                , ("Draco", "Malfoy")
                , ("Seamus", "Finnegan")
                , ("Neville", "Longbottom")
                , ("Luna", "Lovegood")
                ]

        namelist = map (\(f, l) -> Name f l) names :: [Name]
        msg      = intercalate ", " $ map show namelist

    putStrLn(msg)

