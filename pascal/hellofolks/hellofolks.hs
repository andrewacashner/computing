{- say hello to a list of people
-}

folks :: [String]
folks = ["Joseph"
        , "José"
        , "Josephine"
        , "Giuseppe"
        , "Iosephus"
        , "Josefina"
        , "Josephine"
        ]

sayhello :: String -> String
sayhello s = "Hello, " ++ s ++ "!"

greeting :: [String] -> String
greeting names = unlines $ map sayhello names

main :: IO()
main = do
    putStr $ greeting folks

