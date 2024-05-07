import qualified Data.Map as Map
    (Map, null, empty, insertWith, filter)

type LetterDict = Map.Map Char Int

register :: LetterDict -> Int -> String -> LetterDict
register inventory increment s = 
    foldl (\dict c -> Map.insertWith (+) c increment dict) inventory s

isPermutation :: String -> String -> Bool
isPermutation s1 s2 = Map.empty == Map.filter (/= 0) comparison
     where
         comparison  = register inventoryS1 (-1) s2
         inventoryS1 = register Map.empty 1 s1

results :: [(String, String)] -> String
results testValues = unlines $ map (\(a, b) -> testMsg a b) testValues
    where 
        testMsg a b = a ++ " vs. " ++ b ++ ": " 
                        ++ (show $ isPermutation a b)

main :: IO()
main = do 
    let 
        testValues = [
                ("abba", "baab"),
                ("dad", "add"),
                ("bacon", "eggs"),
                ("shut", "tush")
            ]

    putStrLn $ results testValues




