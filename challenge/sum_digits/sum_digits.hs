sumDigits :: Int
sumDigits = 1 + foldl (\acc n -> acc + 5 * (9 + 2 * n)) 0 [0..9]

main :: IO()
main = do
    putStrLn $ show sumDigits
