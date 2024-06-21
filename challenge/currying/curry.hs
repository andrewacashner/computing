import Text.Printf

plus n m = n + m

plusAny n = plus n

plusOne = plusAny 1

plusTwo = plus 2

printN = putStrLn . show

main :: IO()
main = do
    printN $ plusOne 2
    printN $ plusTwo 1

