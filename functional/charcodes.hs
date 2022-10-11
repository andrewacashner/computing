import System.Environment (getArgs)

charCodes :: String -> [Int]
charCodes = map fromEnum

selectInput :: [String] -> IO String
selectInput []   = getLine
selectInput args = readFile $ head args

main :: IO ()
main = do
    args <- getArgs
    chars <- selectInput args
    putStrLn $ show $ charCodes chars
