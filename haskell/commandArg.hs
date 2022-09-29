{- CommandArg
Andrew Cashner
2022/09/29

Given a text file, parse it into words and commands with their arguments.

- A command begins with a backslash and is followed by one argument.
- The argument can be a quoted string, an expression enclosed in curly
  braces, or another command.
-}

main :: IO()
main = do
    inputStr <- getContents

    let 
        outputStr = parse inputStr

    putStrLn outputStr 

parse :: String -> String
parse s = show $ tokenize $ words s

startsWith s c  = head s == c
endsWith s c    = last s == c

isCommand s       = s `startsWith` '\\'
isQuoteArg s      = s `startsWith` '"' && s `endsWith` '"'
startsBraceExpr s = s `startsWith` '{'
endsBraceExpr s   = s `endsWith` '}'

isWord s = not $ isCommand s || isQuoteArg s || startsBraceExpr s || endsBraceExpr s

data Token = Word { base :: String }
            | Command { base :: String }
            | CommandArg {
                command :: String,
                arg :: Token
            } deriving Show

isSimpleCommand x y = isCommand x && (not . isCommand) y
isNestedCommand x y = isCommand x && isCommand y


mapPairs :: (a -> a -> b) 
            -> b -- ^ default case for output
            -> [a] 
            -> [b]
mapPairs f d (x:[]) = [d]
mapPairs f d (x:y:ys) = (f x y):(mapPairs f d (y:ys))

checkCommands = mapPairs isSimpleCommand False
checkNestedCommands = mapPairs isNestedCommand False

toToken :: String -> String -> Token
toToken x y | isCommand x = CommandArg x (Word y)
            | otherwise = Word x

-- TODO does not work properly! this is the wrong approach
tokenize :: [String] -> [Token]
tokenize (x:[]) = [toToken x ""]
tokenize (x:y:[]) | isCommand x = [toToken x y, toToken y ""]
                  | otherwise = [Word x, toToken y ""]
tokenize (x:y:ys) | isCommand x = (toToken x y):(tokenize (ys))
                  | otherwise = [Word x, toToken x y]



