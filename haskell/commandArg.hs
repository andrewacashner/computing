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
parse s = show $ checkArgCommands $ words s

startsWith s c  = head s == c
endsWith s c    = last s == c

isCommand s       = s `startsWith` '\\'
isQuoteArg s      = s `startsWith` '"' && s `endsWith` '"'
startsBraceExpr s = s `startsWith` '{'
endsBraceExpr s   = s `endsWith` '}'

isWord s = not $ isCommand s || isQuoteArg s || startsBraceExpr s || endsBraceExpr s

data Token = 
    Word {
        base :: String
    }
    |
    CommandSimple {
        base :: String,
        arg :: [String] 
    }
    |
    CommandNested {
        base :: String,
        commandArg :: Command
    }

isSimpleCommand x y = isCommand x && (not . isCommand) y
isNestedCommand x y = isCommand x && isCommand y


mapPairs :: (a -> a -> b) 
            -> b -- ^ default case for output
            -> [a] 
            -> [b]
mapPairs f d (x:[]) = [d]
mapPairs f d (x:y:ys) = (f x y):(mapPairs f d (y:ys))

checkCommands = mapPairs False isSimpleCommand
checkNestedCommands = mapPairs False isNestedCommand

makeCommand :: String -> String -> Command
makeCommand x y | isSimpleCommand x y = CommandSimple x y
                | isNestedCommand x y = CommandNested x y

makeCommands = mapPairs makeCommand 


