main :: IO()
main = do
    codes <- map fromEnum <$> getLine
    putStrLn $ show codes

    -- or:
    --  inputStr <- getLine
    --  let
    --      codes = mapFromEnum inputStr
    --  putStrLn $ show codes

