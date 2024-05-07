-- 2024/05/02

-- | Return the kth item from the end of a list 
atFromEnd :: Int -> [a] -> Maybe a
atFromEnd i ls | i < 0 || i > length ls = Nothing
               | otherwise = Just $ head $ drop (i - 1) $ reverse ls

-- head . drop (i - 1) . reverse
-- Just $ ls !! (length ls - 1)
