test :: String -> String
test s
    | (a == 'c') = "true"
    | (b == 'd') = "true"
    | otherwise = "false"
    where
        (a,b) = ('a','b')
