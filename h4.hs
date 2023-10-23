import Data.List

main :: IO ()
main =
    print ( filter tester generator )
    
special :: String -> Bool
special s 
    = not('0' `elem` s)
    && nodups s

nodups :: Eq a => [a] -> Bool
nodups s 
    = s == nub s
    
generator :: [( String , String , String , String )]
generator
    = [(s1,s2,s3,s4)
    | s1 <- map show [100 .. 999]
    , special s1
    , s2 <- permutations s1
    , s3 <- permutations s1
    , s4 <- permutations s1
    , nodups [s1,s2,s3,s4]
    ]
    


perfectSquare :: Int -> Bool
perfectSquare n
    = root 1
        where
        root r
            | n < r * r = False
            | n > r * r = root(r+1)
            | otherwise = True

prime :: Int -> Bool
prime 
    = not . factorisable 2

factorisable :: Int -> Int -> Bool
factorisable f n
    | f*f <= n=n `mod` f==0 || factorisable (f+1)n
    | otherwise = False
    
tester :: (String,String,String,String) -> Bool
tester (s1,s2,s3,s4)
    = perfectSquare i1
    && prime(i1+i2+i3+i4)
    && length (filter perfectSquare is) > 0
    
    where
        i1 = read s1
        i2 = read s2
        i3 = read s3
        i4 = read s4
        is = map read (permutations s1 \\ [s1,s2,s3,s4])