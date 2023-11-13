import Data.List

main :: IO ()

main = 
    print (x_generator2)
    --print(avg 8 6)
    --print(tester1 (16,59,27,4))
    --print (filter tester1 generator1)
    --print(tester ( " 123 " ," 21 " ," 123 " ," 12 " ," 123 " ))

    
generator2:: [(String, String, String, String, String)]
generator2 = [(n1, n2, n3, n4, n5)
    -- Generates numbers
    | s1 <- map show[123..987]
    , special s1
    , n1 <- permutations s1
    , n2 <- take 2 (permutations n1)
    , n3 <- permutations n1
    , n4 <- take 2 (permutations n1)
    , n5 <- permutations n1
    , noDups [n1, n2, n3, n4, n5]
    , first(n1)/=first(n2)
    ]
--toInt::(String,String,String,String,String) -> (Int,Int,Int,Int,Int)
--toInt (n1, n2, n3, n4, n5)
--    = 

first:: String -> Char
first (head:tail) = head

special:: String -> Bool
special s = not ('0' `elem` s) && noDups s

noDups :: Eq a => [a] -> Bool
noDups s 
    = s == nub s
    
x_generator2 :: Int
x_generator2 =
    length [ t | t <- ts , t `elem` g ]
    where
        g = generator2
        ts =
            [ ( " 123 " ," 21 " ," 123 " ," 12 " ," 123 " )
            , ( " 162 " ," 26 " ," 261 " ," 12 " ," 621 " )
            , ( " 219 " ," 19 " ," 912 " ," 21 " ," 291 " )
            , ( " 329 " ," 92 " ," 932 " ," 32 " ," 239 " )
            , ( " 439 " ," 94 " ," 394 " ," 43 " ," 394 " )
            , ( " 549 " ," 95 " ," 945 " ," 95 " ," 945 " )
            , ( " 568 " ," 68 " ," 586 " ," 56 " ," 586 " )
            , ( " 769 " ," 67 " ," 679 " ," 97 " ," 796 " )
            , ( " 879 " ," 79 " ," 897 " ," 98 " ," 789 " )
            , ( " 987 " ," 79 " ," 789 " ," 79 " ," 789 " )
            ]
