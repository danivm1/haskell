doubleMe :: (Num a) => a -> a
doubleMe x = x + x

doubleUs :: (Num a) => a -> a -> a
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber :: (Ord a, Num a) => a -> a
doubleSmallNumber x =
  if x > 100
    then x
    else x * 2

listComprehensionMult :: [Integer]
listComprehensionMult =
  [x * 2 | x <- [0 .. 10], x * 2 >= 14]

listComprehensionMod :: [Integer]
listComprehensionMod =
  [x | x <- [50 .. 100], x `mod` 7 == 3]

listComprehensionOdd :: (Integral a) => [a] -> [String]
listComprehensionOdd xs =
  [if x < 10 then "Lesser than 10" else "10 or grater" | x <- xs, odd x]

listComprehensionPreds :: [Integer]
listComprehensionPreds =
  [x | x <- [10 .. 20], x /= 13, x /= 15, x /= 19]

listComprehensionProd :: [Integer]
listComprehensionProd =
  [x * y | x <- [2, 5, 10], y <- [8, 10, 11], x * y >= 50]

length' :: (Num a) => [t] -> a
length' x =
  sum [1 | _ <- x]

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st =
  [c | c <- st, c `elem` ['A' .. 'Z']]