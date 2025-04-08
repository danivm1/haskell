import           Data.Char                (isSpace)
import           Data.List                (foldl', foldl1', group, isSuffixOf,
                                           partition, scanl', sort, tails)
import           Distribution.Compat.Lens (_1)
import           GHC.Utils.Encoding       (zDecodeString)

doubleMe :: (Num a) => a -> a
doubleMe x = x + x

doubleUs :: (Num a) => a -> a -> a
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber :: (Ord a, Num a) => a -> a
doubleSmallNumber x =
  if x > 100
    then x
    else x * 2

listComprehensionMult :: [Int]
listComprehensionMult =
  [x * 2 | x <- [0 .. 10], x * 2 >= 14]

listComprehensionMod :: [Int]
listComprehensionMod =
  [x | x <- [50 .. 100], x `mod` 7 == 3]

listComprehensionOdd :: (Integral a) => [a] -> [String]
listComprehensionOdd xs =
  [if x < 10 then "Lesser than 10" else "10 or grater" | x <- xs, odd x]

listComprehensionPreds :: [Int]
listComprehensionPreds =
  [x | x <- [10 .. 20], x /= 13, x /= 15, x /= 19]

listComprehensionProd :: [Int]
listComprehensionProd =
  [x * y | x <- [2, 5, 10], y <- [8, 10, 11], x * y >= 50]

length' :: (Num a) => [t] -> a
length' x =
  sum [1 | _ <- x]

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st =
  [c | c <- st, c `elem` ['A' .. 'Z']]

zipList :: [(Int, Int)]
zipList =
  zip [1, 2, 3, 4, 5] [6, 6, 6, 6, 6]

zipListRange :: [(Int, Int)]
zipListRange =
  zip [1 .. 5] (replicate 5 6)

-- lista de triangulos retangulo com diametro = 24 e lados até tamanho 10
rightTriangle :: [(Int, Int, Int)]
rightTriangle =
  [(a, b, c) | c <- [2 .. 10], b <- [1 .. c - 1], a <- [1 .. b], a ^ 2 + b ^ 2 == c ^ 2, a + b + c == 24]

factorial :: (Eq a, Num a) => a -> a
factorial 0 = 1
factorial x = x * factorial (x - 1)

head' :: [a] -> a
head' []    = error "Empty list"
head' (x:_) = x

head'' :: [a] -> a
head'' xs = case xs of []    -> error "Empty list"
                       (x:_) -> x

head''' :: [a] -> a
head''' = foldl1' (\ _ x -> x)

length2' :: [a] -> Int
length2' []       = 0
length2' (_ : xs) = 1 + length2' xs

sum' :: [Int] -> Int
sum' []       = 0
sum' (x : xs) = x + sum' xs

sum'' :: [Int] -> Int
sum'' xs = foldl' (\acc x -> acc + x) 0 xs

sum''' :: [Int] -> Int
sum''' = foldl' (+) 0

capital :: String -> String
capital ""        = ""
capital all@(x:_) = "First letter of '" ++ all ++ "' is '" ++ [x] ++ "'"

imc :: Float -> Float -> String
imc h w
  | x < 18.5  = "Abaixo do peso"
  | x < 25    = "Adequado"
  | x < 30    = "Sobrepeso"
  | x < 35    = "Obeso I"
  | x < 40    = "Obeso II"
  | otherwise = "Obeso III"
  where x = w / h^2

imc' :: (RealFloat a) => [(a, a)] -> [a]
imc' xs = [x w h | (w, h) <- xs]
          where x w h = w / h^2

imc'' :: (RealFloat a) => [(a, a)] -> [a]
imc'' xs = [x | (w, h) <- xs, let x = w / h^2]


max' :: (Ord a) => a -> a -> a
max' a b | a > b = a | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
            | a > b     = GT
            | a == b    = EQ
            | otherwise = LT

initials :: String -> String -> String
initials fname sname =
  f ++ ". " ++ s ++ "."
  where f = take 1 fname
        s = take 1 sname

initials' :: String -> String -> String
initials' fname sname =
  [f] ++ ". " ++ [s] ++ "."
  where (f:_, s:_) = (fname, sname)

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea  = pi * r^2
    in  sideArea + 2 * topArea

describeList :: [a] -> String
describeList xs = "This list is " ++ case xs of []  -> "empty."
                                                [x] -> "a singleton list."
                                                xs  -> "a longer list."

maximum' :: (Ord a) => [a] -> a
maximum' []  = error "Empty list"
maximum' [x] = x
maximum' (x:xs)
       | x > maxTail = x
       | otherwise = maxTail
       where maxTail = maximum' xs

maximum'' :: (Ord a) => [a] -> a
maximum'' []     = error "Empty list"
maximum'' [x]    = x
maximum'' (x:xs) = max x (maximum'' xs)

maximum''' :: (Ord a) => [a] -> a
maximum''' = foldl1' (\acc x -> if x > acc then x else acc)

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

fibonacci' :: [Integer]
fibonacci' = 0 : 1 : zipWith (+) fibonacci' (tail fibonacci')

-- 0 : 1 : [1, 2, 3, ...] = [0, 1, 1, 2, 3, ...]
-- [0, 1, 1, 2, 3]
-- [1, 1, 2, 3]
-- [1, 2, 3, 5]

replicate' :: (Num a, Ord a) => a -> b -> [b]
replicate' n x
         | n <= 0    = []
         | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0   = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

reverse'' :: [a] -> [a]
reverse'' = foldl' (\acc x -> x : acc) []

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' _ []          = []
zip' [] _          = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

zip'' :: [a] -> [a] -> [(Maybe a, Maybe a)]
zip'' [] []         = []
zip'' (x:xs) []     = (Just x, Nothing) : zip'' xs []
zip'' [] (y:ys)     = (Nothing, Just y) : zip'' ys []
zip'' (x:xs) (y:ys) = (Just x, Just y)  : zip'' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
_ `elem'` [] = False
a `elem'` (x:xs)
    | a == x = True
    | otherwise = a `elem'` xs

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' x = foldl' (\acc y -> x == y || acc) False

notElem' :: (Eq a) => a -> [a] -> Bool
_ `notElem'` [] = True
a `notElem'` (x:xs)
    | a == x = False
    | otherwise = a `notElem'` xs

notElem'' :: (Eq a) => a -> [a] -> Bool
notElem'' a xs = not (foldl' (\acc x -> a == x || not acc) False xs)


quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (x:xs) = quicksort lesser ++ [x] ++ quicksort greater
                   where lesser  = filter (< x)  xs
                         greater = filter (>= x) xs

quicksort' :: Ord a => [a] -> [a]
quicksort' []     = []
quicksort' (x:xs) = let smaller = quicksort' [a | a <- xs, a <= x]
                        bigger  = quicksort' [a | a <- xs, a >  x]
                    in smaller ++ [x] ++ bigger

maxCurried :: (Ord a) => a -> (a -> a)
maxCurried x = \ y -> if x >= y then x else y

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x

compareWithHundred' :: (Num a, Ord a) => a -> Ordering
compareWithHundred' = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

sub1 :: Integer -> Integer
sub1 = subtract 1

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _          = []
zipWith' _ _ []          = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g where g y x = f x y

flip'' :: (a -> b -> c) -> (b -> a -> c)
flip'' f = \x y -> f y x

map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = f x : map' f xs

map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr (\x acc -> f x : acc) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ []      = []
filter' p (x:xs)
      | p x       = x : filter' p xs
      | otherwise = filter' p xs

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\x acc -> if p x then x : acc else acc) []

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000, 99999..])
                   where p x = x `mod` 3829 == 0

sumOddSquare :: (Integral a) => a
sumOddSquare = sum ( takeWhile (<10000) (filter odd (map (^2) [1..])))

sumOddSquare' :: (Integral a) => a
sumOddSquare' = sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])

sumOddSquare'' :: (Integral a) => a
sumOddSquare'' = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

sumOddSquare''' :: (Integral a) => a
sumOddSquare''' = let oddSquares = filter odd $ map (^2)  [1..]
                      belowLimit = takeWhile (<10000) oddSquares
                  in sum belowLimit

collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n
      | n < 1  = []
      | even n = n : collatz (n `div` 2)
      | odd n  = n : collatz (n * 3 + 1)

numLongCollatz :: Int
numLongCollatz = length (filter isLong (map collatz [1..100]))
                 where isLong xs = length xs > 15

numLongCollatz' :: Int
numLongCollatz' = length (filter (\ xs -> length xs > 15) (map collatz [1..100]))

addThree :: (Num a) => a -> a -> a -> a
addThree x y z = x + y + z

addThree' :: (Num a) => a -> a -> a -> a
addThree' = \x -> \y -> \z -> x + y + z

triangleNumber :: Int -> Int
triangleNumber 1 = 1
triangleNumber n = n + triangleNumber (n-1)

triangleNumberList :: [Int]
triangleNumberList = [triangle x | x <- [1..]]
                   where triangle x = x * (x + 1) `div` 2

triangleNumberSequence :: [Int]
triangleNumberSequence = [triangle x | x <- [1..]]
                   where triangle x = x * (x + 1) `div` 2

product' :: (Num a) => [a] -> a
product' = foldl1' (*)

last' :: [a] -> a
last' = foldr1 (\ x _ -> x)

sumAcumL :: (Num a) => [a] -> [a]
sumAcumL = scanl' (+) 0

sumAcumR :: (Num a) => [a] -> [a]
sumAcumR = scanr (+) 0

-- How many elements does it take for the sum of the roots of all natural numbers to exceed 1000?
sqrtSums :: [Float]
sqrtSums = takeWhile (<=1000) (scanl1 (+) (map sqrt [1..]))

withoutFunctionApplication :: Integer
withoutFunctionApplication = sum (filter (> 10) (map (*2) [2..10]))

withFunctionApplication :: Integer
withFunctionApplication = sum $ filter (>10) $ map (*2) [2..10]

withoutFunctionComposition :: [Integer]
withoutFunctionComposition = map (\ xs -> negate (sum(tail(xs)))) [[1..5], [2..6], [1..7]]

withFunctionComposition :: [Integer]
withFunctionComposition = map (negate.sum.tail) [[1..5], [2..6], [1..7]]

power2List :: [Integer]
power2List = iterate (*2) 1

iterate'' :: (a -> a) -> a -> [a]
iterate'' f x = x : iterate'' f (f x)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
         | p x = x : takeWhile' p xs
         | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs)
         | p x = dropWhile' p xs
         | otherwise = x : xs

span' :: (a -> Bool) -> [a] -> ([a], [a])
span' _ [] = ([], [])
span' p (x:xs)
    | p x = let (ys, zs) = span' p xs in (x:ys, zs)
    | otherwise = ([], x:xs)

break' :: (a -> Bool) -> [a] -> ([a], [a])
break' _ [] = ([], [])
break' p (x:xs)
     | not (p x) = let (ys, zs) = break' p xs in (x:ys, zs)
     | otherwise = ([], x:xs)

group' :: (Eq a) => [a] -> [[a]]
group' [] = []
group' (x:xs) = (x : ys) : group' zs
                where (ys, zs) = span (==x) xs

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' 0 xs     = ([], xs)
splitAt' _ []     = ([], [])
splitAt' n (x:xs) = let (ys, zs) = splitAt' (n-1) xs
                    in (x:ys, zs)

splitAt'' :: Int -> [a] -> ([a], [a])
splitAt'' _ []         = ([], [])
splitAt'' n (x:xs)
        | n > 0        = (x:ys, zs)
        | otherwise    = (ys, x:zs)
        where (ys, zs) = splitAt'' (n-1) xs

lenUniqueValue :: (Eq a, Ord a) => [a] -> [(a, Int)]
lenUniqueValue = map (\l@(x:xs) -> (x, length l)) . group . sort

inits' :: [a] -> [[a]]
inits' []     = [[]]
inits' (x:xs) = [] : map (x:) (inits' xs)

tails' :: [a] -> [[a]]
tails' []       = [[]]
tails' l@(x:xs) = l : tails' xs

isInfixOf' :: (Eq a) => [a] -> [a] -> Bool
sub `isInfixOf'` lst =
    let len = length sub
    in foldl' (\acc x -> take len x == sub || acc) False (tails lst)

isPrefixOf' :: (Eq a) => [a] -> [a] -> Bool
sub `isPrefixOf'` lst =
    let len = length sub
    in take len lst == sub

isSuffixOf' :: (Eq a) => [a] -> [a] -> Bool
sub `isSuffixOf'` lst =
    let lenSub = length sub
        lenLst = length lst
    in drop (lenLst - lenSub) lst == sub

partition' :: (Eq a) => (a -> Bool) -> [a] -> ([a], [a])
partition' _ []     = ([], [])
partition' p (x:xs) =
           let (ys, zs) = partition' p xs
           in if p x
                 then (x:ys, zs)
                 else (ys, x:zs)

partition'' :: (Eq a) => (a -> Bool) -> [a] -> ([a], [a])
partition'' _ [] = ([], [])
partition'' p (x:xs)
          | p x          = (x:ys, zs)
          | otherwise    = (ys, x:zs)
          where (ys, zs) = partition'' p xs

search :: (Eq a) => [a] -> [a] -> Bool
search xs ys =
    let len = length xs
    in foldl' (\acc x -> take len x == xs || acc) False (tails ys)

find' :: (a -> Bool) -> [a] -> Maybe a
find' _ []      = Nothing
find' p (x:xs)
    | p x       = Just x
    | otherwise = find' p xs

elemIndex' :: (Eq a) => a -> [a] -> Maybe Int
elemIndex' _ [] = Nothing
elemIndex' a xs = indexAcc a xs 0
    where
        indexAcc _ [] _    = Nothing
        indexAcc a (x:xs) i
               | a == x    = Just i
               | otherwise = indexAcc a xs (i + 1)

elemIndices' :: (Eq a) => a -> [a] -> [Int]
_ `elemIndices'` [] = []
a `elemIndices'` xs = indicesAcc a xs 0
    where
        indicesAcc _ [] _    = []
        indicesAcc a (x:xs) i
                 | a == x    = i : indicesAcc a xs (i + 1)
                 | otherwise = indicesAcc a xs (i + 1)

findIndex' :: (a -> Bool) -> [a] -> Maybe Int
findIndex' _ [] = Nothing
findIndex' p xs = indexAcc p xs 0
    where
        indexAcc _ [] _    = Nothing
        indexAcc p (x:xs) i
               | p x       = Just i
               | otherwise = indexAcc p xs (i + 1)

findIndices' :: (a -> Bool) -> [a] -> [Int]
findIndices' _ [] = []
findIndices' p xs = indicesAcc p xs 0
    where
        indicesAcc _ [] _    = []
        indicesAcc p (x:xs) i
                 | p x       = i : indicesAcc p xs (i + 1)
                 | otherwise = indicesAcc p xs (i + 1)

zip4' :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4' [] _ _ _                    = []
zip4' _ [] _ _                    = []
zip4' _ _ [] _                    = []
zip4' _ _ _ []                    = []
zip4' (w:ws) (x:xs) (y:ys) (z:zs) = (w, x, y, z) : zip4' ws xs ys zs

zipWith3' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3' _ [] _ _               = []
zipWith3' _ _ [] _               = []
zipWith3' _ _ _ []               = []
zipWith3' f (x:xs) (y:ys) (z:zs) = f x y z : zipWith3' f xs ys zs

lines' :: String -> [String]
lines' [] = []
lines' s  = let (l, t) = break' (== '\n') s
            in l : case t of
                   []     -> []
                   (_:t') -> lines' t'

unlines' :: [String] -> String
unlines' = foldl1' (\acc x -> acc ++ ['\n'] ++ x)

words' :: String -> [String]
words' [] = []
words' s  = let (w, t) = break' isSpace s
            in w : words' (dropWhile' isSpace t)


unwords' :: [String] -> String
unwords' = foldl1' (\acc x -> acc ++ [' '] ++ x)

nub' :: (Eq a) => [a] -> [a]
nub' []     = []
nub' (x:xs) = x : nub' (filter' (/=x) xs)

delete' :: (Eq a) => a -> [a] -> [a]
delete' _ []      = []
delete' e (x:xs)
      | e == x    = xs
      | otherwise = x : delete' e xs

(\\) :: (Eq a) => [a] -> [a] -> [a]
(\\) [] _            = []
(\\) xs []           = xs
(\\) l@(x:xs) (y:ys) = let a = delete' y l
                       in (\\) a ys

union' :: (Eq a) => [a] -> [a] -> [a]
xs `union'` ys = xs ++ foldl' (\acc y -> if y `elem'` acc || y `elem'` xs then acc else acc ++ [y]) [] ys

intersect' :: (Eq a) => [a] -> [a] -> [a]
xs `intersect'` ys = foldl' (\acc x -> if x `elem` acc || x `elem` ys then acc ++ [x] else acc) [] xs
