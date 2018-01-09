--this is some practice sample of haskell
--notice we hv to ":l baby.hs" before any change applied
--fist practice
doubleMe x = x + x
doubleUs x y = x*2 + y*2

--domain description
doubleSmallNumber x = (if x > 100 then x else x*2)  
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1  

--practice exception, don't put the x pattern in the first statement
sayMe :: (Integral a) => a -> String  
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
sayMe 4 = "Four!"  
sayMe 5 = "Five!"  
sayMe x = "Not between 1 and 5"


--these 2 functions are the same, in different way
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors a b = (fst a + fst b, snd a + snd b) 

addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2) 

--haskell provide fst and snd api for pair, but no api for tuple
--here it is, variable "_" : no further usage
first :: (a, b, c) -> a  
first (x, _, _) = x  
  
second :: (a, b, c) -> b  
second (_, y, _) = y  
  
third :: (a, b, c) -> c  
third (_, _, z) = z 

--practice list comprehension
--combinPair [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]
--[4,7,6,8,11,4]
combinPair xs = [a+b | (a,b) <- xs]

--practice usage of ":", to separate list
head' :: [a] -> a  
head' [] = error "Can't call head on an empty list, dummy!"  
head' (x:y:_) = y

--practice usage of ":"
tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y 

--practice usage of ":"
length' :: (Num b) => [a] -> b  
length' [] = 0  
length' (_:xs) = 1 + length' xs 

--practice usage of "@"
capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x] 

--practice usage of guard
--facing problem when i wanna print out bmi, see trick.hs
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"++show((fromRational(3.14)))
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat = "You're fat! Lose some weight, fatty!"  
    | otherwise                 = "You're a whale, congratulations!"
    where bmi = weight/height^2
          (skinny, normal, fat) = (18.5, 25.0, 30.0) 

--recursive practice: return the MAX of a list
maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs 
--another more clear smaple, using max(return biggest one of two input)
maximum'2 :: (Ord a) => [a] -> a  
maximum'2 [] = error "maximum of empty list"  
maximum'2 [x] = x  
maximum'2 (x:xs) = max x (maximum'2 xs)

--recursive: build a list that repeat element x for n times
--notice that we set i for class Num and class Ord
--since Num is not subclass of Ord, we hc to add both to so add/sub and compare
replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' n x  
    | n <= 0    = []  
    | otherwise = x:replicate' (n-1) x

--resursive practice
take' :: (Num i, Ord i) => i -> [a] -> [a]  
take' n _  
    | n <= 0   = []  
take' _ []     = []  
take' n (x:xs) = x : take' (n-1) xs

--recursive practice: Quick sort
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  

--curried function practice
--multThree 3 5 9 == (((multThree 3) 5) 9)
--the type can also: multThree :: (Num a) => a -> (a -> (a -> a))
multThree :: (Num a) => a -> a -> a -> a  
multThree x y z = x * y * z 

--infix function practice
--divideByTen 200 == 200/10 == (/10) 200
divideByTen :: (Floating a) => a -> a  
divideByTen = (/10) 

--high order example
--use function as input parameter
--notice the declaration
applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x) 

--high order example: zip with different op
--zipWith' (+) [4,2,5,6] [2,6,2,3]
--zipWith' max [6,3,2,1] [7,3,1,5]
--zipWith' (++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"]
--zipWith' (*) (replicate 5 2) [1..] 
--zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys 

--another quick sort version using filter (filter all member in a list with a boolean function)
quicksort_f :: (Ord a) => [a] -> [a]    
quicksort_f [] = []    
quicksort_f (x:xs) =     
    let smallerSorted = quicksort_f (filter (<=x) xs)  
        biggerSorted = quicksort_f (filter (>x) xs)   
    in  smallerSorted ++ [x] ++ biggerSorted 

--another example for filter
--find the largest number divisible by 3829 within 100000
--use filter, define in "where"
largestDivisible :: (Integral a) => a  
largestDivisible = head (filter p [100000,99999..])  
    where p x = x `mod` 3829 == 0

--takeWhile example: 
--for all starting numbers between 1 and 100, how many chains have a length greater than 15?
--the output of chain is a list, check the length of chain
chain :: (Integral a) => a -> [a]  
chain 1 = [1]  
chain n  
    | even n =  n:chain (n `div` 2)  
    | odd n  =  n:chain (n*3 + 1)  

numLongChains :: Int  
numLongChains = length (filter isLong (map chain [1..100]))  
    where isLong xs = length xs > 15
--the same function, using lambda
numLongChains_l :: Int 
numLongChains_l = length (filter (\xs -> length xs > 15) (map chain [1..100]))

--example of fold
sum' :: (Num a) => [a] -> a  
sum' xs = foldl (\acc x -> acc + x) 0 xs 
--another way, due to curried
sum'2 :: (Num a) => [a] -> a  
sum'2 = foldl (+) 0  

--example of fold
elem' :: (Eq a) => a -> [a] -> Bool  
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

--foldl vs foldr
--notice that when we want to apply some function to every element in a list,
--we should consider fold
--btw, : is faster than ++
map'r :: (a -> b) -> [a] -> [b]  
map'r f xs = foldr (\x acc -> f x : acc) [] xs
map'l :: (a -> b) -> [a] -> [b]
map'l f xs = foldl (\acc x -> acc ++ [f x]) [] xs

--some example using foldl1 and foldr1 (use the first element as starting_value)
maximum'f :: (Ord a) => [a] -> a  
maximum'f = foldr1 (\x acc -> if x > acc then x else acc)  
      
reverse'f :: [a] -> [a]  
reverse'f = foldl (\acc x -> x : acc) []  
      
product'f :: (Num a) => [a] -> a  
product'f = foldr1 (*)  
      
filter'f :: (a -> Bool) -> [a] -> [a]  
filter'f p = foldr (\x acc -> if p x then x : acc else acc) []  
      
head'f :: [a] -> a  
head'f = foldr1 (\x _ -> x)  
      
last'f :: [a] -> a  
last'f = foldl1 (\_ x -> x)

-- data type define example
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show) 
--now use the self define data type to define a function
--surface $ Circle 10 20 10        
--314.15927  
--surface $ Rectangle 0 0 100 100        
--10000.0
surface :: Shape -> Float  
surface (Circle _ _ r) = pi * r ^ 2  
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1) 

--another data type define example
data Point' = Point' Float Float deriving (Show)  
data Shape' = Circle' Point' Float | Rectangle' Point' Point' deriving (Show)  
--surface (Rectangle (Point 0 0) (Point 100 100))  
--10000.0  
--surface (Circle (Point 0 0) 24)  
--1809.5574  
surface' :: Shape' -> Float  
surface' (Circle' _ r) = pi * r ^ 2  
surface' (Rectangle' (Point' x1 y1) (Point' x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)  

--record syntax
data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     , height :: Float  
                     , phoneNumber :: String  
                     , flavor :: String  
                     } deriving (Show) 

--devive instance
--deriving (Eq) so we can use '==' between 2 Person'
data Person' = Person' { firstName' :: String  
                     , lastName' :: String  
                     , age' :: Int  
                     } deriving (Eq, Show, Read) 

