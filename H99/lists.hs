import Data.List
import Data.Char
import Data.Maybe


--1

myLast :: [a] -> a
myLast [x] = x
myLast (h:t) = myLast t

--myLast [1,2,3,4]
--myLast ['x','y','z']

--2

myButLast :: [a] -> a
myButLast [a,b] = a
myButLast (a:b) = myButLast b

--myButLast [1,2,3,4]
--myButLast ['a'..'z']

--3 

elementAt :: [a] -> Int -> a
elementAt (h:t) 1 = h
elementAt (h:t) x = elementAt t (x-1)

--elementAt [1,2,3] 2
--elementAt "haskell" 5

--4

myLength :: [a] -> Int
myLength l = length l 

--myLength [123, 456, 789]
--myLength "Hello, world!"


--5

myReverse :: [a] -> [a]
myReverse l = reverse l

--myReverse "A man, a plan, a canal, panama!"
--myReverse [1,2,3,4]

--6

isPalindrome :: Eq a => [a] -> Bool
isPalindrome l = if l == myReverse l
            then True
        else False

--isPalindrome [1,2,3]
--isPalindrome "madamimadam"
--isPalindrome [1,2,4,8,16,8,4,2,1]

--7

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

--8

compress :: Eq a => [a] -> [a]
compress [] = []
compress ls = foldl (\acc x -> if last acc /= x then acc ++ [x] else acc) [head ls] ls 

--compress "aaaabccaadeeee"

--9

pack :: Eq a => [a] -> [[a]]
pack [x] = [[x]]
pack (h:t) = if elem h (head (pack t))
                then (h : (head(pack t))) : (tail(pack t))
            else [h] : (pack t)

--  pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a','a', 'd', 'e', 'e', 'e', 'e']

--10

encode :: Eq a => [a] -> [(a,Int)]
encode l = [(head i,length i) | i<-pack l]

--encode "aaaabccaadeeee"