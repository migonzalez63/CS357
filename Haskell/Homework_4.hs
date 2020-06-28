-- Homework 4
-- Miguel Gonzalez
-- CS357

-- Problem 1
-- The function stutter takes a list of elements and returns a list where every
-- elements and returns a list where every element has been duplicates.
-- For example:
-- Main> stutter "Hello World"
--  "HHeelllloo  WWoorrlldd"
-- Main> stutter [1,2,3]
--  [1,1,2,2,3,3]

-- Creates a procedure called stutter that takes a list and duplicates every
-- item of the list
stutter [] = []
stutter (x:xs) = replicate 2 x ++ stutter xs


-- Problem 2
-- The function compress elimintaes consecutive duplicate elementts of a list.
-- For example,
-- Main> compress "HHeelllloo  WWoorrlldd"
--  "Helo World"
-- Main> compress [1,2,2,3,3,3]
--  [1,2,3]

-- Creates a procedure called compress that removes any repeating  consecutive
-- character that matches it from the list.
compress [] = []
compress (x:xs) = if null xs || x /= head xs then x : compress xs else compress xs

-- Problem 3
-- The function findIndices takes a predicate and a list as arguments and
-- returns a list of numbers indicatinf the posittion of lemenets int the list
-- which satisfy the predicate.
-- For example:
-- Main> findIndeces (< 'a') "AbCdef"
--  [0,2]
-- Main> findIndices (== 0) [1,2,0,3,0]
--  [2,4]

-- Creates a procedure called findIndices that finds the indices of the
-- elements of a list that match the pred ny zipping the list to its
-- corresponding value, filtering the list with those that pass the pred,
-- then mapping snd to extract the index from the tuple made by zip
findIndices pred x = map snd (filter (pred.fst) (zip x [0.. ((length x) - 1)])) 

-- Problem 4
-- The function intersect takes two list as arguments and returns a list of
-- elements common to both lists.
-- For example:
-- Main> intersect "abc" "cat"
--  "ac"
-- Main> intersect [1,2,3] [8]
--  []
-- Main> intersect [3,2,1] [1,2,3]

-- Creates a procedure called intersect that will return a list of the values
-- that are common to both list by checking if items in the first list are
-- members of the second list and consing them into a list if they are
intersect [] _ = []
intersect (x:xs) y = if x `elem` y then x : intersect xs y else intersect xs y

-- Problem 5
-- The function isPrefixOf takes two lists as arguments and returns True iff
-- the first list is a prefix of the second list.
-- For example:
-- Main> "foo" `isPrefixOf` "foobar"
--  True
-- Main> isPrefixOf [1,2,3] [4,5,6]
--  False

-- Creates a procedure called isPrefixOf that checks if x is a prefix to y
-- by making a sublist of length of x from y and checking that x and the
-- sublist match
isPrefixOf x y = if x == take (length x) y then True else False

-- Problem 6
-- The function isSuffixof taked two lists as arguments and returns true iff
-- the first list is a suffix of the second list.
-- For example:
-- Main> "bar" `isSuffixOf` "foobar"
--  True
-- Main> isSuffixOf [1,2,3] [4,5,6]
--  False

-- Creates procedure called isSuffixOf that takes two list and checks if
--  x is a suffix of y by reversing the list, taking a sublist of length x
-- from the reversed list and the reversing the sublist and comparing it to x
isSuffixOf x y = if x == reverse (take (length x) (reverse y)) then True else False

-- Problem 7
-- The dot product of two vectors U and V of length n (written u dot v) is
-- defined to be the sum of i=1 to n of the product of ui and vi. Define
-- a function dot which takes two lists of numbers of equal lenght and
-- returns their dot product
-- For example:
-- Main> [0,0,1] `dot` [0,1,0]
--  0
-- Main> [1,2,1] `dot` [2,1,4]
--  8

-- Creates a procedure called dot that takes the dot product of two list
dot [] [] = 0
dot (x:xs) (y:ys) = x*y + dot xs ys

-- Problem 8
-- The function increasing takes a list of enumerable elements as its
-- arguments and returs True if the list is sorted in increasing order
-- and False otherwise
-- For example:
-- Main> increasing "ABCD"
--  True
-- Main> increasing [100,99..1]
--  False

-- Creates a procedure called increasing that checks if a list is in
-- increasing order.
increasing [] = True
increasing (x:xs) = if null xs || x < head xs then increasing xs else False

-- Problem 9
-- To 'decimate' literally means to kill every tenth man (it was a punishment
-- in the Roman legions). Define a function deciamte which removes every
-- tenth element from a list
-- For example,
-- Main> decimate [1..21]
--  [1,2,3,4,5,6,7,8,9,11,12,13,14,15,16,17,18,19,21]

-- Creates a procedure called decimate that removes every tenth element of
-- the list by zipping the list with its lenght, filtering in all tuples
-- whose first item is not equal to zero when we mod 10 to it and map snd
-- to that list of tuples
decimate x = map snd (filter ((\x -> 0 /= x `mod` 10).fst) (zip x [1..(length x)]))

-- Problem 10
-- Define a function prefixSum which takes a list of numbers as its arguments
-- and returns a list of sums of all prefixes of a list.
-- For example,
-- Main> prefixSum [1..10]
--  [1,3,6,10,15,21,18,36,45,55]
-- Main> prefixSum [2, 5]
-- [2, 7]

-- Creates a procedure called prefixSum that does a scan of the list x and
-- outputs a list of the sum of the prefixes
prefixSum x = scanl1 (+) x


-- Problem 12
-- The function select tales a predicate and two lists as arguments and
-- returns a list composed of elements from the second list in those
-- positions where the predicate, when applied to the element in the
-- corresponding position of the first list, returns True.
-- For example,
-- Main> :t select
--  select :: (t -> Bool) -> [t] -> [a] -> [a]
-- Main> select even [1..26] "abcdefghijklmnopqrstuvwxyz'
--  "bdfhjlnprtvxz"
-- Main> select (<= 'g') "abcdefghijklmnopqrstuvwxyz" [1..26]

-- Creates procedure called select that creates a list of item that pass
-- the pred test by first zipping the list x and y, filtering in with the
-- pred, and then mapping snd to the list of tuples given by the filter
select pred x y = map snd (filter (pred.fst) (zip x y))

-- Problem 12
-- The function numbers which takes a list of integers as its argument and
-- returns the integer which has those numbers as digits.
-- For example,
-- Main> numbers [1..4]
--  1234

numbers x = helper x 0 where helper (x:xs) n = if null xs then ((n * 10) + x) else helper xs ((n*10) + x) 
