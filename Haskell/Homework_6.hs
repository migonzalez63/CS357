import Data.List

-- Miguel Gonzalez
-- CS357
-- April 29, 2019

-- Problem 1
-- The function bits2num takes a string of 0's and 1's and returns the binary number represented
-- by that string. For example:
-- Main> bits2num "1011000"
--  88

-- Defines a procedure called bits2num that takes a list of bits, reverses it, finds the indices of the
-- items that equal to one, maps 2^index, and sums the whole list together
bits2num xs = sum $ map (2^) $ findIndices (== '1') $ reverse xs
   where findIndices pred x = map snd (filter (pred.fst) (zip x [0.. ((length x) - 1)]))

-- Problem 2
-- The function num2bits takes an integer and returns a string representing that number in binary.
-- For example,
-- Main> num2bits 87783
-- "010101011011100111"

num2bits 0 = []
num2bits xs = reverse (helper xs)
   where helper 0 = ['0']
         helper xs = if xs `mod` 2 == 1 then '1' : helper (xs `div` 2) else '0' :  helper (xs `div` 2)

-- Problem 3
-- The variance of a list of numbers of length n is the average squared difference between each
-- number and the numbers' mean. Without using explicit recursion, give a definition of a function,
-- variance, which works as follows:
-- Main> variance [1..10]
--  8.25

variance xs = sum $ map (\ (x, y) -> ((x - y) * (x - y)) / realToFrac(length xs)) $ zip xs (replicate (length xs) mean)
   where mean = sum $ map (/ realToFrac(length xs)) xs

-- Problem 4
-- Define a function difference which, when given two sets xs and ys (represented as lists) returns the set
-- consisting of all elements of xs not found in ys. Note: This definition is asymmetrical. For example:
-- Main> difference "ABCD" "AD"
--  "BC"

difference [] ys = []
difference (x:xs) ys = if x `elem` ys then difference xs ys else x : difference xs ys

-- Problem 5
-- Recall that the function combinations takes a list of elements of typeclass Ord and an integer
-- k as its arguments and returns a list of (n k) length k list representing all possible subsets
-- of size k. The function splits is similar, except that, given a list of elements of length n, it
-- returns a list of ∑ (n k) where k = 1 to n-1 pairs of lists. The first component of the pair represents
-- a combination of length k. The second component represents the complementary combinatio of length n-k. Two
-- combination are complementary when their union is equal to the original list. For example,
-- Main> :t splits
--  splits :: (Ord a) => [a] -> [([a], [a])]
-- Main> splits "abc"
-- [("c", "ab"), ("b", "ac"), ("bc", "a"), ("a", "bc"), ("ac", "b"), ("ab", "c")]

combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = (map (x:) (combinations (n-1) xs)) ++ (combinations n xs)

splits n xs = [(x, y) | x <- combinations n xs, y <- combinations ((length xs) - n) xs, (helper (x ++ y) xs) == True]
   where helper xs ys = null (xs \\ ys) && null (ys \\ xs)

 
-- Problem 6
-- The function argmin takes a function f and a list xs as arguments and returns the element of the
-- list x such that f applied to x has minimum value. For example,
-- Main> :t argmin
--  argmin :; (Ord a) => (t -> a) -> [t] -> t
-- Main> argmin length ["ABC", "EF", "GHIJ", "K"]
-- "K"

-- Creates a procedure called argmin that will first zip the given list with a list in which the function
-- f has been applied to it. Then we will filter out all results that are equal to the minimum value of that
-- list, and get the head of the list.
argmin f xs = fst $ head $ filter ((== b).snd) $ a
   where a = zip xs $ map f xs
         b = foldr1 min $ map snd a

-- Problem 7
-- The function bogus takes a list of pairs of source alphabet probabilities and values and returns
-- a coding tree which can be used with the functions, encode and decode, for encoding and decoding
-- Huffman codiung trees defined in class. The bogus coding algorithm is very simple: it splits its
-- argumnet into two subsets where the sum of the probabilities in each subset are as nearly equal
-- as possible. It then uses the first subset to recursively build the left half of the bogus coding
-- tree and the second to recursively build the right half of the bogus coding tree. For example:
-- Main> :t bogus
-- bogus :: (Ord t) => [(Double, t)] -> Htree t
-- Main>  let xs = [(0.30,’e’), (0.14,’h’), (0.1,’l’), (0.16,’o’), (0.05,’p’), (0.23,’t’), (0.02,’w’)]
-- Main>  concatMap (encode (bogus xs)) "hello"
--  "101001001000011"
-- Main> decode (bogus xs) "10100100100011"
--  "hello"



-- Problem 8
-- The function church takes an integer n as its argument and returns a function which composes any
-- unary function n times. For example,
-- :t church
--  church :; Int -> (c -> c) -> c -> c
-- Main> (church 4) tail "ABCDEFGH"

church n f = (iterate (. f) id) !! n

-- Problem 9
-- Using the definintion of BTree given in class, write a function trees which takes a list
-- of leaf values as its argument and retuns a list of all binary trees with the given
-- leaves. For example,
-- Main> :t trees
--  trees :: (Orde t) => [t] -> [Btree t]
-- Main> (trees "ABCDE") !! 114
--  Fork (Leaf E) (Fork (Fork (Leaf A) (Fork (Leaf C) (Leaf B))) (Leaf D))
-- Main> length (trees [0..4])

data Btree a = Leaf a | Fork (Btree a) (Btree a) deriving (Show, Eq)
 
-- Problem 10
-- A DNA molecule is a sequence of four bases which are conventionally represented using the
-- 'A', 'G', 'C', and 'T'. Genomes represented by DNA molecules are subject to four different
-- types of point mutations:
--     insertions - A base is inserted between two adjacent points in a genome.
--     deletions - A point is deleted from a genome.
--     substitutions - A base at a point is replaced with another base.
--     transpositions - The bases at two adjacent points are exchanged.
-- Give definitions for Haskell functions insertions, deletion, substitutions, and transpositions
-- which take a genome represented as a string and returns a list of all genomes produced by single
-- point mutation of the specified kind.

insertions [] = [['A']] ++ [['T']] ++ [['G']] ++ [['C']]
insertions (x:xs) = [['A']++(x:xs)] ++ [['T']++(x:xs)] ++ [['G']++(x:xs)] ++ [['C']++(x:xs)] ++ (map (x:) (insertions xs))

deletions [] = [[]]
deletions (x:[]) = [[]]
deletions (x:xs) = [xs] ++ map (x:) (deletions xs)

substitutions [] = [[]]
substitutions (x:xs) = [['A']++(xs)]++[['T']++(xs)] ++ [['G']++(xs)] ++ [['C']++(xs)] ++ map (x:) (substitutions xs)

transpositions [] = [[]]
transpositions (a:[]) = [[a]]
transpositions (x:y:[]) = [(y:x:[])]
transpositions (x:y:xs) = [(y:x:xs)] ++ map (x:) (transpositions (y:xs))

