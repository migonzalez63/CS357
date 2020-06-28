-- Miguel Gonzalez
-- CS 357
-- April 14, 2019

-- Problem 1
-- Define a function myTakeWhile which takes a predicate and a list as arguments and
-- returns the prefix of the list satisfying the predicate. For example,
-- Main> myTakeWhile (/= ' ') "This is practice."
--   "This"

-- Creates a procedure called myTakeWhile that will test the head of the tail of the list to see if
-- satisfies the predicate. If it does, we will the head of the list with a recursive call to the tail
myTakeWhile _ [] = []
myTakeWhile pred (x:xs) = if (pred (head xs)) == False then x:[] else x:myTakeWhile pred xs  

-- Problem 2
-- Define a function mySpan which takes a predicate and a list as arguments and returns a
-- pair of list where the first element of the pair is the portion of the list which the
-- function myTakeWhile would return and the second element is the remainder of the list.
-- For example.
-- Main> mySpan (/= ' ') " This is practice."
--  ("This", " is practice.")

-- Creates a procedure that outputs a tuple of list that myTakeWhile would output and the rest of
-- the  list by creating a helper function that does the oposite to myTakeWhile
mySpan pred xs = ((myTakeWhile pred xs), (helper pred xs))
    where helper pred (x:xs) = if (pred (head xs)) == False then xs else helper pred xs 

-- Problem 3
-- The function combinations3 takes a list as its argument and returns a list of length'
-- three lists representing all possible subset of size three. For example.
-- Main> :t combinations3
--  combinations3 :: (Ord a) => [a] -> [[a]]
-- Main> combinations3 "ABCDE"
--  ["ABC","ABD","ABE","ACD","ACE","ADE","BCD","BCE","BDE","CDE"]

--mbinations3 [] = [[]]
--combinations3 xs =  [x:y:z | m <- xs , x <

-- Problem 4
-- The function runLengthEncode takes a list of values as its argument and returns a list
-- of pairs of values and run lengths.
-- For example,
-- Main> runLengthEncode [4,2,2,1,1,1,1,4,4,4,4]
--  [(4,2), (2,2), (1,4), (4,4)]

runLengthEncode [] = []
runLengthEncode (x:xs) = (length $  x : takeWhile (== x) xs, x) : runLengthEncode(dropWhile (== x) xs)


-- Problem 5
-- The function runLengthDecode takes a list of pairts of values and run lengths and returns a list
-- of values. For example,
-- Main> runLenghthDecode [(4,1), (2,2), (1,4), (4,4)]
--  [4,2,2,1,1,1,1,4,4,4,4]

runLengthDecode [] = []
runLengthDecode (x:xs) = take (snd x) (repeat (fst x)) ++ runLengthDecode xs


-- Problem 6
-- Define a function splitText which takes a string oif text and a predicate and returns a list
-- of substrings comprised of contigous characters for which the predicate is satisfied.
-- For example:
-- Main> splitText (/= ' ') "This is practice."
--  ["This", "is", "practice"]

splitText _ [] = []
splitText pred xs = takeWhile pred xs : splitText pred l
     where l = if null (dropWhile pred xs) == False then tail (dropWhile pred xs) else []

-- Problem 7
-- Use list comprehension to define a function encipher which takes two lists of equal length and a thirs list.
-- It uses the first two lists to define a substitution cipher which it uses to encipher the thirs list.
-- For example:
-- Main> encipher ['A'..'Z'] ['a'..'z'] "THIS"
--  "this"

--encipher _ _ [] = []
--encipher xs ys zs = [x | 

-- Problem 8
-- The Goldbach conjecture states that any even number greater than two can be written as the sum of two
-- prime number. Using list comprehensions, write a function goldbach, which when given an even number n,
-- returns a list of all pairs of prime which sum to n.

goldbach n = [(x,y) | x <- primes n, y <- primes n, x + y == n, x <= y] 
   where primes n = [z | z <- [2..n], (z `mod` 2 /= 0)]

-- Problem 9
-- The function increases takes a list of enumerable elemets as its argument and returns True if the
-- list is sorted in increasing order and False otherwise. Write increasing using the function
-- and and a list comprehension.

increasing xs = [x | x <- xs, y <- reverse xs, x > y]  

-- Problem 10
-- The function select takes a predicate ans two lists as arguments ad returns a list composed
-- of elements from the second listr in those positions where the predicate, when applies to the
-- element in the corresponding positions of the first list, returns True. Write select usding
-- list comprehensions.

select pred xs ys = [x | (x, y) <- zip xs ys, pred x]  

-- Problem 11
-- The function combinations takes an integer k and a list of elements of typeclass Ord as its
-- arguments and returns a list of length k list representing all possible subsets of size k.
-- Write combinations

-- Problem 12
-- Addition and multiplication of complex numbers are defined as follows:
-- (x+iy) + (u+iv) = (x+u) + (y+v)i
-- (x+iy)×(u+iv) = (xu−yv) + (xv+yu)i
-- A complex intiger is a complex number with intefer real ans imginary parts. Define a data
-- type for complex integers called ComplexInteger with selector functions real and imaginary
-- which retun the real and imaginary parts. Give aminimum instance decleration for Eq, Show,
-- and Num type classes.

data ComplexInteger = ComplexInteger {real :: Int, imaginary :: Int}

instance Eq ComplexInteger where
   (ComplexInteger a b) == (ComplexInteger c d) = (a == c) && (b == d)

instance Show ComplexInteger where
   show (ComplexInteger x 0) = show x
   show (ComplexInteger 0 x) = show x ++ "i"
   show (ComplexInteger x y) = show x ++ "+" ++ show  y ++ "i"

instance Num ComplexInteger where
   (ComplexInteger a b) * (ComplexInteger c d) = ComplexInteger ((a*c) - (b*d))  ((a*d) + (b*c))
   (ComplexInteger a b) + (ComplexInteger c d) = ComplexInteger (a + c) (b + d)
   (ComplexInteger a b) - (ComplexInteger c d) = ComplexInteger (a - c) (b - d)
   abs (ComplexInteger a b) = ComplexInteger (abs a) (abs b)
