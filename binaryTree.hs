data Btree a = Leaf a | Fork (Btree a) (Btree a) deriving (Show, Eq)
foo = Fork (Fork (Fork (Fork (Leaf 'a') (Leaf 'b')) (Leaf 'c')) (Leaf 'd')) (Leaf 'e')
size :: Btree a -> Int
size (Leaf x) = 1
size (Fork xt yt) = size xt + size yt

height :: Btree a -> Int
height (Leaf x) = 0
height (Fork xt yt) = 1 + (height xt) `max` (height yt)

flatten :: Btree a -> [a]
flatten (Leaf x) = [x]
flatten (Fork xt yt) = flatten xt ++ flatten yt
