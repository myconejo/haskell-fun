-- | TODO marker.
todo :: t
todo = error "todo"

-- | Days of week.
data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday deriving (Eq, Show)

-- | Returns the next weekday (excluding weekend, namely Saturday and Sunday).
nextWeekday :: Day -> Day
nextWeekday day = case day of 
    Monday      -> Tuesday
    Tuesday     -> Wednesday
    Wednesday   -> Thursday
    Thursday    -> Friday
    _           -> Monday

-- | Add tuples of the 2-dimensional plane.
addTuple :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
addTuple t1 t2 = ((fst t1) + (fst t2), (snd t1) + (snd t2))

-- | Dot-products two integer (list) vectors: https://en.wikipedia.org/wiki/Dot_product
-- |
-- | If the two vectors have different number of elements, you can return anything.
productDot :: [Integer] -> [Integer] -> Integer
productDot t1 t2 = foldr (+) 0 $ map (\(a,b) -> a*b) (zip t1 t2)

-- | Maps the given value if it's Just.
maybeMap :: (Integer -> Integer) -> Maybe Integer -> Maybe Integer
maybeMap f value = case value of
    (Just n) -> (Just $ f n)
    Nothing  -> Nothing

-- | If the given value is Just, map it with the given function; otherwise, the result is Nothing.
maybeThen :: Maybe Integer -> (Integer -> Maybe Integer) -> Maybe Integer
maybeThen value cont = case value of
    (Just n) -> cont n
    Nothing  -> Nothing

-- | Trees of integers.
data Tree = Leaf Integer | Branch Integer Tree Tree deriving (Eq, Show) -- Integer is value, Trees are left/right subtrees.

-- | Sums all the integers in the given tree.
sumTree :: Tree -> Integer
sumTree (Leaf x)        = x
sumTree (Branch x l r)  = x + (sumTree l) + (sumTree r)

-- | Right-rotate the given tree. See https://en.wikipedia.org/wiki/Tree_rotation for more detail.
-- |
-- | Returns Nothing if there are not enough nodes.
data Tree = Empty | Node Integer Tree Tree deriving (Show, Eq)

rightRotateTree :: Tree -> Maybe Tree
rightRotateTree Empty = Nothing
rightRotateTree (Node _ Empty _) = Nothing
rightRotateTree (Node x (Node y leftChild rightChild) rightSubtree) =
    Just $ Node y leftChild (Node x rightChild rightSubtree)

-- | Maps the given list.
listMap = map

-- | Sums all the integers in the given list.
listSum :: [Integer] -> Integer
listSum l = foldr (+) 0 l

-- | More compositional construction of sigma.
sumSeq :: (Integer -> Integer) -> Integer -> Integer -> Integer
sumSeq f from to = listSum (listMap f [from .. to])

-- | product of a sequence. See https://en.wikipedia.org/wiki/Multiplication#Product_of_a_sequence for more detail.
productSeq :: (Integer -> Integer) -> Integer -> Integer -> Integer
productSeq f from to = foldr (*) 1 (map f [from .. to])

-- | Returns if the given value is in the (list) set.
setMem :: Integer -> [Integer] -> Bool
setMem _ []     = False
setMem v [n]    = v == n
setMem v ns     = (v == head ns) || (setMem v $ tail ns)

-- | Returns the two sets contain the same elements.
contain :: [Integer] -> [Integer] -> Bool
contain [] ys      = True
contain (x:xs) ys  = setMem x ys && contain xs ys

setEquiv :: [Integer] -> [Integer] -> Bool
setEquiv xs ys = contain xs ys && contain ys xs

-- | Returns the set union.
setUnion :: [Integer] -> [Integer] -> [Integer]
setUnion [] s2              = s2
setUnion s1 s2
    | setMem (head s1) s2   = setUnion (tail s1) s2
    | otherwise             = setUnion (tail s1) ((head s1):s2)

-- | Returns the set intersection
setIntersection :: [Integer] -> [Integer] -> [Integer]
setIntersection [] s2 = []
setIntersection s1 s2
    | setMem (head s1) s2   = [head s1] ++ setIntersection (tail s1) s2
    | otherwise             = setIntersection (tail s1) s2

-- | Returns the set diff, i.e., setDiff a b = $a - b$.
setDiff :: [Integer] -> [Integer] -> [Integer]
setDiff [] s2 = []
setDiff s1 [] = s1
setDiff s1 s2
    | setMem (head s1) s2   = setDiff (tail s1) s2
    | otherwise             = [head s1] ++ (setDiff (tail s1) s2)

-- | Returns the set symmetric diff.
setSymDiff :: [Integer] -> [Integer] -> [Integer]
setSymDiff s1 s2 = setUnion (setDiff s1 s2) (setDiff s2 s1)

r1 :: [(Integer, Integer)]
r1 = [(1,2), (2,4), (3,5), (3,6), (5,7)]

r2 :: [(Integer, Integer)]
r2 = [(1,2), (2,3), (3,4), (4,1)]

-- | Returns if the given pair is in the (list) relation.
relMem :: [(Integer, Integer)] -> Integer -> Integer -> Bool
relMem rel v1 v2 = elem (v1,v2) rel

relContain :: [(Integer, Integer)] -> [(Integer, Integer)] -> Bool
relContain [] ys     = True
relContain (x:xs) ys = elem x ys && relContain xs ys

-- | Returns the two relations contain the same elements.
relEquiv :: [(Integer, Integer)] -> [(Integer, Integer)] -> Bool
relEquiv r1 r2 = relContain r1 r2 && relContain r2 r1

compose :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
compose r1 r2 = (fst r1, snd r2)

getDomain :: [(Integer, Integer)] -> [Integer]
getDomain rels = map (\(a,b) -> a) rels

filterDom :: Integer -> [(Integer, Integer)] -> [(Integer, Integer)]
filterDom n rels = filter (\(a,b) -> a == n) rels

getRange :: [(Integer, Integer)] -> [Integer]
getRange rels = map (\(a,b) -> b) rels

-- | Composes two relations, i.e., {(a,c) | exists b, (a,b) in r1 and (b,c) in r2}.
relComp :: [(Integer, Integer)] -> [(Integer, Integer)] -> [(Integer, Integer)]
relComp [] r2 = []
relComp r1 r2
    | elem (snd $ head r1) (getDomain r2)   = (map (\b -> ((fst $ head r1), b)) (getRange $ filterDom (snd $ head r1) r2)) ++ relComp (tail r1) r2
    | otherwise                             = relComp (tail r1) r2

noDupAdd :: (Integer, Integer) -> [(Integer, Integer)] -> [(Integer, Integer)]
noDupAdd rl rs
    | elem rl rs    = rs
    | otherwise     = rl:rs

isConnected :: (Integer, Integer) -> (Integer, Integer) -> Bool
isConnected r1 r2
    | fst r1 == fst r2  = True
    | fst r1 == snd r2  = True
    | snd r1 == fst r2  = True
    | snd r1 == snd r2  = True
    | otherwise         = False

-- | Returns the transitive closure of the given relation: https://en.wikipedia.org/wiki/Transitive_closure
relTrans :: [(Integer, Integer)] -> [(Integer, Integer)]
relTrans _ = []

-- | Returns the relation [0..n] * [0..n] = {(0,0), (0,1), ..., (0,n), (1,0), (1,1), ..., (1,n), ..., (n,n)}.
relFull :: Integer -> [(Integer, Integer)]
relFull n = [(x, y) | x <- [0..n], y <- [0..n]]

-- | The Fibonacci sequence, starting with 0, 1, 1, 2, 3, ...
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- | The primes, starting with 2, 3, 5, 7, ...
primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [ x | x <- xs, x `mod` p > 0]

-- | The sequence of 1, 2, 1, 3, 2, 1, 4, 3, 2, 1, 5, 4, 3, 2, 1, ...
fuzzySeq :: [Integer]
fuzzySeq = concatMap (\n -> reverse [1..n]) [1..]

-- | Composes two functions, i.e., applies f1 and then f2 to the given argument
funComp :: (Integer -> Integer) -> (Integer -> Integer) -> (Integer -> Integer)
funComp f1 f2 = f2 . f1

-- | Transforms a function that gets single pair into a function that gets two arguments, i.e., curry2 f a1 a2 = f (a1, a2)
curry2 :: ((Integer, Integer) -> Integer) -> Integer -> Integer -> Integer
curry2 f a1 a2 = f (a1,a2)

-- | Transforms a function that gets two arguments into a function that gets single pair, i.e., uncurry2 f (a1, a2) = f a1 a2
uncurry2 :: (Integer -> Integer -> Integer) -> (Integer, Integer) -> Integer
uncurry2 f (a1, a2) = f a1 a2

-- | Filters the given list so that the the filter function returns True for the remaining elements.
myFilter :: (Integer -> Bool) -> [Integer] -> [Integer]
myFilter _ [] = []
myFilter f ns
    | (f $ head ns)     = [head ns] ++ (myFilter f $ tail ns)
    | otherwise         = myFilter f $ tail ns

safeDiv :: Integer -> Integer -> Maybe Integer
safeDiv _ 0 = Nothing
safeDiv a b = Just $ div a b

-- | Maps the given list. If the map function returns Nothing, just drop it.
myFilterMap :: (Integer -> Maybe Integer) -> [Integer] -> [Integer]
myFilterMap _ [] = []
myFilterMap f ns
    | (f $ head ns) == Nothing  = myFilterMap f $ tail ns
    | otherwise                 = [head ns] ++ (myFilterMap f $ tail ns)

-- | Folds the list from the left, i.e., myFoldL init f [l1, l2, ..., ln] = (f (f (f (f init l1) l2) ...) ln).
myFoldL :: Integer -> (Integer -> Integer -> Integer) -> [Integer] -> Integer
myFoldL init f [] = init
myFoldL init f ns = myFoldL (f init (head ns)) f (tail ns)

-- | Reverses the given list.
myRev :: [Integer] -> [Integer]
myRev [] = []
myRev l = myRev (tail l) ++ [head l]
