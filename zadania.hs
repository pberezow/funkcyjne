-- 4.

trojka :: Int a => a -> [(a, a, a)]
trojka n = [(a, b, c) | a <- [1..n], b <- [1..a], c <- [1..b], a^2 == b^2 + c^2 && gcd b c == 1]

-- 6.
choose :: Int a => a -> a -> a
choose n k
    | k == 0 || k == n = 1
    | n > k = choose (n-1) (k-1) + choose (n-1) k
    | otherwise = error "Nieprawidlowe argumenty"

-- 25.
inits :: [a] -> [[a]]
inits xs = [take n xs | n <- [0..length xs]]

-- 27.
nub :: Eq a => [a] -> [a]
nub xs = nub' [] xs
    where
        nub' acc (x:xs) = 
            if not (x `elem` acc) then
                nub' (acc ++ [x]) xs
            else
                nub' acc xs
        nub' acc [] = acc

-- 50.
pack :: Eq a => [a] -> [[a]]
pack xs = f [] xs []
    where
        f current (y:ys) acc =
            if y `elem` current || null current then
                f (current ++ [y]) ys acc
            else
                f [y] ys (acc ++ [current])
        f current [] acc = (acc ++ [current])

-- 51.
rleEncode :: [Char] -> [(Int, Char)]
rleEncode xs = f [] xs []
    where
        f current (y:ys) acc =
            if y `elem` current || null current then
                f (current ++ [y]) ys acc
            else
                f [y] ys (acc ++ [(length current, head current)])
        f current [] acc = (acc ++ [(length current, head current)])

rleDecode :: [(Int, Char)] -> [Char]
rleDecode xs = f xs []
    where
        f ((count, sym):ys) acc =
            f ys (acc ++ repeat sym count [])
                where
                    repeat sym count acc =
                        if count > 0 then
                            repeat sym (count-1) (acc ++ [sym])
                        else
                            acc
        f [] acc = acc

-- rleDecode xs = f xs []
--     where
--         f ((count, sym):ys) acc =
--             f ys (acc ++ repeat sym count [])
--         f [] acc = acc
--         repeat sym count acc =
--             if count > 0 then
--                 repeat sym (count-1) (acc ++ [sym])
--             else
--                 acc

-- 56.
sum_list' :: Num a => [a] -> [a]
sum_list' list = zipWith (*) [(-1)^n | n <- [2..((length list)+1)]] list

sum_list :: Num a => [a] -> a
sum_list xs = foldl (+) 0 (sum_list' xs)

-- 65.
data BTree a = L a | N a (BTree a) (BTree a) deriving (Eq)

instance (Show a) => Show (BTree a) where
    show (L x) = "<" ++ show x ++ ">"
    show (N x l r) = "[" ++ show l ++ ", " ++ show x ++ ", " ++ show r ++ "]"

instance Functor BTree where
    fmap f (L x) = L (f x)
    fmap f (N x l r) = N (f x) (fmap f l) (fmap f r)

instance Foldable BTree where
    foldr f z (L x) = f x z
    foldr f z (N x l r) = foldr f (f x (foldr f z r)) l

findInBTree :: Eq a => a -> BTree a -> Bool
findInBTree x (L y)
    | x == y = True
    | otherwise = False
findInBTree x (N y l r)
    | x == y = True
    | otherwise = (findInBTree x l) || (findInBTree x r)

-- findInBTree x tree = foldr (\a b -> (a == x) || b) False tree

nodeCount :: BTree a -> Int
nodeCount tree = foldr (\_ -> (+1)) 0 tree

concatBTree :: a -> BTree a -> BTree a -> BTree a
concatBTree x l_tree r_tree = N x l_tree r_tree

foldTree :: (z->a->z->z) -> z -> BTree a -> z
foldTree f z (L a) = z
foldTree f z (N x l r) = f (foldTree f z l) x (foldTree f z r)

h :: (Ord z, Num z) => z -> a -> z -> z
h z1 x z2 = (max z1 z2) + 1

treeHeight :: BTree a -> Int
treeHeight tree = foldTree h 1 tree

l :: (Num z) => z -> a -> z -> z
l z1 x z2 = z1 + z2

leafCount :: BTree a -> Int
leafCount tree = foldTree l 1 tree

-- 77.
newtype Writer w a = Writer (a,w) deriving Show

instance Functor (Writer w) where
    fmap f (Writer (x,s)) = Writer (f x, s)
  
instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    (Writer (x,w)) >>= f = let Writer (y,t) = f x in Writer (y, w `mappend` t)
  
instance (Monoid w) => Applicative (Writer w) where
    pure = return
    -- fw <*> xw = do f<- fw; x<- xw; return( f x)
    Writer (f, m) <*> Writer (x, n) = Writer (f x, m `mappend` n)

f :: Int -> Int
f x = if x `mod` 2 == 0 then x `div` 2 else 3 * x + 1

h :: Int -> Writer (Sum Int, String) Int
h 1 = Writer (1, (0, ""))
h x = do
    h (f x) >>= (\y -> 
                    if x `mod` 2 == 0 then Writer (f x, (1, "+"))
                    else Writer (f x, (1, "-"))
                )

-- 82.