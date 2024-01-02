import Data.List (group)

p1 = last
p2 = last . init

p3 :: [a] -> Int -> a
p3 l i = l !! (i-1)

p4 :: [a] -> Int
p4 = length


p5 = reverse

p6 [] = True
p6 [_] = True
p6 l = head l == last l && p6 (tail . init $ l)

data NestedList a = Elem a | List [NestedList a]

p7 :: NestedList a -> [a]
p7 (Elem a) = [a] 
p7 (List (x:xs)) = p7 x ++ p7 (List xs)
p7 (List []) = []

p8 :: Eq a => [a] -> [a]
p8 l 
    | length l == 1 = l
    | head l == l !! 1 = p8 (drop 1 l)
    | head l /= l !! 1 = take 1 l ++ p8 (tail l)

p9 :: Eq a => [a] -> [[a]]
p9 = group

p10 :: Eq a => [a] -> [(Int, a)]
p10 l = map (\x -> (length x, head x)) (group l)
