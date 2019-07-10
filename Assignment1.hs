module Assignment1 (subst, interleave, unroll) where

-- function 1
subst :: Eq t => t -> t -> [t] -> [t]
subst a b [] = []
subst a b (x:xs) = if x == a then (b:(subst a b xs)) else (x:(subst a b xs))

-- function 2
interleave :: [t] -> [t] -> [t]
interleave [] [] = []
interleave a [] = a
interleave [] a = a
interleave (a:as) (b:bs) = (a:(b:interleave as bs))

-- function 3
unroll :: Int -> [a] -> [a]
unroll 0 a = []
unroll n [] = []
unroll n (a:as)
 | n < 0 = error "number should not be negative"
 | n == length (a:as) = (a:as)
 | n > length (a:as) = (a:as) ++ unroll (n - length (a:as)) (a:as)
 | n < length (a:as) = [a] ++ unroll (n-1) as