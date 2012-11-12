-- Takes a function and a Maybe value as input. If the value if nothing then
-- return nothing, otherwhise return the function applied to the Just value
p :: (a -> b) -> Maybe a -> Maybe b
mmap f  Nothing  = Nothing
mmap f (Just x)  = Just (f x)

-- Takes two maybe values as input, if the first value is nothing then return 
-- the other value otherwise return the Just value of the first input
orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing  x  = x
orElse (Just a) _  = Just a
    
-- Try to apply f to the x, if the resulting value is nothing just return x else
-- return f applied to x
try :: (a -> Maybe a) -> a -> a
try f x = maybe x id (f x)

-- meh
fix :: Eq a => (a -> a) -> a -> a
fix f x
   |  f x == x  = x
   |  otherwise = fix f (f x)

-- A obfuscated way to use a random number in the range [0, 1[ to pick a random
-- element in a list
pick :: RealFrac r => r -> [a] -> a
pick u xs = xs !! (floor.(u*).fromIntegral.length) xs
