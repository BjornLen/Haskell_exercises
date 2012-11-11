f 0 =   1
f x = x * f (x -1)

f2 [] = 1
f2 (x:xs) = x* f2 xs

somethin =  do x <- [1,2,3]
               y <- [1,2,3]
               True <- return (x /= y)
               return (x,y)

