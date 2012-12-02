If we had acces to an algorithm for the string alignment problem we 
could solve the maximal common subsequence problem by: 
* Incur a large penalty for the case of mismatch or space. 
* Give a large positive score for the case of matches. 
This will create solutions where the common elements of the two lists
are matched together. To obtain the maximal common subsequence the 
lists can be checked against each other, and the maximal common 
subsequence represents the entries that match. 

========================================================================
Main funtions. I've kept the old, and inefficient, ones.


> type AlignmentType = (String,String)

> optAlignments_ineff :: String -> String -> [AlignmentType]
> optAlignments_ineff string1 string2 = 
>               maximaBy scoreAlignments $ genAllComb string1 string2


> similarityScore_ineff :: String -> String -> Int
> similarityScore_ineff string1 string2 =
>             maximum (map scoreAlignments $ genAllComb string1 string2)


> similarityScore :: String -> String -> Int
> similarityScore xs ys = getOpts (length xs) (length ys)
>     where
>        getOpts i j = optsTable!!i!!j
>        optsTable = [[ optEntry i j | j<-[0..]] | i<-[0..] ]
>        optEntry :: Int -> Int -> Int
>        optEntry 0 0 = 0
>        optEntry i 0 = scoreSpace + optsTable!!(i-1)!!0
>        optEntry 0 j = scoreSpace + optsTable!!0!!(j-1)
>        optEntry i j
>          | x == y    = scoreMatch + optsTable!!(i-1)!!(j-1)
>          | otherwise = maximum [ld,d,ud]
>          where
>             ld = scoreSpace    +  (optsTable!!i!!(j-1)) 
>             d  = scoreMismatch +  (optsTable!!(i-1)!!(j-1))
>             ud = scoreSpace    +  (optsTable!!(i-1)!!j)
>             x = (xs!!(i-1))
>             y = (ys!!(j-1))


> optAlignments :: String -> String -> (Int,[AlignmentType])
> optAlignments xs ys = getOpts (length xs) (length ys)
>     where
>        getOpts i j = optsTable!!i!!j
>        optsTable = [[ optEntry i j | j<-[0..]] | i<-[0..] ]
>        optEntry :: Int -> Int -> (Int, [AlignmentType])
>        optEntry 0 0 = (0,[([],[])])
>        optEntry i 0 = (new_score,new_p)
>           where 
>               new_score = scoreSpace + fst (optsTable!!(i-1)!!0)
>               new_p = attachTails (xs!!(i-1)) '-' (snd (optsTable!!(i-1)!!0)) 
>        optEntry 0 j = (new_score,new_p)
>           where 
>               new_score = scoreSpace + fst (optsTable!!0!!(j-1))
>               new_p = attachTails '-' (ys!!(j-1)) (snd (optsTable!!0!!(j-1))) 
>        optEntry i j
>          | x == y    = (scoreMatch + (fst prev_d),  attachTails x y  (snd prev_d))
>          | otherwise = (maximum (map (fst) [ld,d,ud] ), concat $ map (snd) (maximaBy (fst) [ld,d,ud]))
>          where
>             ld = (scoreSpace    + (fst prev_ld), attachTails '-' y  (snd prev_ld))
>             d  = (scoreMismatch + (fst prev_d ), attachTails x   y  (snd prev_d))
>             ud = (scoreSpace    + (fst prev_ud), attachTails x '-'  (snd prev_ud))
>             x = (xs!!(i-1))
>             y = (ys!!(j-1))
>             prev_ld = optsTable!!i!!(j-1)
>             prev_d  = optsTable!!(i-1)!!(j-1)
>             prev_ud = optsTable!!(i-1)!!j


> outputOptAlignments :: String -> String -> IO ()
> outputOptAlignments st1 st2= 
>     putStrLn $ "\nThere are "++(show (length opts)) 
>     ++" optimal alignments:\n"++ (concat opts)
>       where   opts =  ["\n"++op1++"\n"++op2++"\n"|(op1,op2) <- optAls]
>               optAls = snd $ optAlignments st1 st2


========================================================================
Auxillary funtions.

> -- Takes a list of tuples, where the elements of the tuples are lists
> -- aswell, and inserts h1 at the start of list 1 of the tuple and vice
> -- versa for h2. 
> -- E.g. attachHeads 'H' 'k' [("as","ell")] = [("Has","kell")]
> attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])] 
> attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

> attachTails :: a -> a -> [([a],[a])] -> [([a],[a])] 
> attachTails h1 h2 aList = [(xs++[h1],ys++[h2]) | (xs,ys) <- aList]


> maximaBy :: Ord b => (a -> b) -> [a] -> [a] 
> maximaBy valueFcn xs = [x | x <- xs,(valueFcn x) == max_val]
>           where max_val = maximum $ map valueFcn xs


> genAllComb :: String -> String -> [(String,String)]
> genAllComb [] [] = [([],[])]
> genAllComb (x:xs) [] = attachHeads x '-' $ genAllComb xs []
> genAllComb [] (y:ys) = attachHeads '-' y $ genAllComb [] ys
> genAllComb (x:xs) (y:ys) = concat [c1,c2,c3]
>               where
>                   c1 = attachHeads x   y $ genAllComb xs ys
>                   c2 = attachHeads '-' y $ genAllComb (x:xs) ys
>                   c3 = attachHeads x '-' $ genAllComb xs (y:ys)


> scoreMatch    = 1
> scoreMismatch = -1
> scoreSpace    = -1

> scoreAlignments :: (String,String) -> Int
> scoreAlignments ([],_) = 0
> scoreAlignments (_,[]) = 0
> scoreAlignments ((x:xs),(y:ys))
>      | x == y = (scoreMatch+) $ scoreAlignments (xs,ys)
>      | x == '-' || y == '-' = (scoreSpace+) $ scoreAlignments (xs,ys)
>      | x /= y = (scoreMismatch+) $ scoreAlignments (xs,ys)

