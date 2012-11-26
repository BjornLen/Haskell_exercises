
> similarityScore :: String -> String -> Int
> similarityScore string1 string2 = 1

> -- Takes a list of tuples, where the elements of the tuples are lists
> -- aswell, and inserts h1 at the start of list 1 of the tuple and vice
> -- versa for h2. 
> -- E.g. attachHeads 'H' 'k' [("as","ell")] = [("Has","kell")]
> attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])] 
> attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

> maximaBy :: Ord b => (a -> b) -> [a] -> [a] 
> maximaBy valueFcn xs = [x | x <- xs,(valueFcn x) == (maximum xs_comp)]
>           where xs_comp = map valueFcn xs

> type AlignmentType = (String,String)
> optAlignments :: String -> String -> [AlignmentType]
> optAlignments string1 string2 = [("Im an","alignmenttype")]

> scoreMatch = 0
> scoreMismatch = -1
> scoreSpace = -1

> similarityScore :: String -> String -> Int
> similarityScore string1 string2

> outputAlignements :: String -> String -> [String]
> outputOptAlignments string1 string2
