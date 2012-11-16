module Pattern where
import Utilities


-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------


-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute w t s =concatMap (replace w s) t
	where replace wild val sub 
		| sub == wild = val
		| otherwise = [sub]


-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match w [] [] = Just []
match w [] (s:ss) = Nothing
match w (t:ts) []
	| w == t = match w ts []
	| otherwise = Nothing
match w (t:ts) (s:ss)
	| t == s = match w ts ss
	| t == w = orElse (singleWildcardMatch (t:ts) (s:ss)) ( longerWildcardMatch (t:ts) (s:ss))
	| otherwise = Nothing

-- Helper function to match
singleWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:ps) (x:xs)
	| match wc ps xs /= Nothing = Just [x]
	| otherwise = Nothing 

longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
longerWildcardMatch (wc:ps) (x:xs) = mmap ((:) x) (match wc (wc:ps) xs)

-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions



-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply w f ptrn (p1,p2) =
		    mmap (substitute w p2) (mmap f (match w p1 ptrn))


-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply w f [] ptrn = Nothing
transformationsApply w f (x:xs) ptrn = orElse (transformationApply w f ptrn x) (transformationsApply w f xs ptrn) 

